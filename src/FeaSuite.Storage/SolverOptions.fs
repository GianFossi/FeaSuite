namespace FeaSuite.Storage

open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open FeaSuite.Core

// ---------------------------------------------------------------------------
// Matrix storage format and solver backend choices
// ---------------------------------------------------------------------------

/// Matrix storage format used by the assembler/solver pipeline.
type MatrixStorage =
    | Dense      /// Full dense float[,] – best for small systems (< ~5 000 DOFs)
    | Skyline    /// Profile/skyline symmetric sparse – efficient for banded SPD systems
    | SparseCsr  /// File-backed COO assembly finalised to CSR (PagedMatrixStore backend)

/// Numerical back-end for solving the assembled linear system.
type SolverBackend =
    | BuiltIn        /// Built-in Gaussian elimination (no external packages)
    | MathNet        /// MathNet.Numerics optimised routines (LU / iterative)
    | SparseCg       /// Pure F# Conjugate Gradient with Jacobi preconditioner (SPD systems)
    | SparseBiCgStab /// Pure F# BiCGSTAB with Jacobi preconditioner (general systems)

// ---------------------------------------------------------------------------
// SolverOptions – all configuration that drives a session
// ---------------------------------------------------------------------------

/// All configurable options for a FeaSuite analysis session.
type SolverOptions = {
    MatrixStorage   : MatrixStorage
    SolverBackend   : SolverBackend
    /// Root directory for temporary paging files (defaults to system temp).
    TempDirectory   : string
    /// Write-buffer size (number of triplets) for PagedMatrixStore.
    PageBufferSize  : int
    /// Page size (number of float64 values per page) for PagedVector.
    VectorPageSize  : int
    /// Maximum Newton-Raphson iterations for non-linear analysis.
    MaxNRIterations : int
    /// Residual-norm tolerance for Newton-Raphson convergence.
    NRTolerance     : float
    /// Number of incremental load steps.
    LoadIncrements  : int
}

// ---------------------------------------------------------------------------
// SolverOptions module – defaults, JSON save / load / patch
// ---------------------------------------------------------------------------

module SolverOptions =

    let private jsonOpts = JsonSerializerOptions(WriteIndented = true)

    let private matrixStorageToString = function
        | Dense     -> "Dense"
        | Skyline   -> "Skyline"
        | SparseCsr -> "SparseCsr"
    let private matrixStorageOfString = function
        | "Skyline"   -> Skyline
        | "SparseCsr" -> SparseCsr
        | _           -> Dense

    let private backendToString = function
        | BuiltIn        -> "BuiltIn"
        | MathNet        -> "MathNet"
        | SparseCg       -> "SparseCg"
        | SparseBiCgStab -> "SparseBiCgStab"
    let private backendOfString = function
        | "MathNet"        -> MathNet
        | "SparseCg"       -> SparseCg
        | "SparseBiCgStab" -> SparseBiCgStab
        | _                -> BuiltIn

    // Build a JsonObject from an SolverOptions value.
    let private toJsonObject (opts: SolverOptions) =
        let o = JsonObject()
        o["MatrixStorage"]   <- JsonValue.Create(matrixStorageToString opts.MatrixStorage)
        o["SolverBackend"]   <- JsonValue.Create(backendToString opts.SolverBackend)
        o["TempDirectory"]   <- JsonValue.Create(opts.TempDirectory)
        o["PageBufferSize"]  <- JsonValue.Create(opts.PageBufferSize)
        o["VectorPageSize"]  <- JsonValue.Create(opts.VectorPageSize)
        o["MaxNRIterations"] <- JsonValue.Create(opts.MaxNRIterations)
        o["NRTolerance"]     <- JsonValue.Create(opts.NRTolerance)
        o["LoadIncrements"]  <- JsonValue.Create(opts.LoadIncrements)
        o

    // Parse a JsonObject back to SolverOptions.  Falls back to defaults for
    // missing or malformed fields.
    let private ofJsonObject (o: JsonObject) (fallback: SolverOptions) =
        let str  (key: string) def = if isNull o.[key] then def else o.[key].GetValue<string>()
        let int_ (key: string) def = if isNull o.[key] then def else o.[key].GetValue<int>()
        let flt  (key: string) def = if isNull o.[key] then def else o.[key].GetValue<float>()
        { MatrixStorage   = matrixStorageOfString (str  "MatrixStorage"   (matrixStorageToString fallback.MatrixStorage))
          SolverBackend   = backendOfString       (str  "SolverBackend"   (backendToString       fallback.SolverBackend))
          TempDirectory   = str  "TempDirectory"   fallback.TempDirectory
          PageBufferSize  = int_ "PageBufferSize"  fallback.PageBufferSize
          VectorPageSize  = int_ "VectorPageSize"  fallback.VectorPageSize
          MaxNRIterations = int_ "MaxNRIterations" fallback.MaxNRIterations
          NRTolerance     = flt  "NRTolerance"     fallback.NRTolerance
          LoadIncrements  = int_ "LoadIncrements"  fallback.LoadIncrements }

    /// Factory-default options (Dense matrix, BuiltIn solver, system temp dir).
    let defaults : SolverOptions = {
        MatrixStorage   = Dense
        SolverBackend   = BuiltIn
        TempDirectory   = Path.GetTempPath()
        PageBufferSize  = 256
        VectorPageSize  = 1024
        MaxNRIterations = 50
        NRTolerance     = 1e-6
        LoadIncrements  = 10
    }

    /// Serialise options to a JSON file at <paramref name="path"/>.
    let save (path: string) (opts: SolverOptions) : Validation<unit> =
        try
            let json = (toJsonObject opts).ToJsonString(jsonOpts)
            File.WriteAllText(path, json)
            Ok ()
        with ex ->
            Error [ StorageError ex.Message ]

    /// Deserialise options from a JSON file at <paramref name="path"/>.
    let load (path: string) : Validation<SolverOptions> =
        try
            if not (File.Exists path) then
                Error [ StorageError (sprintf "Options file not found: %s" path) ]
            else
                let json = File.ReadAllText path
                let obj  = JsonNode.Parse(json) :?> JsonObject
                Ok (ofJsonObject obj defaults)
        with ex ->
            Error [ StorageError ex.Message ]

    /// Apply a JSON merge-patch string to an existing options file and save the
    /// result back to the same file.
    ///
    /// The patch is a JSON object whose keys override the matching keys in the
    /// existing file (RFC 7396 shallow merge).  If the file does not exist it
    /// is created from defaults before applying the patch.
    ///
    /// Example: patch path """{"MatrixStorage":"Skyline","NRTolerance":1e-8}"""
    let patch (path: string) (patchJson: string) : Validation<SolverOptions> =
        try
            let baseJson =
                if File.Exists path then File.ReadAllText path
                else (toJsonObject defaults).ToJsonString(jsonOpts)

            let baseObj  = JsonNode.Parse(baseJson)  :?> JsonObject
            let patchObj = JsonNode.Parse(patchJson) :?> JsonObject

            // Shallow merge: patch entries overwrite matching base entries
            for kvp in patchObj do
                let v = kvp.Value
                baseObj.[kvp.Key] <-
                    if isNull v then null
                    else v.DeepClone()

            let merged = baseObj.ToJsonString(jsonOpts)
            File.WriteAllText(path, merged)
            Ok (ofJsonObject baseObj defaults)
        with ex ->
            Error [ StorageError ex.Message ]
