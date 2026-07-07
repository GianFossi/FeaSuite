namespace FeaSuite.Storage

open System
open System.IO
open FeaSuite.Core

// ---------------------------------------------------------------------------
// SessionWorkspace – manages temporary paging files and analysis options.
//
// Creates a unique subdirectory under SolverOptions.TempDirectory for every
// workspace instance.  All PagedMatrixStore / PagedVector temp files are
// written there and cleaned up on Dispose.
//
// Usage:
//   use ws = new SessionWorkspace(opts)
//   use kStore = ws.CreateTempMatrixStore(totalDofs, totalDofs)
//   use fVec   = ws.CreateTempVector(totalDofs)
//   ...
//   ws.SaveOptions "run1.opts.json"
//   ws.LoadOptions "run1.opts.json" |> ...
//   ws.SaveModel   "run1.model.json" model |> ...
//   ws.LoadModel   "run1.model.json"  |> ...
// ---------------------------------------------------------------------------

/// Manages a scratch directory for paging files plus all analysis options.
type SessionWorkspace(opts: SolverOptions) =

    let mutable isDisposed = false

    // Each workspace gets its own subdirectory to avoid file-name collisions
    // between concurrent runs.
    let sessionDir =
        let dir = Path.Combine(opts.TempDirectory, "feasuite_" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(dir) |> ignore
        dir

    let mutable tempFiles : string list = []

    let checkNotDisposed () =
        if isDisposed then raise (ObjectDisposedException "SessionWorkspace")

    let registerTemp path =
        tempFiles <- path :: tempFiles
        path

    // -----------------------------------------------------------------------
    // Properties
    // -----------------------------------------------------------------------

    /// The options that were used to create this workspace.
    member _.Options = opts

    /// The scratch directory where all temporary paging files are written.
    member _.SessionDirectory = sessionDir

    // -----------------------------------------------------------------------
    // Temporary paging file factories
    // -----------------------------------------------------------------------

    /// Create a temporary file-backed paged matrix store (COO, file-backed).
    member _.CreateTempMatrixStore(rows: int, cols: int) : PagedMatrixStore =
        checkNotDisposed ()
        let path = registerTemp (Path.Combine(sessionDir, Path.GetRandomFileName() + ".kmat"))
        new PagedMatrixStore(path, rows, cols, opts.PageBufferSize)

    /// Create a temporary file-backed paged float vector.
    member _.CreateTempVector(length: int) : PagedVector =
        checkNotDisposed ()
        let path = registerTemp (Path.Combine(sessionDir, Path.GetRandomFileName() + ".vec"))
        PagedVector.Create(path, length, opts.VectorPageSize)

    // -----------------------------------------------------------------------
    // Options I/O commands
    // -----------------------------------------------------------------------

    /// Save the current workspace options to <paramref name="path"/> as JSON.
    member _.SaveOptions(path: string) : Validation<unit> =
        checkNotDisposed ()
        SolverOptions.save path opts

    /// Load options from a JSON file at <paramref name="path"/>.
    /// Returns the loaded options without modifying this workspace.
    member _.LoadOptions(path: string) : Validation<SolverOptions> =
        checkNotDisposed ()
        SolverOptions.load path

    /// Apply a JSON merge-patch to an existing options file.
    /// Creates the file from defaults if it does not exist.
    member _.PatchOptions(path: string) (patchJson: string) : Validation<SolverOptions> =
        checkNotDisposed ()
        SolverOptions.patch path patchJson

    // -----------------------------------------------------------------------
    // Model I/O commands
    // -----------------------------------------------------------------------

    /// Serialise <paramref name="model"/> to a JSON file at <paramref name="path"/>.
    member _.SaveModel(path: string) (model: FEAModel) : Validation<unit> =
        checkNotDisposed ()
        ModelSerializer.saveModel path model

    /// Deserialise an FEAModel from a JSON file at <paramref name="path"/>.
    member _.LoadModel(path: string) : Validation<FEAModel> =
        checkNotDisposed ()
        ModelSerializer.loadModel path

    // -----------------------------------------------------------------------
    // Cleanup
    // -----------------------------------------------------------------------

    /// Delete all temporary files and the session directory.
    member _.CleanupTempFiles() =
        for f in tempFiles do
            try if File.Exists f then File.Delete f
            with _ -> ()
        try
            if Directory.Exists sessionDir then
                Directory.Delete(sessionDir, recursive = true)
        with _ -> ()
        tempFiles <- []

    interface IDisposable with
        member self.Dispose() =
            if not isDisposed then
                isDisposed <- true
                self.CleanupTempFiles()

// ---------------------------------------------------------------------------
// SessionWorkspace module – factory helpers
// ---------------------------------------------------------------------------

module SessionWorkspace =

    /// Create a new SessionWorkspace with default options.
    let createDefault () =
        new SessionWorkspace(SolverOptions.defaults)

    /// Create a new SessionWorkspace loading options from a JSON file.
    /// Returns Error if the file cannot be read.
    let fromOptionsFile (path: string) : Validation<SessionWorkspace> =
        match SolverOptions.load path with
        | Ok opts -> Ok (new SessionWorkspace(opts))
        | Error e -> Error e
