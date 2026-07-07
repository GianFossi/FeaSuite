module FeaSuite.Tests.SessionWorkspaceTests

open System.IO
open Xunit
open FeaSuite.Core
open FeaSuite.Storage
open FeaSuite.Post
open FeaSuite.Solvers

// ---------------------------------------------------------------------------
// SessionWorkspace, SolverOptions, ModelSerializer and ResultSerializer tests
// ---------------------------------------------------------------------------

let private tmpFile ext =
    Path.Combine(Path.GetTempPath(), Path.GetRandomFileName() + "." + ext)

// -----------------------------------------------------------------------
// SolverOptions – save / load / patch
// -----------------------------------------------------------------------

[<Fact>]
let ``SolverOptions.defaults: expected values`` () =
    let opts = SolverOptions.defaults
    Assert.Equal(Dense,   opts.MatrixStorage)
    Assert.Equal(BuiltIn, opts.SolverBackend)
    Assert.Equal(256,     opts.PageBufferSize)
    Assert.Equal(1024,    opts.VectorPageSize)
    Assert.Equal(50,      opts.MaxNRIterations)
    Assert.Equal(1e-6,    opts.NRTolerance)
    Assert.Equal(10,      opts.LoadIncrements)

[<Fact>]
let ``SolverOptions.save and load: round-trips defaults`` () =
    let path = tmpFile "opts.json"
    try
        let opts = SolverOptions.defaults
        match SolverOptions.save path opts with
        | Error e -> failwith (sprintf "Save failed: %A" e)
        | Ok () ->
            Assert.True(File.Exists path)
            match SolverOptions.load path with
            | Error e -> failwith (sprintf "Load failed: %A" e)
            | Ok loaded ->
                Assert.Equal(opts.MatrixStorage,  loaded.MatrixStorage)
                Assert.Equal(opts.SolverBackend,  loaded.SolverBackend)
                Assert.Equal(opts.PageBufferSize, loaded.PageBufferSize)
                Assert.Equal(opts.NRTolerance,    loaded.NRTolerance)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``SolverOptions.save and load: Skyline + MathNet backend`` () =
    let path = tmpFile "opts.json"
    try
        let opts = { SolverOptions.defaults with MatrixStorage = Skyline; SolverBackend = MathNet }
        SolverOptions.save path opts |> ignore
        match SolverOptions.load path with
        | Error e -> failwith (sprintf "Load failed: %A" e)
        | Ok loaded ->
            Assert.Equal(Skyline, loaded.MatrixStorage)
            Assert.Equal(MathNet, loaded.SolverBackend)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``SolverOptions.load: returns error for missing file`` () =
    match SolverOptions.load "/nonexistent/path/opts.json" with
    | Error [ StorageError _ ] -> ()
    | other -> failwith (sprintf "Expected StorageError, got %A" other)

[<Fact>]
let ``SolverOptions.patch: creates file from defaults when absent`` () =
    let path = tmpFile "opts.json"
    try
        if File.Exists path then File.Delete path
        match SolverOptions.patch path """{"MatrixStorage":"Skyline"}""" with
        | Error e -> failwith (sprintf "Patch failed: %A" e)
        | Ok patched ->
            Assert.Equal(Skyline, patched.MatrixStorage)
            Assert.Equal(BuiltIn, patched.SolverBackend)  // default preserved
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``SolverOptions.patch: applies partial update to existing file`` () =
    let path = tmpFile "opts.json"
    try
        let original = { SolverOptions.defaults with MaxNRIterations = 20; NRTolerance = 1e-4 }
        SolverOptions.save path original |> ignore

        match SolverOptions.patch path """{"NRTolerance":1e-8,"MatrixStorage":"Skyline"}""" with
        | Error e -> failwith (sprintf "Patch failed: %A" e)
        | Ok patched ->
            Assert.Equal(Skyline, patched.MatrixStorage)
            Assert.Equal(1e-8,    patched.NRTolerance)
            Assert.Equal(20,      patched.MaxNRIterations)  // unchanged field preserved
    finally
        if File.Exists path then File.Delete path

// -----------------------------------------------------------------------
// ModelSerializer – save / load FEAModel
// -----------------------------------------------------------------------

[<Fact>]
let ``ModelSerializer: round-trips a Bar1D model`` () =
    let path = tmpFile "model.json"
    try
        let model, _ = Helpers.buildBar1DModel 2e11 1e-4 1.0 1000.0
        match ModelSerializer.saveModel path model with
        | Error e -> failwith (sprintf "Save failed: %A" e)
        | Ok () ->
            match ModelSerializer.loadModel path with
            | Error e -> failwith (sprintf "Load failed: %A" e)
            | Ok loaded ->
                Assert.Equal(model.Nodes.Count,     loaded.Nodes.Count)
                Assert.Equal(model.Elements.Count,  loaded.Elements.Count)
                Assert.Equal(model.Materials.Count, loaded.Materials.Count)
                Assert.Equal(model.LoadCases.Length, loaded.LoadCases.Length)

                // Node positions preserved
                Assert.Equal(0.0, loaded.Nodes.[NodeId 1].Position.X)
                Assert.Equal(1.0, loaded.Nodes.[NodeId 2].Position.X)

                // Material properties preserved
                let mat = loaded.Materials.[MaterialId 1]
                Assert.Equal(2e11, mat.YoungModulus)
                Assert.True(mat.CrossSectionArea.IsSome)
                Assert.Equal(1e-4, mat.CrossSectionArea.Value)

                // Load case preserved
                let lc = loaded.LoadCases.[0]
                Assert.Equal(1, lc.Loads.Length)
                Assert.Equal(1, lc.BoundaryConditions.Length)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``ModelSerializer: load from non-existent file returns StorageError`` () =
    match ModelSerializer.loadModel "/nonexistent/path/model.json" with
    | Error [ StorageError _ ] -> ()
    | other -> failwith (sprintf "Expected StorageError, got %A" other)

[<Fact>]
let ``ModelSerializer: loaded model solves to same result as original`` () =
    let path = tmpFile "model.json"
    try
        let original, _ = Helpers.buildBar1DModel 2e11 1e-4 1.0 1000.0
        ModelSerializer.saveModel path original |> ignore

        match ModelSerializer.loadModel path with
        | Error e -> failwith (sprintf "LoadModel failed: %A" e)
        | Ok loaded ->
            let mkInput m = {
                Model           = m
                LoadCaseIndex   = 0
                UseNonlinear    = false
                NonlinearConfig = NonlinearConfig.defaults
            }
            match FeaPipeline.run (mkInput original),
                  FeaPipeline.run (mkInput loaded) with
            | Ok outOrig, Ok outLoaded ->
                let uOrig   = outOrig.Displacements.[NodeId 2].[0]
                let uLoaded = outLoaded.Displacements.[NodeId 2].[0]
                let diff = abs (uOrig - uLoaded)
                Assert.True(diff < 1e-10, sprintf "u2 mismatch: orig=%.10g loaded=%.10g" uOrig uLoaded)
            | Error e, _ -> failwith (sprintf "Original pipeline failed: %A" e)
            | _, Error e -> failwith (sprintf "Loaded pipeline failed: %A" e)
    finally
        if File.Exists path then File.Delete path

// -----------------------------------------------------------------------
// SessionWorkspace – temp files, options commands, model I/O
// -----------------------------------------------------------------------

[<Fact>]
let ``SessionWorkspace: creates session directory on construction`` () =
    use ws = new SessionWorkspace(SolverOptions.defaults)
    Assert.True(Directory.Exists ws.SessionDirectory)

[<Fact>]
let ``SessionWorkspace: session directory is cleaned up on Dispose`` () =
    let dir =
        use ws = new SessionWorkspace(SolverOptions.defaults)
        ws.SessionDirectory
    Assert.False(Directory.Exists dir)

[<Fact>]
let ``SessionWorkspace.CreateTempMatrixStore: creates usable file-backed store`` () =
    use ws = new SessionWorkspace(SolverOptions.defaults)
    use store = ws.CreateTempMatrixStore(3, 3)
    store.Add(1, 1, 42.0)
    store.Flush()
    Assert.Equal(42.0, store.Get(1, 1))

[<Fact>]
let ``SessionWorkspace.CreateTempVector: creates usable file-backed vector`` () =
    use ws = new SessionWorkspace(SolverOptions.defaults)
    use vec = ws.CreateTempVector(10)
    vec.[5] <- 3.14
    vec.Flush()
    Assert.Equal(3.14, vec.[5])

[<Fact>]
let ``SessionWorkspace.SaveOptions and LoadOptions: round-trip workspace options`` () =
    let path = tmpFile "opts.json"
    try
        let skylineOpts = { SolverOptions.defaults with MatrixStorage = Skyline }
        use ws = new SessionWorkspace(skylineOpts)
        match ws.SaveOptions(path) with
        | Error e -> failwith (sprintf "SaveOptions failed: %A" e)
        | Ok () ->
            match ws.LoadOptions(path) with
            | Error e -> failwith (sprintf "LoadOptions failed: %A" e)
            | Ok loaded -> Assert.Equal(Skyline, loaded.MatrixStorage)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``SessionWorkspace.SaveModel and LoadModel: round-trip`` () =
    let path = tmpFile "model.json"
    try
        use ws = new SessionWorkspace(SolverOptions.defaults)
        let model, _ = Helpers.buildBar1DModel 2e11 1e-4 1.0 500.0
        match ws.SaveModel path model with
        | Error e -> failwith (sprintf "SaveModel failed: %A" e)
        | Ok () ->
            match ws.LoadModel path with
            | Error e -> failwith (sprintf "LoadModel failed: %A" e)
            | Ok loaded ->
                Assert.Equal(model.Nodes.Count,    loaded.Nodes.Count)
                Assert.Equal(model.Elements.Count, loaded.Elements.Count)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``SessionWorkspace.PatchOptions: updates options file`` () =
    let path = tmpFile "opts.json"
    try
        use ws = new SessionWorkspace(SolverOptions.defaults)
        ws.SaveOptions(path) |> ignore
        match ws.PatchOptions path """{"LoadIncrements":25}""" with
        | Error e -> failwith (sprintf "PatchOptions failed: %A" e)
        | Ok patched -> Assert.Equal(25, patched.LoadIncrements)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``SessionWorkspace.fromOptionsFile: creates workspace from saved options`` () =
    let optsPath = tmpFile "opts.json"
    try
        let opts = { SolverOptions.defaults with NRTolerance = 1e-9 }
        SolverOptions.save optsPath opts |> ignore
        match SessionWorkspace.fromOptionsFile optsPath with
        | Error e -> failwith (sprintf "fromOptionsFile failed: %A" e)
        | Ok ws ->
            use _ = ws
            Assert.Equal(1e-9, ws.Options.NRTolerance)
    finally
        if File.Exists optsPath then File.Delete optsPath

// -----------------------------------------------------------------------
// ResultSerializer – save / load FEAResults
// -----------------------------------------------------------------------

[<Fact>]
let ``ResultSerializer: round-trips FEAResults from a solved Bar1D model`` () =
    let path = tmpFile "results.json"
    try
        let model, lc = Helpers.buildBar1DModel 2e11 1e-4 1.0 1000.0
        let input = {
            Model           = model
            LoadCaseIndex   = 0
            UseNonlinear    = false
            NonlinearConfig = NonlinearConfig.defaults
        }
        match FeaPipeline.run input with
        | Error e -> failwith (sprintf "Pipeline failed: %A" e)
        | Ok out ->
            let assembler = DenseAssembler() :> IAssembler
            match assembler.Assemble(model, lc) with
            | Error e -> failwith (sprintf "Assemble failed: %A" e)
            | Ok system ->
                let results = ResultRecovery.postProcess out system model lc

                match ResultSerializer.saveResults path results with
                | Error e -> failwith (sprintf "SaveResults failed: %A" e)
                | Ok () ->
                    match ResultSerializer.loadResults path with
                    | Error e -> failwith (sprintf "LoadResults failed: %A" e)
                    | Ok loaded ->
                        Assert.Equal(results.Displacements.Values.Count,
                                     loaded.Displacements.Values.Count)
                        Assert.Equal(results.Reactions.Values.Count,
                                     loaded.Reactions.Values.Count)
                        Assert.Equal(results.ElementResults.Length,
                                     loaded.ElementResults.Length)
                        // Verify displacement value preserved
                        let u2Orig   = results.Displacements.Values.[NodeId 2].[0]
                        let u2Loaded = loaded.Displacements.Values.[NodeId 2].[0]
                        Assert.Equal(u2Orig, u2Loaded)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``ResultSerializer: load from non-existent file returns StorageError`` () =
    match ResultSerializer.loadResults "/nonexistent/path/results.json" with
    | Error [ StorageError _ ] -> ()
    | other -> failwith (sprintf "Expected StorageError, got %A" other)
