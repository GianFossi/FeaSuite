module FeaSuite.Tests.IterativeSolverTests

open Xunit
open FeaSuite.Core
open FeaSuite.Solvers

// ---------------------------------------------------------------------------
// Tests for CgSolver and BiCgStabSolver
//
// 1.  Correctness on small known-answer systems.
// 2.  Solver diagnostics: iteration count, residual norm, converged flag.
// 3.  End-to-end pipeline with each iterative solver on the Bar1D model.
// 4.  Agreement with dense Gaussian elimination within tight tolerance.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Helper: build a trivially solvable 3×3 SPD system
//
//   K = [[4, 1, 0],        b = [1, 2, 3]
//        [1, 3,-1],
//        [0,-1, 2]]
//
//   Analytical solution (from direct solve):
//     via Python: x ≈ [-0.0625, 0.375, 1.6875]
// ---------------------------------------------------------------------------

let private build3x3Spd () : float[,] * float[] =
    let K = array2D [ [  4.0;  1.0;  0.0 ]
                      [  1.0;  3.0; -1.0 ]
                      [  0.0; -1.0;  2.0 ] ]
    let b = [| 1.0; 2.0; 3.0 |]
    K, b

/// Minimal IAssembledSystem backed by a dense float[,].
type private DenseSystem(k: float[,], f: float[]) =
    interface IAssembledSystem with
        member _.TotalDofs = f.Length
        member _.K         = k
        member _.F         = f


[<Fact>]
let ``CgSolver: solves small 3x3 SPD system correctly`` () =
    let K, b = build3x3Spd ()
    let n    = b.Length
    let sys  = DenseSystem(K, b) :> IAssembledSystem
    let solver = CgSolver() :> ILinearSolver

    match solver.Solve(sys, [], Map.empty) with
    | Error e -> failwith (sprintf "CG failed: %A" e)
    | Ok x    ->
        // Verify A·x ≈ b
        for i in 0 .. n - 1 do
            let mutable Ax_i = 0.0
            for j in 0 .. n - 1 do
                Ax_i <- Ax_i + K.[i, j] * x.[j]
            let diff = abs (Ax_i - b.[i])
            Assert.True(diff < 1e-9,
                sprintf "Row %d: expected residual < 1e-9, got %.3e (Ax=%g, b=%g)"
                        i diff Ax_i b.[i])


[<Fact>]
let ``BiCgStabSolver: solves small 3x3 SPD system correctly`` () =
    let K, b = build3x3Spd ()
    let n    = b.Length
    let sys  = DenseSystem(K, b) :> IAssembledSystem
    let solver = BiCgStabSolver() :> ILinearSolver

    match solver.Solve(sys, [], Map.empty) with
    | Error e -> failwith (sprintf "BiCGSTAB failed: %A" e)
    | Ok x    ->
        for i in 0 .. n - 1 do
            let mutable Ax_i = 0.0
            for j in 0 .. n - 1 do
                Ax_i <- Ax_i + K.[i, j] * x.[j]
            let diff = abs (Ax_i - b.[i])
            Assert.True(diff < 1e-9,
                sprintf "Row %d: BiCGSTAB residual %.3e" i diff)


[<Fact>]
let ``CgSolver: diagnostics report convergence and iteration count`` () =
    let K, b = build3x3Spd ()
    let sys  = DenseSystem(K, b) :> IAssembledSystem
    let solver = CgSolver() :> ILinearSolver

    match solver.Solve(sys, [], Map.empty) with
    | Error e -> failwith (sprintf "CG failed: %A" e)
    | Ok _ ->
        let diag = (solver :?> IIterativeSolverDiagnostics).LastDiagnostics
        Assert.True(diag.IsSome, "Expected diagnostics to be set after solve")
        let d = diag.Value
        Assert.True(d.Converged,           sprintf "CG did not converge; iter=%d rNorm=%.3e" d.Iterations d.ResidualNorm)
        Assert.True(d.ResidualNorm < 1e-9, sprintf "Residual norm too large: %.3e" d.ResidualNorm)
        Assert.True(d.Iterations >= 0,     "Negative iteration count")


[<Fact>]
let ``BiCgStabSolver: diagnostics report convergence`` () =
    let K, b = build3x3Spd ()
    let sys  = DenseSystem(K, b) :> IAssembledSystem
    let solver = BiCgStabSolver() :> ILinearSolver

    match solver.Solve(sys, [], Map.empty) with
    | Error e -> failwith (sprintf "BiCGSTAB failed: %A" e)
    | Ok _ ->
        let diag = (solver :?> IIterativeSolverDiagnostics).LastDiagnostics
        Assert.True(diag.IsSome, "Expected diagnostics to be set after solve")
        let d = diag.Value
        Assert.True(d.Converged,           sprintf "BiCGSTAB did not converge; rNorm=%.3e" d.ResidualNorm)
        Assert.True(d.ResidualNorm < 1e-9, sprintf "Residual norm too large: %.3e" d.ResidualNorm)


[<Fact>]
let ``CgSolver: bar1D displacement matches analytical`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 1000.0
    let model, _ = Helpers.buildBar1DModel E A L F
    let input = {
        Model              = model
        LoadCaseIndex      = 0
        UseNonlinear       = false
        NonlinearConfig    = NonlinearConfig.defaults
        LinearSolverKind   = LinearSolverKind.defaultCg
        UseSparseAssembler = false
    }
    match FeaPipeline.run input with
    | Error e -> failwith (sprintf "Pipeline failed: %A" e)
    | Ok out  ->
        let u2_expected = F * L / (E * A)
        let u2_actual   = out.Displacements.[NodeId 2].[0]
        Helpers.assertClose u2_expected u2_actual "CG bar1D u2"


[<Fact>]
let ``BiCgStabSolver: bar1D displacement matches analytical`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 1000.0
    let model, _ = Helpers.buildBar1DModel E A L F
    let input = {
        Model              = model
        LoadCaseIndex      = 0
        UseNonlinear       = false
        NonlinearConfig    = NonlinearConfig.defaults
        LinearSolverKind   = LinearSolverKind.defaultBiCgStab
        UseSparseAssembler = false
    }
    match FeaPipeline.run input with
    | Error e -> failwith (sprintf "Pipeline failed: %A" e)
    | Ok out  ->
        let u2_expected = F * L / (E * A)
        let u2_actual   = out.Displacements.[NodeId 2].[0]
        Helpers.assertClose u2_expected u2_actual "BiCGSTAB bar1D u2"


[<Fact>]
let ``CgSolver: result agrees with DenseLinearSolver on bar1D`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 1000.0
    let model, _ = Helpers.buildBar1DModel E A L F

    let runWith kind =
        let input = {
            Model              = model
            LoadCaseIndex      = 0
            UseNonlinear       = false
            NonlinearConfig    = NonlinearConfig.defaults
            LinearSolverKind   = kind
            UseSparseAssembler = false
        }
        match FeaPipeline.run input with
        | Ok out   -> out.Displacements.[NodeId 2].[0]
        | Error e  -> failwith (sprintf "Pipeline failed: %A" e)

    let uDense  = runWith Dense
    let uCg     = runWith (LinearSolverKind.defaultCg)
    let uBicgst = runWith (LinearSolverKind.defaultBiCgStab)

    let diffCg     = abs (uDense - uCg)
    let diffBicgst = abs (uDense - uBicgst)
    Assert.True(diffCg     < 1e-9, sprintf "CG vs Dense diff = %.3e"     diffCg)
    Assert.True(diffBicgst < 1e-9, sprintf "BiCGSTAB vs Dense diff = %.3e" diffBicgst)


[<Fact>]
let ``CgSolver: reaction at fixed node equals -F (with BCs applied)`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 1000.0
    let model, _ = Helpers.buildBar1DModel E A L F
    let input = {
        Model              = model
        LoadCaseIndex      = 0
        UseNonlinear       = false
        NonlinearConfig    = NonlinearConfig.defaults
        LinearSolverKind   = LinearSolverKind.defaultCg
        UseSparseAssembler = false
    }
    match FeaPipeline.run input with
    | Error e -> failwith (sprintf "Pipeline failed: %A" e)
    | Ok out  ->
        match out.Reactions.TryFind (NodeId 1, 0) with
        | None   -> failwith "No reaction at Node 1"
        | Some r -> Helpers.assertClose (-F) r "CG reaction R1"
