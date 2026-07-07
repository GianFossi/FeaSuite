module FeaSuite.Tests.LinearSolverTests

open Xunit
open FeaSuite.Core
open FeaSuite.Solvers

// ---------------------------------------------------------------------------
// Linear FEA solver tests (1-D bar, end-to-end pipeline)
//
//   Model: N1(x=0) --[EA]-- N2(x=L)
//   BC: u1 = 0 (fixed)
//   Load: F at N2
//   Analytical: u2 = F·L / (E·A),  R1 = -F
// ---------------------------------------------------------------------------

[<Fact>]
let ``bar1D: displacement equals F*L/(E*A)`` () =
    let E = 2e11   // Pa
    let A = 1e-4   // m²
    let L = 1.0    // m
    let F = 1000.0 // N

    let model, lc = Helpers.buildBar1DModel E A L F
    let input = {
        Model           = model
        LoadCaseIndex   = 0
        UseNonlinear    = false
        NonlinearConfig = NonlinearConfig.defaults
    }
    match FeaPipeline.run input with
    | Error e -> failwith (sprintf "Pipeline failed: %A" e)
    | Ok out  ->
        let u2_expected = F * L / (E * A)
        let u2_actual   = out.Displacements.[NodeId 2].[0]
        Helpers.assertClose u2_expected u2_actual "u2"

[<Fact>]
let ``bar1D: reaction at fixed node equals -F`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 1000.0

    let model, lc = Helpers.buildBar1DModel E A L F
    let input = {
        Model           = model
        LoadCaseIndex   = 0
        UseNonlinear    = false
        NonlinearConfig = NonlinearConfig.defaults
    }
    match FeaPipeline.run input with
    | Error e -> failwith (sprintf "Pipeline failed: %A" e)
    | Ok out  ->
        // Reaction at Node 1, DOF 0
        match out.Reactions.TryFind (NodeId 1, 0) with
        | None -> failwith "No reaction at Node 1"
        | Some r ->
            Helpers.assertClose (-F) r "R1"

[<Fact>]
let ``bar1D: fixed node displacement is zero`` () =
    let model, _ = Helpers.buildBar1DModel 2e11 1e-4 1.0 1000.0
    let input = {
        Model           = model
        LoadCaseIndex   = 0
        UseNonlinear    = false
        NonlinearConfig = NonlinearConfig.defaults
    }
    match FeaPipeline.run input with
    | Error e -> failwith (sprintf "Pipeline failed: %A" e)
    | Ok out  ->
        let u1 = out.Displacements.[NodeId 1].[0]
        Helpers.assertClose 0.0 u1 "u1 (fixed)"

[<Fact>]
let ``bar1D: different stiffness values give correct ratio`` () =
    // u2 = F*L/(E*A); if we double E, u2 halves
    let L = 1.0
    let F = 500.0
    let A = 1e-4
    let runWith e =
        let model, _ = Helpers.buildBar1DModel e A L F
        let input = { Model = model; LoadCaseIndex = 0; UseNonlinear = false; NonlinearConfig = NonlinearConfig.defaults }
        match FeaPipeline.run input with
        | Ok out   -> out.Displacements.[NodeId 2].[0]
        | Error err -> failwith (sprintf "%A" err)
    let u2_E1 = runWith 1e9
    let u2_E2 = runWith 2e9
    Helpers.assertClose (u2_E1 / 2.0) u2_E2 "halved E → halved u2"

[<Fact>]
let ``nonlinear bar1D converges to same solution as linear`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 1000.0
    let model, _ = Helpers.buildBar1DModel E A L F

    let linInput = { Model = model; LoadCaseIndex = 0; UseNonlinear = false; NonlinearConfig = NonlinearConfig.defaults }
    let nlConfig = { NonlinearConfig.defaults with IncrementCount = 5 }
    let nlInput  = { linInput with UseNonlinear = true; NonlinearConfig = nlConfig }

    match FeaPipeline.run linInput, FeaPipeline.run nlInput with
    | Ok lin, Ok nl ->
        let uLin = lin.Displacements.[NodeId 2].[0]
        let uNL  = nl.Displacements.[NodeId 2].[0]
        // For a linear model non-linear converges to same answer (within tolerance)
        let diff = abs (uLin - uNL)
        Assert.True(diff < 1e-6, sprintf "Linear=%.8g  NL=%.8g  diff=%.3e" uLin uNL diff)
    | Error e, _ -> failwith (sprintf "Linear failed: %A" e)
    | _, Error e -> failwith (sprintf "NL failed: %A" e)

[<Fact>]
let ``gaussian elimination solves 3x3 system`` () =
    // Solve: 2x+y=5, 4x+3y=9, 2y+z=4
    //   → x=3, y=-1, z=6
    let A = array2D [ [ 2.0; 1.0; 0.0 ]
                      [ 4.0; 3.0; 0.0 ]
                      [ 0.0; 2.0; 1.0 ] ]
    let b = [| 5.0; 9.0; 4.0 |]
    match GaussianElimination.solve A b with
    | Error e -> failwith (sprintf "Unexpected error: %A" e)
    | Ok x ->
        Helpers.assertClose  3.0 x.[0] "x"
        Helpers.assertClose -1.0 x.[1] "y"
        Helpers.assertClose  6.0 x.[2] "z"

[<Fact>]
let ``gaussian elimination detects singular matrix`` () =
    let A = array2D [ [ 1.0; 2.0 ]; [ 2.0; 4.0 ] ]  // rank 1
    let b = [| 1.0; 2.0 |]
    match GaussianElimination.solve A b with
    | Error [ SingularMatrix ] -> ()  // expected
    | other -> failwith (sprintf "Expected SingularMatrix, got %A" other)
