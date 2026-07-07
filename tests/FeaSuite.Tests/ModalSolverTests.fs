module FeaSuite.Tests.ModalSolverTests

open System
open Xunit
open FeaSuite.Core
open FeaSuite.Solvers

// ---------------------------------------------------------------------------
// Modal solver tests
//
// Analytical reference for a 1-D bar fixed at one end (Bar1D, 1 DOF/node):
//
//   Model:  N1(x=0) --[Bar EA, ρ]-- N2(x=L)
//   BC: u1 = 0 (fixed)
//   1 free DOF (u2 only)
//
//   Reduced stiffness:  k_r = EA/L
//   Lumped mass at N2:  m_r = ρ·A·L/2
//   (N1 has zero free DOF, so the other half of element mass is "lost" to BC)
//
//   Single eigenvalue:   λ = k_r / m_r = (EA/L) / (ρ·A·L/2) = 2E/(ρ·L²)
//   Angular frequency:   ω = √λ
//   Natural frequency:   f = ω / (2π)
//
// For a 2-element bar fixed at both ends (N1 fixed, N3 fixed, N2 free):
//   k_r = EA/L + EA/L = 2EA/L   (two springs in parallel)
//   m_r = ρ·A·(L/2)  (half-mass from each element segment to the free node)
//   λ = k_r / m_r = (2EA/L) / (ρ·A·L/2) = 4E/(ρ·L²)
//   ω = 2·√(E/(ρ·L²))
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Shared helper: build a 2-node Bar1D model (fixed at node 1, free at node 2)
// ---------------------------------------------------------------------------
let private buildBar1DModalModel (E: float) (rho: float) (A: float) (L: float)
        : FEAModel * LoadCase =
    let mat = {
        Id               = MaterialId 1
        Name             = "Mat"
        YoungModulus     = E
        PoissonRatio     = 0.3
        Density          = rho
        CrossSectionArea = Some A
    }
    let n1 = { Id = NodeId 1; Position = Point3D.ofXY 0.0 0.0; DegreesOfFreedom = 1 }
    let n2 = { Id = NodeId 2; Position = Point3D.ofXY L   0.0; DegreesOfFreedom = 1 }
    let el = { Id = ElementId 1; Type = Beam Bar1D; NodeIds = [ NodeId 1; NodeId 2 ]
               MaterialId = MaterialId 1; Properties = Map.empty }
    // Load case only supplies the BC; no loads needed for modal analysis.
    let bc = { NodeId = NodeId 1; LocalDofIndex = 0; Constraint = Fixed }
    let lc = { Id = LoadCaseId 1; Name = "Modal"; Loads = []; BoundaryConditions = [ bc ] }
    let model =
        FEAModel.empty
        |> FEAModel.addMaterial mat
        |> FEAModel.addNode n1
        |> FEAModel.addNode n2
        |> FEAModel.addElement el
        |> FEAModel.addLoadCase lc
    model, lc


// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

[<Fact>]
let ``ModalPipeline: single free-DOF bar1D returns 1 mode`` () =
    let E   = 2e11
    let rho = 7850.0
    let A   = 1e-4
    let L   = 1.0
    let model, _ = buildBar1DModalModel E rho A L
    let input = { ModalSolveInput.defaults with Model = model; LoadCaseIndex = 0
                                                Config = { NumberOfModes = 5 } }
    match ModalPipeline.run input with
    | Error e -> failwith (sprintf "Modal pipeline failed: %A" e)
    | Ok out  ->
        // Only 1 free DOF → at most 1 mode can be extracted
        Assert.Equal(1, out.Modes.Length)

[<Fact>]
let ``ModalPipeline: bar1D natural frequency matches analytical`` () =
    let E   = 2e11
    let rho = 7850.0
    let A   = 1e-4
    let L   = 1.0
    let model, _ = buildBar1DModalModel E rho A L
    let input = { ModalSolveInput.defaults with Model = model; LoadCaseIndex = 0
                                                Config = { NumberOfModes = 1 } }
    match ModalPipeline.run input with
    | Error e -> failwith (sprintf "Modal pipeline failed: %A" e)
    | Ok out  ->
        // Analytical: λ = 2E/(ρ·L²),  ω = √λ,  f = ω/(2π)
        let kR = E * A / L          // reduced stiffness
        let mR = rho * A * L / 2.0  // lumped mass at free DOF
        let omegaExpected = sqrt (kR / mR)
        let freqExpected  = omegaExpected / (2.0 * Math.PI)
        let mode1 = out.Modes.[0]
        let diffOmega = abs (mode1.AngularFrequency - omegaExpected) / omegaExpected
        let diffFreq  = abs (mode1.NaturalFrequency  - freqExpected)  / freqExpected
        Assert.True(diffOmega < 1e-9,
            sprintf "ω mismatch: expected %.6g rad/s, got %.6g (rel err=%.3e)"
                    omegaExpected mode1.AngularFrequency diffOmega)
        Assert.True(diffFreq  < 1e-9,
            sprintf "f mismatch: expected %.6g Hz, got %.6g (rel err=%.3e)"
                    freqExpected mode1.NaturalFrequency diffFreq)

[<Fact>]
let ``ModalPipeline: bar1D mode index is 1-based`` () =
    let model, _ = buildBar1DModalModel 2e11 7850.0 1e-4 1.0
    let input = { ModalSolveInput.defaults with Model = model }
    match ModalPipeline.run input with
    | Error e -> failwith (sprintf "%A" e)
    | Ok out  ->
        Assert.Equal(1, out.Modes.[0].Index)

[<Fact>]
let ``ModalPipeline: bar1D mode shape has zero at constrained DOF`` () =
    let model, _ = buildBar1DModalModel 2e11 7850.0 1e-4 1.0
    let input = { ModalSolveInput.defaults with Model = model
                                                Config = { NumberOfModes = 1 } }
    match ModalPipeline.run input with
    | Error e -> failwith (sprintf "%A" e)
    | Ok out  ->
        let phi = out.Modes.[0].ModeShape
        // DOF 0 belongs to Node 1 (fixed); should be zero
        Assert.Equal(0.0, phi.[0])
        // DOF 1 belongs to Node 2 (free); should be non-zero
        Assert.NotEqual(0.0, phi.[1])

[<Fact>]
let ``ModalPipeline: modes are sorted by ascending frequency`` () =
    // Build a 3-node, 2-element bar (fixed at node 1 only → 2 free DOFs)
    let E   = 2e11
    let rho = 7850.0
    let A   = 1e-4
    let L   = 0.5   // each element length
    let mat = { Id = MaterialId 1; Name = "Mat"
                YoungModulus = E; PoissonRatio = 0.3
                Density = rho; CrossSectionArea = Some A }
    let n1  = { Id = NodeId 1; Position = Point3D.ofXY 0.0 0.0; DegreesOfFreedom = 1 }
    let n2  = { Id = NodeId 2; Position = Point3D.ofXY L   0.0; DegreesOfFreedom = 1 }
    let n3  = { Id = NodeId 3; Position = Point3D.ofXY (2.0*L) 0.0; DegreesOfFreedom = 1 }
    let e1  = { Id = ElementId 1; Type = Beam Bar1D; NodeIds = [ NodeId 1; NodeId 2 ]
                MaterialId = MaterialId 1; Properties = Map.empty }
    let e2  = { Id = ElementId 2; Type = Beam Bar1D; NodeIds = [ NodeId 2; NodeId 3 ]
                MaterialId = MaterialId 1; Properties = Map.empty }
    let bc  = { NodeId = NodeId 1; LocalDofIndex = 0; Constraint = Fixed }
    let lc  = { Id = LoadCaseId 1; Name = "Modal"; Loads = []; BoundaryConditions = [ bc ] }
    let model =
        FEAModel.empty
        |> FEAModel.addMaterial mat
        |> FEAModel.addNode n1 |> FEAModel.addNode n2 |> FEAModel.addNode n3
        |> FEAModel.addElement e1 |> FEAModel.addElement e2
        |> FEAModel.addLoadCase lc

    let input = { ModalSolveInput.defaults with Model = model; Config = { NumberOfModes = 2 } }
    match ModalPipeline.run input with
    | Error e -> failwith (sprintf "%A" e)
    | Ok out  ->
        Assert.Equal(2, out.Modes.Length)
        // Frequencies must be ascending
        Assert.True(out.Modes.[0].NaturalFrequency <= out.Modes.[1].NaturalFrequency,
            sprintf "Mode 1 freq=%.4g > Mode 2 freq=%.4g" out.Modes.[0].NaturalFrequency out.Modes.[1].NaturalFrequency)

[<Fact>]
let ``ModalPipeline: NumberOfModes limits modes returned`` () =
    // 3-node model → 2 free DOFs; request only 1 mode
    let mat = { Id = MaterialId 1; Name = "Mat"
                YoungModulus = 2e11; PoissonRatio = 0.3
                Density = 7850.0; CrossSectionArea = Some 1e-4 }
    let n1  = { Id = NodeId 1; Position = Point3D.ofXY 0.0 0.0; DegreesOfFreedom = 1 }
    let n2  = { Id = NodeId 2; Position = Point3D.ofXY 0.5 0.0; DegreesOfFreedom = 1 }
    let n3  = { Id = NodeId 3; Position = Point3D.ofXY 1.0 0.0; DegreesOfFreedom = 1 }
    let e1  = { Id = ElementId 1; Type = Beam Bar1D; NodeIds = [ NodeId 1; NodeId 2 ]
                MaterialId = MaterialId 1; Properties = Map.empty }
    let e2  = { Id = ElementId 2; Type = Beam Bar1D; NodeIds = [ NodeId 2; NodeId 3 ]
                MaterialId = MaterialId 1; Properties = Map.empty }
    let bc  = { NodeId = NodeId 1; LocalDofIndex = 0; Constraint = Fixed }
    let lc  = { Id = LoadCaseId 1; Name = "Modal"; Loads = []; BoundaryConditions = [ bc ] }
    let model =
        FEAModel.empty |> FEAModel.addMaterial mat
        |> FEAModel.addNode n1 |> FEAModel.addNode n2 |> FEAModel.addNode n3
        |> FEAModel.addElement e1 |> FEAModel.addElement e2
        |> FEAModel.addLoadCase lc
    let input = { ModalSolveInput.defaults with Model = model; Config = { NumberOfModes = 1 } }
    match ModalPipeline.run input with
    | Error e -> failwith (sprintf "%A" e)
    | Ok out  -> Assert.Equal(1, out.Modes.Length)

[<Fact>]
let ``ModalPipeline: TotalDofs matches model DOF count`` () =
    let model, _ = buildBar1DModalModel 2e11 7850.0 1e-4 1.0
    let input = { ModalSolveInput.defaults with Model = model }
    match ModalPipeline.run input with
    | Error e -> failwith (sprintf "%A" e)
    | Ok out  ->
        let _, expected = FEAModel.buildDofMap model
        Assert.Equal(expected, out.TotalDofs)

[<Fact>]
let ``ModalPipeline: angular frequency and natural frequency are consistent`` () =
    let model, _ = buildBar1DModalModel 2e11 7850.0 1e-4 1.0
    let input = { ModalSolveInput.defaults with Model = model; Config = { NumberOfModes = 1 } }
    match ModalPipeline.run input with
    | Error e -> failwith (sprintf "%A" e)
    | Ok out  ->
        let m = out.Modes.[0]
        let fFromOmega = m.AngularFrequency / (2.0 * Math.PI)
        let diff = abs (fFromOmega - m.NaturalFrequency)
        Assert.True(diff < 1e-10,
            sprintf "f from ω=%.8g vs stored f=%.8g (diff=%.3e)"
                    fFromOmega m.NaturalFrequency diff)

// ---------------------------------------------------------------------------
// 4.1.1  Direct (Sparse) Solver tests – SparseDirect via FeaPipeline
// ---------------------------------------------------------------------------

[<Fact>]
let ``SparseDirect: bar1D displacement matches analytical`` () =
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
        LinearSolverKind   = SparseDirect
        UseSparseAssembler = false
    }
    match FeaPipeline.run input with
    | Error e -> failwith (sprintf "Pipeline failed: %A" e)
    | Ok out  ->
        let expected = F * L / (E * A)
        let actual   = out.Displacements.[NodeId 2].[0]
        Helpers.assertClose expected actual "SparseDirect bar1D u2"

[<Fact>]
let ``SparseDirect: result agrees with Dense solver on bar1D`` () =
    let model, _ = Helpers.buildBar1DModel 2e11 1e-4 1.0 1000.0
    let run kind =
        let input = { Model = model; LoadCaseIndex = 0; UseNonlinear = false
                      NonlinearConfig = NonlinearConfig.defaults
                      LinearSolverKind = kind; UseSparseAssembler = false }
        match FeaPipeline.run input with
        | Ok out   -> out.Displacements.[NodeId 2].[0]
        | Error e  -> failwith (sprintf "%A" e)
    let uDense  = run Dense
    let uSparse = run SparseDirect
    Assert.True(abs (uDense - uSparse) < 1e-9,
        sprintf "Dense=%.8g  SparseDirect=%.8g" uDense uSparse)

[<Fact>]
let ``SparseDirect: reaction at fixed node equals -F`` () =
    let F = 1000.0
    let model, _ = Helpers.buildBar1DModel 2e11 1e-4 1.0 F
    let input = { Model = model; LoadCaseIndex = 0; UseNonlinear = false
                  NonlinearConfig = NonlinearConfig.defaults
                  LinearSolverKind = SparseDirect; UseSparseAssembler = false }
    match FeaPipeline.run input with
    | Error e -> failwith (sprintf "%A" e)
    | Ok out  ->
        match out.Reactions.TryFind (NodeId 1, 0) with
        | None   -> failwith "No reaction at Node 1"
        | Some r -> Helpers.assertClose (-F) r "SparseDirect reaction R1"

// ---------------------------------------------------------------------------
// 4.1.2  Iterative PCG Solver tests (CgSolver / SparseCg)
// These tests complement the existing IterativeSolverTests.fs and verify
// key PCG-specific behaviour.
// ---------------------------------------------------------------------------

[<Fact>]
let ``PCG: CgSolver converges on bar1D within default iteration budget`` () =
    let model, _ = Helpers.buildBar1DModel 2e11 1e-4 1.0 1000.0
    let input = { Model = model; LoadCaseIndex = 0; UseNonlinear = false
                  NonlinearConfig = NonlinearConfig.defaults
                  LinearSolverKind = LinearSolverKind.defaultCg
                  UseSparseAssembler = false }
    match FeaPipeline.run input with
    | Error e -> failwith (sprintf "PCG pipeline failed: %A" e)
    | Ok out  ->
        let expected = 1000.0 * 1.0 / (2e11 * 1e-4)
        Helpers.assertClose expected out.Displacements.[NodeId 2].[0] "PCG u2"

[<Fact>]
let ``PCG: diagnostics Converged=true and residual below tolerance`` () =
    let K = array2D [ [  4.0;  1.0;  0.0 ]
                      [  1.0;  3.0; -1.0 ]
                      [  0.0; -1.0;  2.0 ] ]
    let b   = [| 1.0; 2.0; 3.0 |]
    let sys = { new IAssembledSystem with
                    member _.TotalDofs = 3
                    member _.K         = K
                    member _.F         = b }
    let solver = CgSolver(maxIterations = 1000, tolerance = 1e-12)
    match (solver :> ILinearSolver).Solve(sys, [], Map.empty) with
    | Error e -> failwith (sprintf "%A" e)
    | Ok _ ->
        let diag = (solver :> IIterativeSolverDiagnostics).LastDiagnostics.Value
        Assert.True(diag.Converged,    sprintf "PCG did not converge; rNorm=%.3e" diag.ResidualNorm)
        Assert.True(diag.ResidualNorm < 1e-10, sprintf "Residual too large: %.3e" diag.ResidualNorm)
