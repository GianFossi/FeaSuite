module FeaSuite.Tests.Truss3DPipelineTests

open Xunit
open FeaSuite.Core
open FeaSuite.Solvers

// ---------------------------------------------------------------------------
// Truss3D pipeline tests (full 3-D truss with multiple load cases)
// Parametric/table-driven style for easy extension.
// ---------------------------------------------------------------------------

type private TrussCase = {
    Name: string
    LoadOnTopNode: float * float * float // Fx, Fy, Fz on Node 4
    Scale: float
}

let private buildTruss3DModelWithCases (cases: TrussCase list) : FEAModel =
    let e = 210e9
    let a = 1.0e-4

    let mat = {
        Id               = MaterialId 1
        Name             = "Steel"
        YoungModulus     = e
        PoissonRatio     = 0.30
        Density          = 7850.0
        CrossSectionArea = Some a
    }

    // Stable tetra-like 3D truss topology
    let n1 = { Id = NodeId 1; Position = { X = 0.0; Y = 0.0; Z = 0.0 }; DegreesOfFreedom = 3 }
    let n2 = { Id = NodeId 2; Position = { X = 1.0; Y = 0.0; Z = 0.0 }; DegreesOfFreedom = 3 }
    let n3 = { Id = NodeId 3; Position = { X = 0.0; Y = 1.0; Z = 0.0 }; DegreesOfFreedom = 3 }
    let n4 = { Id = NodeId 4; Position = { X = 0.0; Y = 0.0; Z = 1.0 }; DegreesOfFreedom = 3 }

    let mkBar eid i j = {
        Id         = ElementId eid
        Type       = Beam Truss3D
        NodeIds    = [ NodeId i; NodeId j ]
        MaterialId = MaterialId 1
        Properties = NoProperties
    }

    let elements = [
        mkBar 1 1 2
        mkBar 2 1 3
        mkBar 3 1 4
        mkBar 4 2 3
        mkBar 5 2 4
        mkBar 6 3 4
    ]

    // Fix base nodes (1,2,3) all translational DOFs -> top node remains free
    let baseBcs =
        [ for nid in [1;2;3] do
            for dof in [0;1;2] do
                yield { NodeId = NodeId nid; LocalDofIndex = dof; Constraint = Fixed } ]

    let loadCases =
        cases
        |> List.mapi (fun idx c ->
            let fx, fy, fz = c.LoadOnTopNode
            let loads =
                [ { NodeId = NodeId 4; LocalDofIndex = 0; Value = fx * c.Scale }
                  { NodeId = NodeId 4; LocalDofIndex = 1; Value = fy * c.Scale }
                  { NodeId = NodeId 4; LocalDofIndex = 2; Value = fz * c.Scale } ]
            { Id = LoadCaseId (idx + 1)
              Name = c.Name
              Loads = loads
              BoundaryConditions = baseBcs })

    FEAModel.empty
    |> FEAModel.addMaterial mat
    |> FEAModel.addNode n1
    |> FEAModel.addNode n2
    |> FEAModel.addNode n3
    |> FEAModel.addNode n4
    |> (fun m -> elements |> List.fold (fun acc e -> FEAModel.addElement e acc) m)
    |> (fun m -> loadCases |> List.fold (fun acc lc -> FEAModel.addLoadCase lc acc) m)

let private runLinear (model: FEAModel) (lcIndex: int) =
    let input = {
        Model           = model
        LoadCaseIndex   = lcIndex
        UseNonlinear    = false
        NonlinearConfig = NonlinearConfig.defaults
        LinearSolverKind   = Dense
        UseSparseAssembler = false
    }
    FeaPipeline.run input

let private sumAbsReactionsAtBase (out: SolveOutput) =
    out.Reactions
    |> Seq.filter (fun kv ->
        let (nid, _) = kv.Key
        nid = NodeId 1 || nid = NodeId 2 || nid = NodeId 3)
    |> Seq.sumBy (fun kv -> abs kv.Value)

[<Fact>]
let ``truss3D parametric: pipeline runs all load cases and responses scale consistently`` () =
    let cases = [
        { Name = "LC_Z_base";      LoadOnTopNode = (0.0, 0.0, -10_000.0); Scale = 1.0 }
        { Name = "LC_Z_double";    LoadOnTopNode = (0.0, 0.0, -10_000.0); Scale = 2.0 }
        { Name = "LC_xyz_mixed";   LoadOnTopNode = (3_000.0, -2_000.0, -8_000.0); Scale = 1.0 }
    ]

    let model = buildTruss3DModelWithCases cases

    // Run all load-cases (parametric / table-driven)
    let results =
        cases
        |> List.mapi (fun i c ->
            match runLinear model i with
            | Error e -> failwith (sprintf "Pipeline failed for %s: %A" c.Name e)
            | Ok out  -> c, out)

    // 1) Basic sanity for each case: top node displacement exists and at least one component is non-zero
    for (c, out) in results do
        let uTop = out.Displacements.[NodeId 4]
        Assert.Equal(3, uTop.Length)
        let norm = sqrt (uTop.[0] * uTop.[0] + uTop.[1] * uTop.[1] + uTop.[2] * uTop.[2])
        Assert.True(norm > 0.0, sprintf "%s: expected non-zero top-node displacement" c.Name)

        // Reactions should exist on base supports and be finite
        let rSum = sumAbsReactionsAtBase out
        Assert.True(rSum > 0.0, sprintf "%s: expected non-zero base reactions" c.Name)

    // 2) Scaling check between LC_Z_base and LC_Z_double on uz of top node
    let _, outBase  = results.[0]
    let _, outDouble = results.[1]
    let uzBase = outBase.Displacements.[NodeId 4].[2]
    let uzDouble = outDouble.Displacements.[NodeId 4].[2]

    // Linear system => displacement should scale approximately with load
    let ratio = uzDouble / uzBase
    Assert.InRange(ratio, 1.95, 2.05)

    // 3) Mixed load case should activate at least two displacement components at top node
    let _, outMixed = results.[2]
    let uMix = outMixed.Displacements.[NodeId 4]
    let activeCount =
        [| abs uMix.[0]; abs uMix.[1]; abs uMix.[2] |]
        |> Array.filter (fun v -> v > 1e-14)
        |> Array.length
    Assert.True(activeCount >= 2, "LC_xyz_mixed: expected at least two active displacement components")
