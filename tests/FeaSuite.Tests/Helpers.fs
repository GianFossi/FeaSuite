module FeaSuite.Tests.Helpers

open FeaSuite.Core

// ---------------------------------------------------------------------------
// Shared test helpers and model builders
// ---------------------------------------------------------------------------

/// Tolerance for floating-point comparisons.
let [<Literal>] Tol = 1e-10

/// Assert two floats are equal within tolerance.
let assertClose (expected: float) (actual: float) (message: string) =
    let diff = abs (expected - actual)
    if diff > Tol then
        failwith (sprintf "%s — expected %.6g, got %.6g (diff=%.3e)" message expected actual diff)

// ---------------------------------------------------------------------------
// Build a minimal 2-node 1-D bar model
//
//   Node 1 (x=0) ----[Bar EA]---- Node 2 (x=L)
//   BC: Node 1 fixed (u1=0)
//   Load: Node 2, F_axial = F
// ---------------------------------------------------------------------------

let buildBar1DModel
        (E: float) (A: float) (L: float) (F: float)
        : FEAModel * LoadCase =

    let mat = {
        Id               = MaterialId 1
        Name             = "Steel"
        YoungModulus     = E
        PoissonRatio     = 0.3
        Density          = 7850.0
        CrossSectionArea = Some A
    }
    let n1 = { Id = NodeId 1; Position = Point3D.ofXY 0.0 0.0; DegreesOfFreedom = 1 }
    let n2 = { Id = NodeId 2; Position = Point3D.ofXY L   0.0; DegreesOfFreedom = 1 }
    let el = {
        Id         = ElementId 1
        Type       = Beam Bar1D
        NodeIds    = [ NodeId 1; NodeId 2 ]
        MaterialId = MaterialId 1
        Properties = Map.empty
    }
    let bc = { NodeId = NodeId 1; LocalDofIndex = 0; Constraint = Fixed }
    let ld = { NodeId = NodeId 2; LocalDofIndex = 0; Value = F }
    let lc = { Id = LoadCaseId 1; Name = "LC1"; Loads = [ ld ]; BoundaryConditions = [ bc ] }
    let model =
        FEAModel.empty
        |> FEAModel.addMaterial mat
        |> FEAModel.addNode n1
        |> FEAModel.addNode n2
        |> FEAModel.addElement el
        |> FEAModel.addLoadCase lc
    model, lc
