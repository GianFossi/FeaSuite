namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Core domain types for the FEA model
// ---------------------------------------------------------------------------

// --- Strongly-typed identifiers ------------------------------------------

[<Struct>] type NodeId     = NodeId     of int
[<Struct>] type ElementId  = ElementId  of int
[<Struct>] type MaterialId = MaterialId of int
[<Struct>] type LoadCaseId = LoadCaseId of int

module NodeId     = let value (NodeId     n) = n
module ElementId  = let value (ElementId  e) = e
module MaterialId = let value (MaterialId m) = m
module LoadCaseId = let value (LoadCaseId l) = l

// --- Material ------------------------------------------------------------

/// Linear-elastic isotropic material.
type Material = {
    Id             : MaterialId
    Name           : string
    /// Young's modulus E [Pa]
    YoungModulus   : float
    /// Poisson's ratio ν [-]
    PoissonRatio   : float
    /// Mass density ρ [kg/m³]
    Density        : float
    /// Cross-section area A [m²] – used for 1-D and truss elements.
    CrossSectionArea : float option
}

// --- Node ----------------------------------------------------------------

/// Structural node: position + number of active DOFs.
type Node = {
    Id                : NodeId
    Position          : Point3D
    /// Active degrees of freedom per node (1 for Bar1D, 3 for Truss3D, 6 for Beam3D).
    DegreesOfFreedom  : int
}

// --- Element type --------------------------------------------------------

/// Supported element families.
type ElementType =
    | Bar1D    /// 1-D axial bar, 1 DOF/node
    | Truss3D  /// 3-D truss, 3 DOFs/node
    | Beam2D   /// 2-D Euler-Bernoulli beam, 3 DOFs/node (ux, uy, rz)
    | Beam3D   /// 3-D beam, 6 DOFs/node
    | Shell4   /// 4-node shell (reserved for future implementation)
    | Solid8   /// 8-node hexahedral solid (reserved for future implementation)

module ElementType =
    /// Number of DOFs per node for each element type.
    let dofsPerNode = function
        | Bar1D   -> 1
        | Truss3D -> 3
        | Beam2D  -> 3
        | Beam3D  -> 6
        | Shell4  -> 6
        | Solid8  -> 3

// --- Element -------------------------------------------------------------

/// Structural element: connectivity + material + type-specific properties.
type Element = {
    Id         : ElementId
    Type       : ElementType
    /// Ordered list of node IDs forming this element.
    NodeIds    : NodeId list
    MaterialId : MaterialId
    /// Arbitrary key-value properties (e.g. "CrossSectionArea", "Thickness").
    Properties : Map<string, float>
}

// --- Boundary condition --------------------------------------------------

/// Kinematic constraint on a single DOF.
type DofConstraint =
    | Fixed             /// u = 0
    | Prescribed of float  /// u = given value

/// Boundary condition applied to one DOF of one node.
type BoundaryCondition = {
    NodeId        : NodeId
    /// Local DOF index: 0=ux, 1=uy, 2=uz, 3=rx, 4=ry, 5=rz
    LocalDofIndex : int
    Constraint    : DofConstraint
}

// --- Load ----------------------------------------------------------------

/// Nodal load or moment applied to one DOF.
type Load = {
    NodeId        : NodeId
    LocalDofIndex : int
    Value         : float  /// force [N] or moment [N·m]
}

// --- Load case -----------------------------------------------------------

type LoadCase = {
    Id                  : LoadCaseId
    Name                : string
    Loads               : Load list
    BoundaryConditions  : BoundaryCondition list
}

// --- DOF numbering map ---------------------------------------------------

/// Maps (NodeId, localDofIndex) → global DOF index.
type DofMap = Map<NodeId * int, int>

// --- FEA Model -----------------------------------------------------------

type FEAModel = {
    Nodes      : Map<NodeId, Node>
    Elements   : Map<ElementId, Element>
    Materials  : Map<MaterialId, Material>
    LoadCases  : LoadCase list
}

module FEAModel =
    let empty : FEAModel = {
        Nodes     = Map.empty
        Elements  = Map.empty
        Materials = Map.empty
        LoadCases = []
    }

    let addNode     (n   : Node)      (m: FEAModel) = { m with Nodes     = m.Nodes.Add(n.Id, n) }
    let addElement  (e   : Element)   (m: FEAModel) = { m with Elements  = m.Elements.Add(e.Id, e) }
    let addMaterial (mat : Material)  (m: FEAModel) = { m with Materials = m.Materials.Add(mat.Id, mat) }
    let addLoadCase (lc  : LoadCase)  (m: FEAModel) = { m with LoadCases = m.LoadCases @ [ lc ] }

    /// Compute sequential global DOF numbering and total DOF count.
    let buildDofMap (model: FEAModel) : DofMap * int =
        let mutable dofMap = Map.empty<NodeId * int, int>
        let mutable count  = 0
        for KeyValue(nid, node) in model.Nodes do
            for local in 0 .. node.DegreesOfFreedom - 1 do
                dofMap <- dofMap.Add((nid, local), count)
                count  <- count + 1
        dofMap, count
