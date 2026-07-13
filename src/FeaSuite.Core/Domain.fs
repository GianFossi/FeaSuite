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
    /// Active degrees of freedom per node (1 for Beam Bar1D, 3 for Beam Truss3D, 6 for Beam Beam3D).
    DegreesOfFreedom  : int
}

// --- Element type --------------------------------------------------------

/// Top-level element family: wraps a group-specific sub-type.
type ElementType =
    | Beam         of BeamElement         /// 1-D bar, truss, or beam element
    | Shell        of ShellElement        /// Shell element
    | Axisymmetric of AxisymmetricElement /// Axisymmetric / harmonic-plane element
    | Link         of LinkElement         /// Link element
    | Pipe         of PipeElement         /// Pipe or elbow element
    | Special      of SpecialElement      /// Solid, fluid, multi-physics, or other special element

module ElementType =
    /// Number of DOFs per node for each element type.
    let dofsPerNode = function
        | Beam e ->
            match e with
            | Bar1D   -> 1   // UX
            | Truss3D -> 3   // UX, UY, UZ
            | Beam2D  -> 3   // UX, UY, ROTZ
            | Beam3D  -> 6   // UX, UY, UZ, ROTX, ROTY, ROTZ
        | Shell e ->
            match e with
            | Shell4  -> 6  // UX, UY, UZ, ROTX, ROTY, ROTZ
            | Shell61 -> 4  // UX, UY, UZ, ROTZ
        | Axisymmetric e ->
            match e with
            | Plane75  -> 1   // TEMP
            | Plane78  -> 1   // TEMP
            | Plane83  -> 3   // UX, UY, UZ
            | Shell208 -> 3   // UX, UY, ROTZ
            | Shell209 -> 3   // UX, UY, ROTZ
        | Link e ->
            match e with
            | Link11  -> 3   // UX, UY, UZ
            | Link31  -> 1   // TEMP
            | Link33  -> 1   // TEMP
            | Link34  -> 1   // TEMP
            | Link68  -> 2   // TEMP, VOLT
            | Link180 -> 3   // UX, UY, UZ
            | Link228 -> 5   // UX, UY, UZ, TEMP, VOLT
        | Pipe e ->
            match e with
            | Pipe288  -> 6  // UX, UY, UZ, ROTX, ROTY, ROTZ
            | Pipe289  -> 6  // UX, UY, UZ, ROTX, ROTY, ROTZ
            | Elbow290 -> 6  // UX, UY, UZ, ROTX, ROTY, ROTZ
        | Special e ->
            match e with
            | Solid8   -> 3   // UX, UY, UZ
            | Cpt212   -> 4   // UX, UY, PRES, TEMP
            | Cpt213   -> 4   // UX, UY, PRES, TEMP
            | Cpt215   -> 5   // UX, UY, UZ, PRES, TEMP
            | Cpt216   -> 5   // UX, UY, UZ, PRES, TEMP
            | Cpt217   -> 5   // UX, UY, UZ, PRES, TEMP
            | Fluid29  -> 3   // UX, UY, PRES
            | Fluid30  -> 5   // UX, UY, UZ, PRES, ENKE
            | Fluid38  -> 3   // UX, UY, UZ
            | Fluid116 -> 2   // PRES, TEMP
            | Fluid129 -> 1   // PRES
            | Fluid130 -> 1   // PRES
            | Fluid136 -> 1   // PRES
            | Fluid138 -> 1   // PRES
            | Fluid139 -> 3   // UX, UY, UZ
            | Fluid218 -> 4   // UX, UY, UZ, PRES
            | Fluid220 -> 9   // UX, UY, UZ, PRES, ENKE, VX, VY, VZ, TEMP
            | Fluid221 -> 9   // UX, UY, UZ, PRES, ENKE, VX, VY, VZ, TEMP
            | Fluid243 -> 7   // UX, UY, PRES, ENKE, VX, VY, TEMP
            | Fluid244 -> 7   // UX, UY, PRES, ENKE, VX, VY, TEMP
            | Follw201 -> 6   // UX, UY, UZ, ROTX, ROTY, ROTZ
            | Hsfld241 -> 4   // UX, UY, HDSP, PRES
            | Hsfld242 -> 5   // UX, UY, UZ, HDSP, PRES
            | Infin47  -> 2   // MAG, TEMP
            | Infin110 -> 3   // AZ, VOLT, TEMP
            | Infin111 -> 4   // MAG, AZ, VOLT, TEMP
            | Infin257 -> 3   // UX, UY, UZ (3D); for 2D models, Node.DegreesOfFreedom governs the actual per-node count (2)
            | Inter192 -> 2   // UX, UY
            | Inter193 -> 2   // UX, UY
            | Inter194 -> 3   // UX, UY, UZ
            | Inter195 -> 3   // UX, UY, UZ
            | Inter202 -> 2   // UX, UY
            | Inter203 -> 2   // UX, UY
            | Inter204 -> 3   // UX, UY, UZ
            | Inter205 -> 3   // UX, UY, UZ
            | Mass21   -> 6   // UX, UY, UZ, ROTX, ROTY, ROTZ
            | Mass71   -> 1   // TEMP
            | Mpc184   -> 6   // UX, UY, UZ, ROTX, ROTY, ROTZ

// --- Element -------------------------------------------------------------

/// Structural element: connectivity + material + type-specific properties.
type Element = {
    Id         : ElementId
    Type       : ElementType
    /// Ordered list of node IDs forming this element.
    NodeIds    : NodeId list
    MaterialId : MaterialId
    /// Typed section / geometry properties specific to this element's family.
    Properties : ElementProperties
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

// --- Acceleration load ---------------------------------------------------

/// Body acceleration field applied to all elements (gravity, seismic base acceleration, etc.).
/// Components are in the global coordinate system [m/s²].
/// Positive Z means upward; gravity is typically { Ax=0; Ay=0; Az=-9.80665 }.
type AccelerationLoad = {
    Ax : float
    Ay : float
    Az : float
}

module AccelerationLoad =
    /// Standard gravitational acceleration acting in the −Z direction (g = 9.80665 m/s²).
    let gravity : AccelerationLoad = { Ax = 0.0; Ay = 0.0; Az = -9.80665 }
    /// Gravitational acceleration along −Z with a custom magnitude.
    let gravityAlongZ (g: float) : AccelerationLoad = { Ax = 0.0; Ay = 0.0; Az = -g }

// --- Load case -----------------------------------------------------------

type LoadCase = {
    Id                  : LoadCaseId
    Name                : string
    Loads               : Load list
    BoundaryConditions  : BoundaryCondition list
    AccelerationLoads   : AccelerationLoad list
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
