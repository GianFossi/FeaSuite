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
    // --- ANSYS SHELL elements -----------------------------------------------
    | Shell208 /// 2-Node Axisymmetric Shell; 2D; DOF: UX, UY, ROTZ
    | Shell209 /// 3-Node Axisymmetric Shell; 2D; DOF: UX, UY, ROTZ
    | Shell61  /// 2D Axisymmetric-Harmonic Structural Shell; 2 nodes; DOF: UX, UY, UZ, ROTZ
    // --- ANSYS CPT (Coupled Pore-Pressure-Thermal Mechanical) elements -------
    | Cpt212   /// 2D 4-Node Coupled Pore-Pressure-Thermal Mechanical Solid; DOF: UX, UY, PRES, TEMP
    | Cpt213   /// 2D 8-Node Coupled Pore-Pressure-Thermal Mechanical Solid; DOF: UX, UY, PRES, TEMP
    | Cpt215   /// 3D 8-Node Coupled Pore-Pressure-Thermal Mechanical Solid; DOF: UX, UY, UZ, PRES, TEMP
    | Cpt216   /// 3D 20-Node Coupled Pore-Pressure-Thermal Mechanical Solid; DOF: UX, UY, UZ, PRES, TEMP
    | Cpt217   /// 3D 10-Node Coupled Pore-Pressure-Thermal Mechanical Solid; DOF: UX, UY, UZ, PRES, TEMP
    // --- ANSYS FLUID elements -----------------------------------------------
    | Fluid29  /// 2D Acoustic Fluid; 4 nodes; DOF: UX, UY, PRES
    | Fluid30  /// 3D Acoustic Fluid; 8 nodes; DOF: UX, UY, UZ, PRES, ENKE
    | Fluid38  /// Dynamic Fluid Coupling; 2 nodes; DOF: UX, UY, UZ
    | Fluid116 /// Coupled Thermal-Fluid Pipe; 2 nodes; DOF: PRES, TEMP
    | Fluid129 /// 2D Infinite Acoustic; 2 or 3 nodes; DOF: PRES
    | Fluid130 /// 3D Infinite Acoustic; 4 or 8 nodes; DOF: PRES
    | Fluid136 /// 3D Squeeze Film Fluid; 4 or 8 nodes; DOF: PRES
    | Fluid138 /// 3D Viscous Fluid Link; 2 nodes; DOF: PRES
    | Fluid139 /// 3D Slide Film Fluid; 2 or 32 nodes; DOF: UX, UY, UZ
    | Fluid218 /// 3D Hydrodynamic Bearing Element; 4 nodes; DOF: UX, UY, UZ, PRES
    | Fluid220 /// 3D Acoustic Fluid; 20 nodes; DOF: UX, UY, UZ, PRES, ENKE, VX, VY, VZ, TEMP
    | Fluid221 /// 3D Acoustic Fluid; 10 nodes; DOF: UX, UY, UZ, PRES, ENKE, VX, VY, VZ, TEMP
    | Fluid243 /// 2D 4-Node Acoustic Fluid; DOF: UX, UY, PRES, ENKE, VX, VY, TEMP
    | Fluid244 /// 2D 8-Node Acoustic Fluid; DOF: UX, UY, PRES, ENKE, VX, VY, TEMP
    // --- ANSYS FOLLW elements -----------------------------------------------
    | Follw201 /// 3D Follower Load; 1 node; DOF: UX, UY, UZ, ROTX, ROTY, ROTZ
    // --- ANSYS HSFLD elements -----------------------------------------------
    | Hsfld241 /// 2D Hydrostatic Fluid; 4 nodes; DOF: UX, UY, HDSP, PRES
    | Hsfld242 /// 3D Hydrostatic Fluid; 9 nodes; DOF: UX, UY, UZ, HDSP, PRES
    // --- ANSYS INFIN elements -----------------------------------------------
    | Infin47  /// 3D Infinite Boundary; 4 nodes; DOF: MAG, TEMP
    | Infin110 /// 2D Infinite Solid; 4 or 8 nodes; DOF: AZ, VOLT, TEMP
    | Infin111 /// 3D Infinite Solid; 8 or 20 nodes; DOF: MAG, AZ, VOLT, TEMP
    | Infin257 /// 2D/3D Structural Infinite Solid; DOF (2D): UX, UY (2/node); DOF (3D): UX, UY, UZ (3/node) – actual count governed by Node.DegreesOfFreedom
    // --- ANSYS INTER elements -----------------------------------------------
    | Inter192 /// Structural 2D Interface 4-Node Gasket; DOF: UX, UY
    | Inter193 /// Structural 2D Interface 6-Node Gasket; DOF: UX, UY
    | Inter194 /// Structural 3D Interface 16-Node Gasket; DOF: UX, UY, UZ
    | Inter195 /// Structural 3D Interface 8-Node Gasket; DOF: UX, UY, UZ
    | Inter202 /// Structural 2D Interface 4-Node Cohesive; DOF: UX, UY
    | Inter203 /// Structural 2D Interface 6-Node Cohesive; DOF: UX, UY
    | Inter204 /// Structural 3D Interface 16-Node Cohesive; DOF: UX, UY, UZ
    | Inter205 /// Structural 3D Interface 8-Node Cohesive; DOF: UX, UY, UZ
    // --- ANSYS LINK elements ------------------------------------------------
    | Link11   /// Structural 3D Linear Actuator; 2 nodes; DOF: UX, UY, UZ
    | Link31   /// Radiation Link; 2 nodes; DOF: TEMP
    | Link33   /// Thermal 3D Conduction Bar; 2 or 3 nodes; DOF: TEMP
    | Link34   /// Convection Link; 2 nodes; DOF: TEMP
    | Link68   /// Coupled Thermal-Electric Line; 2 nodes; DOF: TEMP, VOLT
    | Link180  /// Structural 3D Spar (or Truss); 2 nodes; DOF: UX, UY, UZ
    | Link228  /// 3D Coupled-Field Link; 2 or 3 nodes; DOF: UX, UY, UZ, TEMP, VOLT
    // --- ANSYS MASS elements ------------------------------------------------
    | Mass21   /// Structural Mass; 1 node; DOF: UX, UY, UZ, ROTX, ROTY, ROTZ
    | Mass71   /// Thermal Mass; 1 node; DOF: TEMP
    // --- ANSYS MPC elements -------------------------------------------------
    | Mpc184   /// Structural Multipoint Constraint; 2 or 3 nodes; DOF: UX, UY, UZ, ROTX, ROTY, ROTZ
    // --- ANSYS PIPE elements ------------------------------------------------
    | Pipe288  /// 3D 2-Node Pipe; DOF: UX, UY, UZ, ROTX, ROTY, ROTZ
    | Pipe289  /// 3D 3-Node Pipe; DOF: UX, UY, UZ, ROTX, ROTY, ROTZ
    | Elbow290 /// 3D 3-Node Elbow; DOF: UX, UY, UZ, ROTX, ROTY, ROTZ
    // --- ANSYS PLANE elements -----------------------------------------------
    | Plane75  /// 2D Axisymmetric-Harmonic 4-Node Thermal Solid; DOF: TEMP
    | Plane78  /// 2D Axisymmetric-Harmonic 8-Node Thermal Solid; DOF: TEMP
    | Plane83  /// 2D Axisymmetric-Harmonic 8-Node Structural Solid; DOF: UX, UY, UZ

module ElementType =
    /// Number of DOFs per node for each element type.
    let dofsPerNode = function
        | Bar1D   -> 1
        | Truss3D -> 3
        | Beam2D  -> 3
        | Beam3D  -> 6
        | Shell4  -> 6
        | Solid8  -> 3
        // ANSYS SHELL
        | Shell208 -> 3   // UX, UY, ROTZ
        | Shell209 -> 3   // UX, UY, ROTZ
        | Shell61  -> 4   // UX, UY, UZ, ROTZ
        // ANSYS CPT
        | Cpt212 -> 4     // UX, UY, PRES, TEMP
        | Cpt213 -> 4     // UX, UY, PRES, TEMP
        | Cpt215 -> 5     // UX, UY, UZ, PRES, TEMP
        | Cpt216 -> 5     // UX, UY, UZ, PRES, TEMP
        | Cpt217 -> 5     // UX, UY, UZ, PRES, TEMP
        // ANSYS FLUID
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
        // ANSYS FOLLW
        | Follw201 -> 6   // UX, UY, UZ, ROTX, ROTY, ROTZ
        // ANSYS HSFLD
        | Hsfld241 -> 4   // UX, UY, HDSP, PRES
        | Hsfld242 -> 5   // UX, UY, UZ, HDSP, PRES
        // ANSYS INFIN
        | Infin47  -> 2   // MAG, TEMP
        | Infin110 -> 3   // AZ, VOLT, TEMP
        | Infin111 -> 4   // MAG, AZ, VOLT, TEMP
        | Infin257 -> 3   // UX, UY, UZ (3D); for 2D models, Node.DegreesOfFreedom governs the actual per-node count (2)
        // ANSYS INTER
        | Inter192 -> 2   // UX, UY
        | Inter193 -> 2   // UX, UY
        | Inter194 -> 3   // UX, UY, UZ
        | Inter195 -> 3   // UX, UY, UZ
        | Inter202 -> 2   // UX, UY
        | Inter203 -> 2   // UX, UY
        | Inter204 -> 3   // UX, UY, UZ
        | Inter205 -> 3   // UX, UY, UZ
        // ANSYS LINK
        | Link11   -> 3   // UX, UY, UZ
        | Link31   -> 1   // TEMP
        | Link33   -> 1   // TEMP
        | Link34   -> 1   // TEMP
        | Link68   -> 2   // TEMP, VOLT
        | Link180  -> 3   // UX, UY, UZ
        | Link228  -> 5   // UX, UY, UZ, TEMP, VOLT
        // ANSYS MASS
        | Mass21   -> 6   // UX, UY, UZ, ROTX, ROTY, ROTZ
        | Mass71   -> 1   // TEMP
        // ANSYS MPC
        | Mpc184   -> 6   // UX, UY, UZ, ROTX, ROTY, ROTZ
        // ANSYS PIPE / ELBOW
        | Pipe288  -> 6   // UX, UY, UZ, ROTX, ROTY, ROTZ
        | Pipe289  -> 6   // UX, UY, UZ, ROTX, ROTY, ROTZ
        | Elbow290 -> 6   // UX, UY, UZ, ROTX, ROTY, ROTZ
        // ANSYS PLANE
        | Plane75  -> 1   // TEMP
        | Plane78  -> 1   // TEMP
        | Plane83  -> 3   // UX, UY, UZ

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
