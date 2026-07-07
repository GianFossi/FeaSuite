namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Special / multi-physics element sub-types
// ---------------------------------------------------------------------------

/// Special, solid, and multi-physics element sub-types.
type SpecialElement =
    // --- Solid ---------------------------------------------------------------
    | Solid8   /// 8-node hexahedral solid; 3 DOFs/node (UX, UY, UZ) – reserved for future implementation
    // --- CPT (Coupled Pore-Pressure-Thermal Mechanical) ----------------------
    | Cpt212   /// ANSYS CPT212 – 2D 4-Node Coupled Pore-Pressure-Thermal Mechanical Solid; DOF: UX, UY, PRES, TEMP
    | Cpt213   /// ANSYS CPT213 – 2D 8-Node Coupled Pore-Pressure-Thermal Mechanical Solid; DOF: UX, UY, PRES, TEMP
    | Cpt215   /// ANSYS CPT215 – 3D 8-Node Coupled Pore-Pressure-Thermal Mechanical Solid; DOF: UX, UY, UZ, PRES, TEMP
    | Cpt216   /// ANSYS CPT216 – 3D 20-Node Coupled Pore-Pressure-Thermal Mechanical Solid; DOF: UX, UY, UZ, PRES, TEMP
    | Cpt217   /// ANSYS CPT217 – 3D 10-Node Coupled Pore-Pressure-Thermal Mechanical Solid; DOF: UX, UY, UZ, PRES, TEMP
    // --- FLUID ---------------------------------------------------------------
    | Fluid29  /// ANSYS FLUID29  – 2D Acoustic Fluid; 4 nodes; DOF: UX, UY, PRES
    | Fluid30  /// ANSYS FLUID30  – 3D Acoustic Fluid; 8 nodes; DOF: UX, UY, UZ, PRES, ENKE
    | Fluid38  /// ANSYS FLUID38  – Dynamic Fluid Coupling; 2 nodes; DOF: UX, UY, UZ
    | Fluid116 /// ANSYS FLUID116 – Coupled Thermal-Fluid Pipe; 2 nodes; DOF: PRES, TEMP
    | Fluid129 /// ANSYS FLUID129 – 2D Infinite Acoustic; 2 or 3 nodes; DOF: PRES
    | Fluid130 /// ANSYS FLUID130 – 3D Infinite Acoustic; 4 or 8 nodes; DOF: PRES
    | Fluid136 /// ANSYS FLUID136 – 3D Squeeze Film Fluid; 4 or 8 nodes; DOF: PRES
    | Fluid138 /// ANSYS FLUID138 – 3D Viscous Fluid Link; 2 nodes; DOF: PRES
    | Fluid139 /// ANSYS FLUID139 – 3D Slide Film Fluid; 2 or 32 nodes; DOF: UX, UY, UZ
    | Fluid218 /// ANSYS FLUID218 – 3D Hydrodynamic Bearing Element; 4 nodes; DOF: UX, UY, UZ, PRES
    | Fluid220 /// ANSYS FLUID220 – 3D Acoustic Fluid; 20 nodes; DOF: UX, UY, UZ, PRES, ENKE, VX, VY, VZ, TEMP
    | Fluid221 /// ANSYS FLUID221 – 3D Acoustic Fluid; 10 nodes; DOF: UX, UY, UZ, PRES, ENKE, VX, VY, VZ, TEMP
    | Fluid243 /// ANSYS FLUID243 – 2D 4-Node Acoustic Fluid; DOF: UX, UY, PRES, ENKE, VX, VY, TEMP
    | Fluid244 /// ANSYS FLUID244 – 2D 8-Node Acoustic Fluid; DOF: UX, UY, PRES, ENKE, VX, VY, TEMP
    // --- FOLLW ---------------------------------------------------------------
    | Follw201 /// ANSYS FOLLW201 – 3D Follower Load; 1 node; DOF: UX, UY, UZ, ROTX, ROTY, ROTZ
    // --- HSFLD ---------------------------------------------------------------
    | Hsfld241 /// ANSYS HSFLD241 – 2D Hydrostatic Fluid; 4 nodes; DOF: UX, UY, HDSP, PRES
    | Hsfld242 /// ANSYS HSFLD242 – 3D Hydrostatic Fluid; 9 nodes; DOF: UX, UY, UZ, HDSP, PRES
    // --- INFIN ---------------------------------------------------------------
    | Infin47  /// ANSYS INFIN47  – 3D Infinite Boundary; 4 nodes; DOF: MAG, TEMP
    | Infin110 /// ANSYS INFIN110 – 2D Infinite Solid; 4 or 8 nodes; DOF: AZ, VOLT, TEMP
    | Infin111 /// ANSYS INFIN111 – 3D Infinite Solid; 8 or 20 nodes; DOF: MAG, AZ, VOLT, TEMP
    | Infin257 /// ANSYS INFIN257 – 2D/3D Structural Infinite Solid; DOF (2D): UX, UY (2/node); DOF (3D): UX, UY, UZ (3/node) – actual count governed by Node.DegreesOfFreedom
    // --- INTER ---------------------------------------------------------------
    | Inter192 /// ANSYS INTER192 – Structural 2D Interface 4-Node Gasket; DOF: UX, UY
    | Inter193 /// ANSYS INTER193 – Structural 2D Interface 6-Node Gasket; DOF: UX, UY
    | Inter194 /// ANSYS INTER194 – Structural 3D Interface 16-Node Gasket; DOF: UX, UY, UZ
    | Inter195 /// ANSYS INTER195 – Structural 3D Interface 8-Node Gasket; DOF: UX, UY, UZ
    | Inter202 /// ANSYS INTER202 – Structural 2D Interface 4-Node Cohesive; DOF: UX, UY
    | Inter203 /// ANSYS INTER203 – Structural 2D Interface 6-Node Cohesive; DOF: UX, UY
    | Inter204 /// ANSYS INTER204 – Structural 3D Interface 16-Node Cohesive; DOF: UX, UY, UZ
    | Inter205 /// ANSYS INTER205 – Structural 3D Interface 8-Node Cohesive; DOF: UX, UY, UZ
    // --- MASS ----------------------------------------------------------------
    | Mass21   /// ANSYS MASS21   – Structural Mass; 1 node; DOF: UX, UY, UZ, ROTX, ROTY, ROTZ
    | Mass71   /// ANSYS MASS71   – Thermal Mass; 1 node; DOF: TEMP
    // --- MPC -----------------------------------------------------------------
    | Mpc184   /// ANSYS MPC184   – Structural Multipoint Constraint; 2 or 3 nodes; DOF: UX, UY, UZ, ROTX, ROTY, ROTZ
