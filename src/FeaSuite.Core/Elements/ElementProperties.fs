namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Typed section / geometry properties for structural elements.
// ---------------------------------------------------------------------------

/// Section properties for 1-D bar and 3-D truss elements.
type BarSectionProperties = {
    /// Cross-section area A [m²]
    Area                : float
    /// Additional non-structural mass per unit length [kg/m] (e.g. insulation, fluid).
    /// Added on top of ρ·A when assembling the mass matrix. None = structural mass only.
    AddedMassPerLength  : float option
}

/// Section properties for 2-D Euler-Bernoulli beam elements.
/// DOF order per node: UX, UY, ROTZ.
type Beam2DSectionProperties = {
    /// Cross-section area A [m²]
    Area                : float
    /// Second moment of area about the local z-axis [m⁴]
    Iz                  : float
    /// Additional non-structural mass per unit length [kg/m].
    /// Added on top of ρ·A when assembling the mass matrix. None = structural mass only.
    AddedMassPerLength  : float option
}

/// Section properties for 3-D Euler-Bernoulli beam elements.
/// DOF order per node: UX, UY, UZ, ROTX, ROTY, ROTZ.
type Beam3DSectionProperties = {
    /// Cross-section area A [m²]
    Area                : float
    /// Second moment of area about the local z-axis (bending in local XY plane) [m⁴]
    Iz                  : float
    /// Second moment of area about the local y-axis (bending in local XZ plane) [m⁴]
    Iy                  : float
    /// St. Venant torsional constant [m⁴]
    J                   : float
    /// Additional non-structural mass per unit length [kg/m].
    /// Added on top of ρ·A when assembling the mass matrix. None = structural mass only.
    AddedMassPerLength  : float option
}

/// Section properties for shell elements.
type ShellSectionProperties = {
    /// Shell thickness [m]
    Thickness : float
}

// ---------------------------------------------------------------------------
// Mass element properties
// ---------------------------------------------------------------------------

/// Concentrated structural mass (Mass21 — 6 DOFs: UX UY UZ ROTX ROTY ROTZ).
/// Each component can differ (e.g. asymmetric mass distribution).
type StructuralMassProperties = {
    /// Translational mass in X [kg]
    Mx  : float
    /// Translational mass in Y [kg]
    My  : float
    /// Translational mass in Z [kg]
    Mz  : float
    /// Mass moment of inertia about X [kg·m²]
    Ixx : float
    /// Mass moment of inertia about Y [kg·m²]
    Iyy : float
    /// Mass moment of inertia about Z [kg·m²]
    Izz : float
}

module StructuralMassProperties =
    /// Isotropic point mass: equal translational mass M in all directions, zero rotational inertia.
    let isotropic (m: float) : StructuralMassProperties =
        { Mx = m; My = m; Mz = m; Ixx = 0.0; Iyy = 0.0; Izz = 0.0 }

/// Concentrated thermal mass (Mass71 — 1 DOF: TEMP).
type ThermalMassProperties = {
    /// Thermal capacitance C [J/K]
    Capacitance : float
}

// ---------------------------------------------------------------------------
// Link element properties
// ---------------------------------------------------------------------------

/// Section / physical properties for link elements.
/// Use only the fields relevant to the specific link type; leave others as None.
///
/// Guidance by type:
///   Link11  (actuator)    – Area, SpringStiffness
///   Link33  (conduction)  – Area (conductivity from material)
///   Link34  (convection)  – FilmCoefficient, Area
///   Link180 (truss spar)  – use BarSection instead
///   Link31/Link68/Link228 – leave as NoProperties (radiation / coupled-field physics
///                           require dedicated solver support)
type LinkSectionProperties = {
    /// Cross-section area A [m²] — for structural and conduction links.
    Area            : float option
    /// Linear spring / actuator stiffness K [N/m] — for Link11.
    SpringStiffness : float option
    /// Convection film coefficient h [W/(m²·K)] — for Link34.
    FilmCoefficient : float option
}

// ---------------------------------------------------------------------------
// Top-level union
// ---------------------------------------------------------------------------

/// Typed element properties. Each element carries exactly the section data
/// appropriate for its type; the assembler pattern-matches on this union.
type ElementProperties =
    /// Elements with no geometry-specific section data
    /// (radiation links, coupled-field elements, MPC, infinite boundaries, etc.).
    | NoProperties
    /// 1-D bar or 3-D truss section.
    | BarSection          of BarSectionProperties
    /// 2-D Euler-Bernoulli beam section.
    | Beam2DSection       of Beam2DSectionProperties
    /// 3-D Euler-Bernoulli beam section.
    | Beam3DSection       of Beam3DSectionProperties
    /// Shell element section.
    | ShellSection        of ShellSectionProperties
    /// Concentrated structural mass (Mass21).
    | StructuralMassSection of StructuralMassProperties
    /// Concentrated thermal mass (Mass71).
    | ThermalMassSection    of ThermalMassProperties
    /// Link element properties (Link11, Link33, Link34, …).
    | LinkSection           of LinkSectionProperties
