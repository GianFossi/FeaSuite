namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Typed section / geometry properties for structural elements.
// ---------------------------------------------------------------------------

/// Section properties for 1-D bar and 3-D truss elements.
type BarSectionProperties = {
    /// Cross-section area A [m²]
    Area : float
}

/// Section properties for 2-D Euler-Bernoulli beam elements.
/// DOF order per node: UX, UY, ROTZ.
type Beam2DSectionProperties = {
    /// Cross-section area A [m²]
    Area : float
    /// Second moment of area about the local z-axis [m⁴]
    Iz   : float
}

/// Section properties for 3-D Euler-Bernoulli beam elements.
/// DOF order per node: UX, UY, UZ, ROTX, ROTY, ROTZ.
type Beam3DSectionProperties = {
    /// Cross-section area A [m²]
    Area : float
    /// Second moment of area about the local z-axis (bending in local XY plane) [m⁴]
    Iz   : float
    /// Second moment of area about the local y-axis (bending in local XZ plane) [m⁴]
    Iy   : float
    /// St. Venant torsional constant [m⁴]
    J    : float
}

/// Section properties for shell elements.
type ShellSectionProperties = {
    /// Shell thickness [m]
    Thickness : float
}

/// Typed element properties. Each element carries exactly the section data
/// appropriate for its type; the assembler pattern-matches on this union.
type ElementProperties =
    /// Elements with no geometry-specific section data
    /// (link, mass, fluid, special elements whose section is fully in the material).
    | NoProperties
    /// 1-D bar or 3-D truss section.
    | BarSection     of BarSectionProperties
    /// 2-D Euler-Bernoulli beam section.
    | Beam2DSection  of Beam2DSectionProperties
    /// 3-D Euler-Bernoulli beam section.
    | Beam3DSection  of Beam3DSectionProperties
    /// Shell element section.
    | ShellSection   of ShellSectionProperties
