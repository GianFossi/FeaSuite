namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Axisymmetric (harmonic) plane element sub-types
// ---------------------------------------------------------------------------

/// Axisymmetric / axisymmetric-harmonic plane element sub-types.
type AxisymmetricElement =
    | Plane75 /// ANSYS PLANE75 – 2D Axisymmetric-Harmonic 4-Node Thermal Solid; DOF: TEMP
    | Plane78 /// ANSYS PLANE78 – 2D Axisymmetric-Harmonic 8-Node Thermal Solid; DOF: TEMP
    | Plane83 /// ANSYS PLANE83 – 2D Axisymmetric-Harmonic 8-Node Structural Solid; DOF: UX, UY, UZ
