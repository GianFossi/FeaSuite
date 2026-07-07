namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Shell element sub-types
// ---------------------------------------------------------------------------

/// Shell element sub-types.
type ShellElement =
    | Shell4  /// 4-node shell; 6 DOFs/node (UX, UY, UZ, ROTX, ROTY, ROTZ) – reserved for future implementation
    | Shell61 /// ANSYS SHELL61  – 2D Axisymmetric-Harmonic Structural Shell; 2 nodes; DOF: UX, UY, UZ, ROTZ
