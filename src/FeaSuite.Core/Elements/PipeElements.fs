namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Pipe / elbow element sub-types
// ---------------------------------------------------------------------------

/// Pipe and elbow element sub-types.
type PipeElement =
    | Pipe288  /// ANSYS PIPE288  – 3D 2-Node Pipe; DOF: UX, UY, UZ, ROTX, ROTY, ROTZ
    | Pipe289  /// ANSYS PIPE289  – 3D 3-Node Pipe; DOF: UX, UY, UZ, ROTX, ROTY, ROTZ
    | Elbow290 /// ANSYS ELBOW290 – 3D 3-Node Elbow; DOF: UX, UY, UZ, ROTX, ROTY, ROTZ
