namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Beam / truss element sub-types
// ---------------------------------------------------------------------------

/// 1-D, beam, and truss structural element sub-types.
type BeamElement =
    | Bar1D    /// 1-D axial bar; 1 DOF/node (UX)
    | Truss3D  /// 3-D truss spar; 3 DOFs/node (UX, UY, UZ)
    | Beam2D   /// 2-D Euler-Bernoulli beam; 3 DOFs/node (UX, UY, ROTZ)
    | Beam3D   /// 3-D beam; 6 DOFs/node (UX, UY, UZ, ROTX, ROTY, ROTZ)
