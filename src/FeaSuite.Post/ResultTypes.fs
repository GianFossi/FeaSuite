namespace FeaSuite.Post

open FeaSuite.Core

// ---------------------------------------------------------------------------
// FEA result types
// ---------------------------------------------------------------------------

/// Nodal displacement results.
type NodalDisplacements = {
    /// Map from NodeId to displacement vector (one entry per active DOF).
    Values : Map<NodeId, float[]>
}

/// Nodal reaction force results (at constrained DOFs).
type NodalReactions = {
    /// Map from (NodeId, localDofIndex) to reaction force/moment.
    Values : Map<NodeId * int, float>
}

/// Nodal internal forces or heat flux (one value per active DOF).
type NodalForces = {
    /// Map from NodeId to internal force/flux vector (one entry per active DOF).
    Values : Map<NodeId, float[]>
}

/// Nodal temperatures extracted from thermal degrees of freedom.
type NodalTemperatures = {
    /// Map from NodeId to temperature [K or °C, per model convention].
    Values : Map<NodeId, float>
}

/// Full 3-D Cauchy stress tensor [Pa].
type StressTensor = {
    /// Normal stress in X direction.
    Sxx : float
    /// Normal stress in Y direction.
    Syy : float
    /// Normal stress in Z direction.
    Szz : float
    /// Shear stress on XY plane.
    Sxy : float
    /// Shear stress on YZ plane.
    Syz : float
    /// Shear stress on XZ plane.
    Sxz : float
}

module StressTensor =

    /// Zero stress state.
    let zero : StressTensor = { Sxx=0.0; Syy=0.0; Szz=0.0; Sxy=0.0; Syz=0.0; Sxz=0.0 }

    /// Von Mises equivalent stress [Pa].
    ///   σ_vm = √(½·[(σ_xx−σ_yy)²+(σ_yy−σ_zz)²+(σ_zz−σ_xx)²+6·(τ_xy²+τ_yz²+τ_xz²)])
    let vonMises (s: StressTensor) : float =
        sqrt (0.5 * ((s.Sxx-s.Syy)*(s.Sxx-s.Syy)
                    + (s.Syy-s.Szz)*(s.Syy-s.Szz)
                    + (s.Szz-s.Sxx)*(s.Szz-s.Sxx)
                    + 6.0*(s.Sxy*s.Sxy + s.Syz*s.Syz + s.Sxz*s.Sxz)))

    /// Principal stresses sorted in descending order (σ₁ ≥ σ₂ ≥ σ₃),
    /// computed via the trigonometric solution to the characteristic cubic.
    let principalStresses (s: StressTensor) : float * float * float =
        let I1 = s.Sxx + s.Syy + s.Szz
        let I2 = s.Sxx*s.Syy + s.Syy*s.Szz + s.Szz*s.Sxx
                 - s.Sxy*s.Sxy - s.Syz*s.Syz - s.Sxz*s.Sxz
        let I3 = s.Sxx*(s.Syy*s.Szz - s.Syz*s.Syz)
                 - s.Sxy*(s.Sxy*s.Szz - s.Syz*s.Sxz)
                 + s.Sxz*(s.Sxy*s.Syz - s.Syy*s.Sxz)
        let p  = I1*I1 - 3.0*I2
        if p < 1e-30 then
            let s0 = I1 / 3.0
            s0, s0, s0
        else
            let sqrtp = sqrt p
            let q = (2.0*I1*I1*I1 - 9.0*I1*I2 + 27.0*I3) / (2.0 * sqrtp * sqrtp * sqrtp)
            let q = max -1.0 (min 1.0 q)
            let phi = acos q / 3.0
            let s1 = (I1 + 2.0*sqrtp*(cos phi)) / 3.0
            let s2 = (I1 - sqrtp*(cos phi + sqrt 3.0 * sin phi)) / 3.0
            let s3 = (I1 - sqrtp*(cos phi - sqrt 3.0 * sin phi)) / 3.0
            let arr = [| s1; s2; s3 |] |> Array.sortDescending
            arr.[0], arr.[1], arr.[2]

    /// Tresca equivalent stress (maximum shear stress criterion): σ_T = σ₁ − σ₃  [Pa].
    let tresca (s: StressTensor) : float =
        let s1, _, s3 = principalStresses s
        s1 - s3

/// Full 3-D engineering strain tensor.
/// Shear components use engineering convention: Exy = γ_xy = 2·ε_xy  [-].
type StrainTensor = {
    /// Normal strain in X direction.
    Exx : float
    /// Normal strain in Y direction.
    Eyy : float
    /// Normal strain in Z direction.
    Ezz : float
    /// Engineering shear strain XY (γ_xy = 2·ε_xy).
    Exy : float
    /// Engineering shear strain YZ (γ_yz = 2·ε_yz).
    Eyz : float
    /// Engineering shear strain XZ (γ_xz = 2·ε_xz).
    Exz : float
}

module StrainTensor =

    /// Zero strain state.
    let zero : StrainTensor = { Exx=0.0; Eyy=0.0; Ezz=0.0; Exy=0.0; Eyz=0.0; Exz=0.0 }

    /// Von Mises (equivalent) strain [-].
    ///   ε_eq = √(⅔·[ε_xx²+ε_yy²+ε_zz²+½·(γ_xy²+γ_yz²+γ_xz²)])
    let vonMisesEquivalent (e: StrainTensor) : float =
        sqrt (2.0/3.0 * (e.Exx*e.Exx + e.Eyy*e.Eyy + e.Ezz*e.Ezz
                          + 0.5*(e.Exy*e.Exy + e.Eyz*e.Eyz + e.Exz*e.Exz)))

/// Per-element stress and strain result including failure indicators.
type ElementStressStrainResult = {
    ElementId : ElementId
    /// Full Cauchy stress tensor [Pa].
    Stress : StressTensor
    /// Full engineering strain tensor [-].
    Strain : StrainTensor
    /// Von Mises equivalent stress [Pa].
    VonMisesStress : float
    /// Tresca equivalent stress (σ₁ − σ₃) [Pa].
    TrescaStress : float
    /// Von Mises equivalent strain [-].
    EquivalentStrain : float
    /// True when Von Mises stress exceeds the material yield stress.
    /// Always false when no yield stress has been supplied.
    IsPlastic : bool
    /// Accumulated equivalent plastic strain [-].
    /// Estimated as max(ε_eq − σ_vm/E, 0) when plastic; 0 otherwise.
    CumulativeStrain : float
}

/// Per-element stress / strain result for 1-D elements (legacy).
type ElementResult1D = {
    ElementId  : ElementId
    AxialForce : float   // N
    AxialStress : float  // Pa = N/A
    AxialStrain : float  // ε = σ/E
}

/// Container for all post-processing results.
type FEAResults = {
    Displacements        : NodalDisplacements
    Temperatures         : NodalTemperatures
    NodalForces          : NodalForces
    Reactions            : NodalReactions
    /// Legacy 1-D bar element results (kept for backward compatibility).
    ElementResults       : ElementResult1D list
    /// General per-element stress, strain, and failure results.
    ElementStressStrains : ElementStressStrainResult list
}
