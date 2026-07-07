namespace FeaSuite.Solvers

open System
open FeaSuite.Core
open MathNet.Numerics.LinearAlgebra

// ---------------------------------------------------------------------------
// Modal (eigensolver) analysis – natural frequencies and mode shapes
//
// Solves the generalised eigenvalue problem:
//
//   K · φ_i  =  ω_i² · M · φ_i
//
// where K is the structural stiffness matrix (BCs applied by DOF elimination),
// M is the diagonal lumped mass matrix, ω_i is the i-th angular frequency
// (rad/s), and φ_i is the corresponding mode shape vector.
//
// Solution strategy
// -----------------
// 1.  Validate the model and select the load case (BCs come from here).
// 2.  Assemble K (dense) and diagonal lumped M.
// 3.  Identify free DOFs; extract reduced K_f ∈ ℝ^{n_f×n_f} and M_f ∈ ℝ^{n_f}.
// 4.  Scale: K̃ = D⁻¹ · K_f · D⁻¹,  D = diag(√M_f).
//     This transforms the generalised problem into the standard EVP K̃·y = λ·y.
// 5.  Symmetric EVD of K̃ via MathNet.Numerics Evd (O(n³), dense, exact).
// 6.  Sort eigenvalues ascending; take the requested number of modes.
// 7.  Un-scale: φ_f = D⁻¹ · y  (mode shape in physical DOF space).
// 8.  Reconstruct full mode shape with zeros at constrained DOFs.
// 9.  Convert eigenvalues to frequencies: ω_i = √(max(λ_i, 0)), f_i = ω_i/(2π).
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Domain types
// ---------------------------------------------------------------------------

/// Configuration for a modal analysis run.
type ModalConfig = {
    /// Number of modes to extract, sorted by ascending natural frequency.
    NumberOfModes : int
}

module ModalConfig =
    /// Default: extract the lowest 10 modes.
    let defaults = { NumberOfModes = 10 }

/// A single extracted mode.
type ModalMode = {
    /// Mode index (1-based, sorted by ascending natural frequency).
    Index            : int
    /// Angular frequency ω_i [rad/s].  ω_i = √λ_i.
    AngularFrequency : float
    /// Natural frequency f_i [Hz].  f_i = ω_i / (2π).
    NaturalFrequency : float
    /// Full displacement mode shape (length = totalDofs).
    /// Entries at constrained DOFs are set to zero.
    ModeShape        : float[]
}

/// Result of a modal analysis run.
type ModalOutput = {
    /// Extracted modes sorted by ascending natural frequency.
    Modes     : ModalMode[]
    /// Total DOFs of the model.
    TotalDofs : int
}

// ---------------------------------------------------------------------------
// Interface
// ---------------------------------------------------------------------------

/// Solver for the generalised eigenvalue problem K·φ = ω²·M·φ.
type IModalSolver =
    /// Solve for the lowest N modes.  BoundaryConditions in loadCase
    /// define the constrained DOFs (rigid supports).
    abstract Solve : model: FEAModel * loadCase: LoadCase * config: ModalConfig -> Validation<ModalOutput>

// ---------------------------------------------------------------------------
// Input / pipeline wrapper
// ---------------------------------------------------------------------------

/// Input for a modal analysis run through ModalPipeline.
type ModalSolveInput = {
    Model         : FEAModel
    /// Index into Model.LoadCases; the BCs from this load case constrain the model.
    LoadCaseIndex : int
    Config        : ModalConfig
}

module ModalSolveInput =
    let defaults = {
        Model         = FEAModel.empty
        LoadCaseIndex = 0
        Config        = ModalConfig.defaults
    }

// ---------------------------------------------------------------------------
// Implementation: MathNet-backed symmetric EVD
// ---------------------------------------------------------------------------

/// Modal solver using MathNet.Numerics symmetric eigenvalue decomposition.
///
/// Builds the dense reduced stiffness matrix and performs a direct symmetric
/// EVD (O(n³)).  Suitable for models up to a few thousand free DOFs.
/// MKL acceleration is used automatically when available.
type MathNetModalSolver() =

    interface IModalSolver with
        member _.Solve(model, loadCase, config) =

            let dofMap, totalDofs = FEAModel.buildDofMap model

            // 1. Assemble global stiffness K
            let assembler = DenseAssembler() :> IAssembler
            match assembler.Assemble(model, loadCase) with
            | Error e -> Error e
            | Ok system ->

            // 2. Assemble diagonal lumped mass vector M
            match MassAssembler.assemble model with
            | Error e -> Error e
            | Ok mVec ->

            // 3. Identify constrained (fixed/prescribed) global DOF indices
            let constrainedDofs =
                loadCase.BoundaryConditions
                |> List.choose (fun bc -> dofMap.TryFind (bc.NodeId, bc.LocalDofIndex))
                |> Set.ofList

            // 4. Free DOF array (sorted for deterministic ordering)
            let freeDofs =
                [| for i in 0 .. totalDofs - 1 do
                       if not (constrainedDofs.Contains i) then yield i |]

            let nFree = freeDofs.Length
            if nFree = 0 then
                Validation.fail (InvalidInput "No free DOFs after applying boundary conditions.")
            else

            // 5. Validate mass entries (all must be > 0 for a well-posed EVP)
            let mFree = Array.init nFree (fun i -> mVec.[freeDofs.[i]])
            if mFree |> Array.exists (fun m -> m <= 0.0) then
                Validation.fail (InvalidInput
                    "Lumped mass vector has zero or negative entries. \
                     Ensure all elements have positive density, cross-section area and length.")
            else

            // 6. Extract reduced stiffness K_f
            let K = system.K
            let kFree = Array2D.init nFree nFree (fun i j ->
                K.[freeDofs.[i], freeDofs.[j]])

            // 7. Compute scaling vectors D = sqrt(M_f), D_inv = 1/D
            let dVec    = mFree |> Array.map sqrt
            let dInvVec = dVec  |> Array.map (fun d -> 1.0 / d)

            // 8. Form K̃ = D⁻¹ · K_f · D⁻¹  (symmetric, positive semi-definite)
            let kTilde = Array2D.init nFree nFree (fun i j ->
                kFree.[i, j] * dInvVec.[i] * dInvVec.[j])

            // 9. Symmetric EVD via MathNet
            try
                ignore (NativeProvider.ensureConfigured ())
                let kTildeMat = Matrix<float>.Build.DenseOfArray(kTilde)
                let evd       = kTildeMat.Evd(Symmetricity.Symmetric)

                // 10. Collect (eigenvalue, eigenvector) pairs; sort ascending
                let numModes = min config.NumberOfModes nFree
                let eigenPairs =
                    [| for i in 0 .. nFree - 1 ->
                           let lambda = evd.EigenValues.[i].Real
                           let vec    = evd.EigenVectors.Column(i).ToArray()
                           lambda, vec |]
                    |> Array.sortBy fst
                    |> Array.take numModes

                // 11. Build ModalMode records
                let modes =
                    eigenPairs
                    |> Array.mapi (fun idx (lambda, y) ->
                        // Clamp tiny negative eigenvalues (numerical noise near zero)
                        let lambdaClamped = max 0.0 lambda
                        let omega = sqrt lambdaClamped
                        let freq  = omega / (2.0 * Math.PI)

                        // Un-scale to physical mode shape: φ_f = D⁻¹ · y
                        let phiFree = Array.map2 ( * ) dInvVec y

                        // Reconstruct full mode shape; constrained DOFs → 0
                        let phi = Array.zeroCreate totalDofs
                        for k in 0 .. nFree - 1 do
                            phi.[freeDofs.[k]] <- phiFree.[k]

                        { Index            = idx + 1
                          AngularFrequency = omega
                          NaturalFrequency = freq
                          ModeShape        = phi })

                Ok { Modes = modes; TotalDofs = totalDofs }

            with ex ->
                Error [ SolverError ex.Message ]

// ---------------------------------------------------------------------------
// ModalPipeline – end-to-end convenience entry-point
// ---------------------------------------------------------------------------

module ModalPipeline =

    let private solver = MathNetModalSolver() :> IModalSolver

    /// Run a modal analysis for the given input.
    /// Steps: validate model → pick load case → assemble K and M → EVD.
    let run (input: ModalSolveInput) : Validation<ModalOutput> =
        match ModelValidation.validate input.Model with
        | Error e -> Error e
        | Ok model ->

        let loadCases = model.LoadCases
        if input.LoadCaseIndex < 0 || input.LoadCaseIndex >= List.length loadCases then
            Validation.fail (InvalidInput "LoadCaseIndex out of range.")
        else

        let loadCase = loadCases.[input.LoadCaseIndex]
        solver.Solve(model, loadCase, input.Config)
