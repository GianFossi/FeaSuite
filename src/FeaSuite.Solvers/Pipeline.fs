namespace FeaSuite.Solvers

open FeaSuite.Core

// ---------------------------------------------------------------------------
// End-to-end FEA pipeline
// Validate → Assemble → Solve → Recover results
// ---------------------------------------------------------------------------

/// Selects the linear solver used for direct (non-nonlinear) analyses.
type LinearSolverKind =
    /// Built-in Gaussian elimination on the dense K matrix (default).
    /// Best for small-to-medium models (up to ~5 000 DOFs).
    | Dense
    /// Pure F# Conjugate Gradient with Jacobi preconditioner.
    /// Requires a symmetric positive-definite system; efficient for large
    /// sparse models when combined with SparseAssembler.
    | SparseCg of maxIterations: int * tolerance: float
    /// Pure F# BiCGSTAB with Jacobi preconditioner.
    /// Handles general (non-symmetric) systems; efficient for large sparse
    /// models when combined with SparseAssembler.
    | SparseBiCgStab of maxIterations: int * tolerance: float

module LinearSolverKind =
    /// Default CG settings (10 000 iterations, tolerance 1e-10).
    let defaultCg         = SparseCg (10_000, 1e-10)
    /// Default BiCGSTAB settings (10 000 iterations, tolerance 1e-10).
    let defaultBiCgStab   = SparseBiCgStab (10_000, 1e-10)

/// Input for a single-load-case solve.
type SolveInput = {
    Model               : FEAModel
    /// Index into Model.LoadCases.
    LoadCaseIndex       : int
    UseNonlinear        : bool
    NonlinearConfig     : NonlinearConfig
    /// Linear solver method (ignored when UseNonlinear = true).
    /// Defaults to Dense (Gaussian elimination).
    LinearSolverKind    : LinearSolverKind
    /// When true, uses SparseAssembler (PagedMatrixStore-backed CSR assembly)
    /// instead of the default DenseAssembler.
    UseSparseAssembler  : bool
}

module SolveInput =
    /// Default SolveInput values – callers can copy-and-update with { ... with ... }.
    let defaults = {
        Model              = FEAModel.empty
        LoadCaseIndex      = 0
        UseNonlinear       = false
        NonlinearConfig    = NonlinearConfig.defaults
        LinearSolverKind   = Dense
        UseSparseAssembler = false
    }

/// Result of a successful solve.
type SolveOutput = {
    Displacements : Map<NodeId, float[]>
    Reactions     : Map<NodeId * int, float>
    RawVector     : float[]
    TotalDofs     : int
}

module FeaPipeline =

    let private nlSolver = NewtonRaphsonSolver() :> INonlinearSolver

    /// Instantiate the appropriate linear solver for the given kind.
    let private makeLinearSolver (kind: LinearSolverKind) : ILinearSolver =
        match kind with
        | Dense                          -> DenseLinearSolver()      :> ILinearSolver
        | SparseCg (maxIter, tol)        -> CgSolver(maxIter, tol)      :> ILinearSolver
        | SparseBiCgStab (maxIter, tol)  -> BiCgStabSolver(maxIter, tol) :> ILinearSolver

    /// Run the full FEA pipeline for the given input.
    let run (input: SolveInput) : Validation<SolveOutput> =
        // 1. Validate model
        match ModelValidation.validate input.Model with
        | Error e -> Error e
        | Ok model ->

        // 2. Pick load case
        let loadCases = model.LoadCases
        if input.LoadCaseIndex < 0 || input.LoadCaseIndex >= List.length loadCases then
            Validation.fail (InvalidInput "LoadCaseIndex out of range.")
        else

        let loadCase = loadCases.[input.LoadCaseIndex]
        let dofMap, totalDofs = FEAModel.buildDofMap model

        // 3. Choose assembler
        let assembler : IAssembler =
            if input.UseSparseAssembler then SparseAssembler() :> IAssembler
            else                             DenseAssembler()  :> IAssembler

        // 4. Assemble (original system kept for reaction recovery)
        match assembler.Assemble(model, loadCase) with
        | Error e -> Error e
        | Ok system ->

        // 5. Solve
        let solveResult =
            if input.UseNonlinear then
                // Newton-Raphson always uses the dense assembler/solver internally
                nlSolver.Solve(DenseAssembler(), model, loadCase, input.NonlinearConfig)
            else
                let solver = makeLinearSolver input.LinearSolverKind
                solver.Solve(system, loadCase.BoundaryConditions, dofMap)

        match solveResult with
        | Error e -> Error e
        | Ok uVec ->

        // 6. Recover displacements
        let displacements =
            model.Nodes
            |> Map.map (fun nid node ->
                [| for d in 0 .. node.DegreesOfFreedom - 1 do
                       let gdof = dofMap.[(nid, d)]
                       yield uVec.[gdof] |])

        // 7. Recover reactions R = K·u − F (at constrained DOFs)
        // Use CSR SpMV when a sparse system is available, otherwise dense.
        let ku =
            match system with
            | :? ISparseAssembledSystem as sparse ->
                CsrMatrix.mulVec sparse.KCsr uVec
            | _ ->
                DenseMatrix.mulVec system.K uVec

        let reactions =
            [ for bc in loadCase.BoundaryConditions do
                  match dofMap.TryFind (bc.NodeId, bc.LocalDofIndex) with
                  | Some gdof ->
                      let reaction = ku.[gdof] - system.F.[gdof]
                      yield (bc.NodeId, bc.LocalDofIndex), reaction
                  | None -> () ]
            |> Map.ofList

        Ok {
            Displacements = displacements
            Reactions     = reactions
            RawVector     = uVec
            TotalDofs     = totalDofs
        }
