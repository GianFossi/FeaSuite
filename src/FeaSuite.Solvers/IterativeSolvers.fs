namespace FeaSuite.Solvers

open System
open FeaSuite.Core

// ---------------------------------------------------------------------------
// Iterative linear solvers: CG and BiCGSTAB
//
// Both solvers implement ILinearSolver and work with any IAssembledSystem:
//   - If the system also implements ISparseAssembledSystem the CSR
//     representation is used for efficient sparse matrix-vector products.
//   - Otherwise a dense SpMV is used (adds a CSR conversion step).
//
// SolverDiagnostics is available to callers who cast the solver to
// IIterativeSolverDiagnostics after a solve.
// ---------------------------------------------------------------------------

/// Convergence diagnostics returned by iterative solvers.
type SolverDiagnostics = {
    /// Number of iterations performed.
    Iterations   : int
    /// Euclidean norm of the final residual ||r||₂.
    ResidualNorm : float
    /// True if the solver reached the requested tolerance.
    Converged    : bool
}

/// Augmented interface that exposes solver diagnostics from the last solve.
type IIterativeSolverDiagnostics =
    inherit ILinearSolver
    /// Diagnostics recorded during the most recent call to Solve.
    abstract LastDiagnostics : SolverDiagnostics option


// ---------------------------------------------------------------------------
// Helper: build a working CSR + F from an IAssembledSystem with BCs applied
// ---------------------------------------------------------------------------

module private IterativeSolverHelpers =

    /// Return a CSR matrix (copy of values) and RHS vector with BCs applied.
    let buildCsrAndRhs
            (system : IAssembledSystem)
            (bcs    : BoundaryCondition list)
            (dofMap : DofMap)
            : CsrMatrix * float[] =
        let F = Array.copy system.F
        match system with
        | :? ISparseAssembledSystem as sparse ->
            // Reuse the CSR structure; copy values so the original is untouched
            let kCsr = sparse.KCsr
            let A = { kCsr with Values = Array.copy kCsr.Values }
            CsrMatrix.applyBCs A F bcs dofMap
            A, F
        | _ ->
            // Dense fallback: copy K, apply BCs, convert to CSR
            let K = DenseMatrix.copy system.K
            let n = system.TotalDofs
            // Inline BC elimination on dense K (same logic as BcElimination.apply)
            for bc in bcs do
                match dofMap.TryFind (bc.NodeId, bc.LocalDofIndex) with
                | None -> ()
                | Some gdof ->
                    let pv = match bc.Constraint with Fixed -> 0.0 | Prescribed v -> v
                    for row in 0 .. n - 1 do
                        if row <> gdof then
                            F.[row] <- F.[row] - K.[row, gdof] * pv
                            K.[row, gdof] <- 0.0
                            K.[gdof, row] <- 0.0
                    K.[gdof, gdof] <- 1.0
                    F.[gdof]       <- pv
            CsrMatrix.ofDense K, F

    /// Euclidean norm of a vector.
    let inline norm2 (v: float[]) =
        v |> Array.sumBy (fun x -> x * x) |> sqrt


// ---------------------------------------------------------------------------
// Conjugate Gradient solver (symmetric positive-definite systems)
// ---------------------------------------------------------------------------

/// Preconditioned Conjugate Gradient (CG) iterative solver.
///
/// Suitable for symmetric positive-definite (SPD) linear systems such as
/// the global stiffness equation after boundary conditions are applied.
/// Uses diagonal (Jacobi) preconditioning by default.
///
/// Parameters:
///   maxIterations – upper bound on CG iterations (default 10 000)
///   tolerance     – convergence criterion ||r||₂ < tol (default 1e-10)
type CgSolver(?maxIterations: int, ?tolerance: float) =

    let maxIter = defaultArg maxIterations 10_000
    let tol     = defaultArg tolerance 1e-10

    let mutable lastDiag : SolverDiagnostics option = None

    interface IIterativeSolverDiagnostics with
        member _.LastDiagnostics = lastDiag

    interface ILinearSolver with
        member self.Solve(system, bcs, dofMap) =
            let n = system.TotalDofs
            let A, F = IterativeSolverHelpers.buildCsrAndRhs system bcs dofMap

            // Diagonal (Jacobi) preconditioner M = diag(A)⁻¹
            let diag  = CsrMatrix.diagonal A
            let minv  = diag |> Array.map (fun d ->
                if abs d > 1e-14 then 1.0 / d else 1.0)

            // Initialise: x=0, r=b, z=M⁻¹r, p=z, ρ=r·z
            let x = Array.zeroCreate n
            let r = Array.copy F          // r = b − A·x = b  (x=0)
            let z = Array.map2 ( * ) minv r
            let p = Array.copy z
            let mutable rz   = Array.map2 ( * ) r z |> Array.sum
            let mutable iter = 0
            let mutable converged = false

            while not converged && iter < maxIter do
                let Ap  = CsrMatrix.mulVec A p
                let pAp = Array.map2 ( * ) p Ap |> Array.sum
                if abs pAp < 1e-30 then
                    // Numerically stalled – accept current iterate
                    converged <- true
                else
                    let alpha = rz / pAp
                    for i in 0 .. n - 1 do
                        x.[i] <- x.[i] + alpha * p.[i]
                        r.[i] <- r.[i] - alpha * Ap.[i]
                    let rNorm = IterativeSolverHelpers.norm2 r
                    if rNorm < tol then
                        converged <- true
                    else
                        let zNew  = Array.map2 ( * ) minv r
                        let rzNew = Array.map2 ( * ) r zNew |> Array.sum
                        let beta  = rzNew / rz
                        for i in 0 .. n - 1 do
                            p.[i] <- zNew.[i] + beta * p.[i]
                        rz   <- rzNew
                        iter <- iter + 1

            let finalNorm = IterativeSolverHelpers.norm2 r
            lastDiag <- Some { Iterations = iter; ResidualNorm = finalNorm; Converged = converged }

            if x |> Array.exists Double.IsNaN then
                Error [ SingularMatrix ]
            else
                Ok x


// ---------------------------------------------------------------------------
// BiCGSTAB solver (general non-symmetric systems)
// ---------------------------------------------------------------------------

/// BiCGSTAB (Bi-Conjugate Gradient Stabilised) iterative solver.
///
/// Converges for general (possibly non-symmetric) linear systems.
/// Uses diagonal (Jacobi) preconditioning.
///
/// Parameters:
///   maxIterations – upper bound on iterations (default 10 000)
///   tolerance     – convergence criterion ||r||₂ < tol (default 1e-10)
type BiCgStabSolver(?maxIterations: int, ?tolerance: float) =

    let maxIter = defaultArg maxIterations 10_000
    let tol     = defaultArg tolerance 1e-10

    let mutable lastDiag : SolverDiagnostics option = None

    interface IIterativeSolverDiagnostics with
        member _.LastDiagnostics = lastDiag

    interface ILinearSolver with
        member self.Solve(system, bcs, dofMap) =
            let n = system.TotalDofs
            let A, F = IterativeSolverHelpers.buildCsrAndRhs system bcs dofMap

            // Diagonal preconditioner
            let diag = CsrMatrix.diagonal A
            let minv = diag |> Array.map (fun d ->
                if abs d > 1e-14 then 1.0 / d else 1.0)

            // Initialise
            let x  = Array.zeroCreate n
            let r  = Array.copy F          // r = b (x=0)
            let r0 = Array.copy r          // shadow residual (fixed)

            let mutable rho   = 1.0
            let mutable alpha = 1.0
            let mutable omega = 1.0
            let v = Array.zeroCreate n
            let p = Array.zeroCreate n

            let mutable iter      = 0
            let mutable converged = false

            while not converged && iter < maxIter do
                let rhoNew = Array.map2 ( * ) r0 r |> Array.sum
                if abs rhoNew < 1e-300 then
                    converged <- true  // breakdown
                else
                    let beta =
                        if abs rho < 1e-300 || abs omega < 1e-300 then 0.0
                        else (rhoNew / rho) * (alpha / omega)

                    // p = r + β·(p − ω·v)
                    for i in 0 .. n - 1 do
                        p.[i] <- r.[i] + beta * (p.[i] - omega * v.[i])

                    // Preconditioned direction: phat = M⁻¹ · p
                    let phat = Array.map2 ( * ) minv p
                    let vNew = CsrMatrix.mulVec A phat
                    let r0v  = Array.map2 ( * ) r0 vNew |> Array.sum
                    alpha <-
                        if abs r0v < 1e-300 then 0.0
                        else rhoNew / r0v

                    // s = r − α·v
                    let s = Array.init n (fun i -> r.[i] - alpha * vNew.[i])

                    let sNorm = IterativeSolverHelpers.norm2 s
                    if sNorm < tol then
                        for i in 0 .. n - 1 do
                            x.[i] <- x.[i] + alpha * phat.[i]
                        converged <- true
                    else
                        let shat = Array.map2 ( * ) minv s
                        let t    = CsrMatrix.mulVec A shat
                        let tt   = Array.map2 ( * ) t t |> Array.sum
                        omega <-
                            if abs tt < 1e-300 then 0.0
                            else (Array.map2 ( * ) t s |> Array.sum) / tt

                        for i in 0 .. n - 1 do
                            x.[i] <- x.[i] + alpha * phat.[i] + omega * shat.[i]
                            r.[i] <- s.[i] - omega * t.[i]

                        rho <- rhoNew
                        let rNorm = IterativeSolverHelpers.norm2 r
                        if rNorm < tol then
                            converged <- true

                iter <- iter + 1

            let finalNorm = IterativeSolverHelpers.norm2 r
            lastDiag <- Some { Iterations = iter; ResidualNorm = finalNorm; Converged = converged }

            if x |> Array.exists Double.IsNaN then
                Error [ SingularMatrix ]
            else
                Ok x
