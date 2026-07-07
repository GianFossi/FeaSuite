namespace FeaSuite.Solvers

open System
open FeaSuite.Core
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double.Solvers
open MathNet.Numerics.LinearAlgebra.Solvers

// ---------------------------------------------------------------------------
// MathNet.Numerics linear solvers
//
// Provides two ILinearSolver implementations backed by MathNet.Numerics
// (https://numerics.mathdotnet.com), an open-source numerics library with
// optimised BLAS/LAPACK-backed dense and sparse routines.
//
// MathNetDenseLinearSolver
//   Converts the assembled float[,] K to a MathNet DenseMatrix and uses
//   LU decomposition to solve K·u = F.
//   Suitable for small-to-medium systems.
//
// MathNetSparseLinearSolver
//   Converts K to a MathNet SparseMatrix (CSR) and solves using the
//   BiCGSTAB iterative method with MILU0 preconditioner.
//   Better suited for large sparse symmetric positive-definite systems.
// ---------------------------------------------------------------------------

module private NativeProvider =
    let private mutable initialized = false
    let private mutable useMkl = false

    let ensureConfigured () =
        if not initialized then
            initialized <- true
            try
                MathNet.Numerics.Control.UseNativeMKL()
                useMkl <- true
            with _ ->
                // Automatic fallback to managed MathNet backend
                useMkl <- false
        useMkl


/// Apply boundary conditions by the standard DOF-elimination method.
/// Modifies K and F in place.
module private BcElimination =
    let apply (n: int) (K: float[,]) (F: float[])
              (bcs: BoundaryCondition list) (dofMap: DofMap) =
        for bc in bcs do
            match dofMap.TryFind (bc.NodeId, bc.LocalDofIndex) with
            | None -> ()
            | Some gdof ->
                let prescribed =
                    match bc.Constraint with
                    | Fixed         -> 0.0
                    | Prescribed v  -> v
                for row in 0 .. n - 1 do
                    if row <> gdof then
                        F.[row] <- F.[row] - K.[row, gdof] * prescribed
                        K.[row, gdof] <- 0.0
                        K.[gdof, row] <- 0.0
                K.[gdof, gdof] <- 1.0
                F.[gdof]       <- prescribed


/// Linear solver using MathNet.Numerics dense LU decomposition.
type MathNetDenseLinearSolver() =

    interface ILinearSolver with
        member _.Solve(system, bcs, dofMap) =
            let n = system.TotalDofs
            let K = DenseMatrix.copy system.K
            let F = Array.copy system.F

            BcElimination.apply n K F bcs dofMap

            try
                let _ = NativeProvider.ensureConfigured ()
                let A = Matrix<float>.Build.DenseOfArray(K)
                let b = Vector<float>.Build.DenseOfArray(F)
                let x = A.Solve(b)
                if x.Exists(fun v -> Double.IsNaN v) then
                    Error [ SingularMatrix ]
                else
                    Ok (x.ToArray())
            with ex ->
                Error [ SolverError ex.Message ]


/// Linear solver using MathNet.Numerics sparse CSR + BiCGSTAB iterative solver.
/// The MILU0 preconditioner accelerates convergence for SPD systems.
type MathNetSparseLinearSolver(?maxIterations: int, ?tolerance: float) =

    let maxIter = defaultArg maxIterations 10_000
    let tol     = defaultArg tolerance 1e-10

    interface ILinearSolver with
        member _.Solve(system, bcs, dofMap) =
            let n = system.TotalDofs
            let K = DenseMatrix.copy system.K
            let F = Array.copy system.F

            BcElimination.apply n K F bcs dofMap

            try
                let _ = NativeProvider.ensureConfigured ()
                let A = Matrix<float>.Build.SparseOfArray(K)
                let b = Vector<float>.Build.DenseOfArray(F)

                let solver    = BiCgStab()            :> IIterativeSolver<float>
                let precond   = MILU0Preconditioner() :> IPreconditioner<float>
                let stopCount = IterationCountStopCriterion<float>(maxIter) :> IIterationStopCriterion<float>
                let stopResid = ResidualStopCriterion<float>(tol)           :> IIterationStopCriterion<float>

                let x = A.SolveIterative(b, solver, precond, stopCount, stopResid)

                if x.Exists(fun v -> Double.IsNaN v) then
                    Error [ SingularMatrix ]
                else
                    Ok (x.ToArray())
            with ex ->
                Error [ SolverError ex.Message ]
