namespace FeaSuite.Solvers

open FeaSuite.Core

// ---------------------------------------------------------------------------
// Linear solver: Gaussian elimination with partial pivoting
// Applies boundary conditions by the DOF-elimination method.
// ---------------------------------------------------------------------------

module GaussianElimination =

    /// Solve A·x = b in-place using Gaussian elimination with partial pivoting.
    /// Returns Ok x or Error SingularMatrix.
    let solve (A: float[,]) (b: float[]) : Validation<float[]> =
        let n = Array2D.length1 A
        let a = DenseMatrix.copy A         // work on a copy
        let x = Array.copy b               // RHS copy

        for col in 0 .. n - 1 do
            // Partial pivot: find row with largest |a[row, col]|
            let mutable pivotRow = col
            let mutable pivotVal = abs a.[col, col]
            for row in col + 1 .. n - 1 do
                if abs a.[row, col] > pivotVal then
                    pivotRow <- row
                    pivotVal <- abs a.[row, col]

            // Swap rows
            if pivotRow <> col then
                for k in 0 .. n - 1 do
                    let tmp = a.[col, k]
                    a.[col, k]      <- a.[pivotRow, k]
                    a.[pivotRow, k] <- tmp
                let tmp = x.[col]
                x.[col]      <- x.[pivotRow]
                x.[pivotRow] <- tmp

            if abs a.[col, col] < 1e-14 then
                () // singular – will propagate NaN; checked after
            else
                // Eliminate below
                for row in col + 1 .. n - 1 do
                    let factor = a.[row, col] / a.[col, col]
                    for k in col .. n - 1 do
                        a.[row, k] <- a.[row, k] - factor * a.[col, k]
                    x.[row] <- x.[row] - factor * x.[col]

        // Back-substitution
        let result = Array.zeroCreate n
        for i in n - 1 .. -1 .. 0 do
            let mutable s = x.[i]
            for j in i + 1 .. n - 1 do
                s <- s - a.[i, j] * result.[j]
            if abs a.[i, i] < 1e-14 then
                result.[i] <- System.Double.NaN
            else
                result.[i] <- s / a.[i, i]

        if result |> Array.exists System.Double.IsNaN then
            Error [ SingularMatrix ]
        else
            Ok result

/// Linear solver that applies BCs by elimination and calls Gaussian elimination.
type DenseLinearSolver() =
    interface ILinearSolver with
        member _.Solve(system, bcs, dofMap) =
            let totalDofs = system.TotalDofs
            let K = DenseMatrix.copy system.K
            let F = Array.copy system.F

            // --- Apply boundary conditions by elimination ---
            // For each constrained DOF: zero out the row/col and set diagonal = 1, RHS = prescribed value.
            for bc in bcs do
                match dofMap.TryFind (bc.NodeId, bc.LocalDofIndex) with
                | None -> ()
                | Some gdof ->
                    let prescribed =
                        match bc.Constraint with
                        | Fixed            -> 0.0
                        | Prescribed value -> value

                    // Adjust RHS for off-diagonal columns before zeroing
                    for row in 0 .. totalDofs - 1 do
                        if row <> gdof then
                            F.[row] <- F.[row] - K.[row, gdof] * prescribed
                            K.[row, gdof] <- 0.0
                            K.[gdof, row] <- 0.0

                    K.[gdof, gdof] <- 1.0
                    F.[gdof]       <- prescribed

            GaussianElimination.solve K F
