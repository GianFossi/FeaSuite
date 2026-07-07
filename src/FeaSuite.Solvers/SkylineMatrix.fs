namespace FeaSuite.Solvers

open System
open FeaSuite.Core

// ---------------------------------------------------------------------------
// SkylineMatrix – profile (skyline / variable-bandwidth) storage for
// symmetric matrices arising in FEA.
//
// Storage layout
// --------------
// The matrix is symmetric; only the upper triangle is stored column-by-column.
// For column j the stored entries run from row `profile[j]` to the diagonal j
// (inclusive).  Column j therefore occupies
//
//   height[j] = j - profile[j] + 1
//
// entries in a flat `data` array.  `colStart[j]` gives the index in `data`
// of the first (topmost) entry of column j so that:
//
//   data[ colStart[j] + (i - profile[j]) ]  =  K[i, j]
//   for i in profile[j] .. j
//
// Cholesky factorisation
// ----------------------
// SkylineCholesky.factorize performs an in-place L·Lᵀ Cholesky–Crout
// factorisation directly within the skyline data array, replacing entries
// with the upper-triangle factor Lᵀ.  No fill-in occurs outside the profile.
// SkylineCholesky.solve then solves K·x = b via forward / back substitution.
// ---------------------------------------------------------------------------

/// Profile/skyline sparse matrix for symmetric positive-definite systems.
type SkylineMatrix private (n: int, profile: int[], colStart: int[], data: float[]) =

    // ------------------------------------------------------------------
    // Internal offset helper
    // ------------------------------------------------------------------

    /// Flat index into `data` for entry (i, j) where i ≤ j.
    /// Returns -1 when (i, j) lies outside the stored profile.
    let offsetOf (i: int) (j: int) =
        if i > j || i < profile.[j] then -1
        else colStart.[j] + (i - profile.[j])

    // ------------------------------------------------------------------
    // Properties
    // ------------------------------------------------------------------

    member _.Size     = n
    member _.Profile  = profile
    member _.ColStart = colStart
    member _.Data     = data

    // ------------------------------------------------------------------
    // Element access (symmetric; 0-indexed)
    // ------------------------------------------------------------------

    member _.Item
        with get(i: int, j: int) : float =
            if i < 0 || i >= n || j < 0 || j >= n then
                raise (IndexOutOfRangeException(sprintf "SkylineMatrix index (%d,%d) out of range." i j))
            let ii, jj = if i <= j then i, j else j, i
            let off = offsetOf ii jj
            if off < 0 then 0.0 else data.[off]
        and set (i: int, j: int) (v: float) =
            if i < 0 || i >= n || j < 0 || j >= n then
                raise (IndexOutOfRangeException(sprintf "SkylineMatrix index (%d,%d) out of range." i j))
            let ii, jj = if i <= j then i, j else j, i
            let off = offsetOf ii jj
            if off >= 0 then data.[off] <- v
            // entries outside the profile are structural zeros – silently ignore

    /// Accumulate v into entry (i, j) = K[i,j] + v (and the symmetric K[j,i]).
    member self.Add(i: int, j: int, v: float) =
        if i < 0 || i >= n || j < 0 || j >= n then
            raise (IndexOutOfRangeException(sprintf "SkylineMatrix index (%d,%d) out of range." i j))
        let ii, jj = if i <= j then i, j else j, i
        let off = offsetOf ii jj
        if off >= 0 then data.[off] <- data.[off] + v

    // ------------------------------------------------------------------
    // Factory methods
    // ------------------------------------------------------------------

    /// Create a zero skyline matrix of size n with the given column profile.
    /// profile[j] = first non-zero row in column j  (0 ≤ profile[j] ≤ j).
    static member Create(n: int, profile: int[]) =
        if n <= 0 then invalidArg "n" "Size must be positive."
        if profile.Length <> n then invalidArg "profile" "Profile length must equal n."
        let cs = Array.zeroCreate (n + 1)
        for j in 0 .. n - 1 do
            cs.[j + 1] <- cs.[j] + (j - profile.[j] + 1)
        SkylineMatrix(n, profile, cs, Array.zeroCreate cs.[n])

    /// Build a SkylineMatrix from a dense float[,] by auto-detecting the
    /// sparsity profile and copying only upper-triangle entries.
    static member FromDense(K: float[,]) =
        let n = Array2D.length1 K
        let profile =
            Array.init n (fun j ->
                let mutable first = j
                for i in 0 .. j - 1 do
                    if (abs K.[i, j] > 1e-14 || abs K.[j, i] > 1e-14) && i < first then
                        first <- i
                first)
        let sky = SkylineMatrix.Create(n, profile)
        for j in 0 .. n - 1 do
            for i in profile.[j] .. j do
                sky.[i, j] <- K.[i, j]
        sky

    /// Materialise the full symmetric dense matrix (for testing / diagnostics).
    member self.ToDense() =
        let A = Array2D.zeroCreate<float> n n
        for j in 0 .. n - 1 do
            for i in profile.[j] .. j do
                let v = self.[i, j]
                A.[i, j] <- v
                if i <> j then A.[j, i] <- v
        A

// ---------------------------------------------------------------------------
// Cholesky factorisation and solve for SkylineMatrix
// ---------------------------------------------------------------------------

module SkylineCholesky =

    // Build colStart once, inline, from the sky object.
    let private makeColStart (sky: SkylineMatrix) =
        let n  = sky.Size
        let p  = sky.Profile
        let cs = Array.zeroCreate (n + 1)
        for j in 0 .. n - 1 do
            cs.[j + 1] <- cs.[j] + (j - p.[j] + 1)
        cs

    /// In-place L·Lᵀ Cholesky (Crout) factorisation of a symmetric positive-
    /// definite skyline matrix.  The upper factor Lᵀ is stored back into the
    /// same data array (overwriting the original values).
    ///
    /// Returns Ok sky on success or Error [SingularMatrix] if a non-positive
    /// diagonal pivot is encountered (matrix not SPD or near-singular).
    let factorize (sky: SkylineMatrix) : Validation<SkylineMatrix> =
        let n    = sky.Size
        let p    = sky.Profile
        let d    = sky.Data
        let cs   = sky.ColStart

        let inline off i j =
            if i < p.[j] then -1
            else cs.[j] + (i - p.[j])

        let inline get i j =
            let o = off i j
            if o < 0 then 0.0 else d.[o]

        let inline set i j v =
            let o = off i j
            if o >= 0 then d.[o] <- v

        let mutable singular = false

        for j in 0 .. n - 1 do
            if not singular then
                // Update off-diagonal entries in column j
                for i in p.[j] .. j - 1 do
                    let kStart = max p.[i] p.[j]
                    let mutable s = get i j
                    for k in kStart .. i - 1 do
                        s <- s - (get k i) * (get k j)
                    let lii = get i i
                    set i j (if abs lii > 1e-14 then s / lii else 0.0)

                // Update diagonal
                let mutable diag = get j j
                for k in p.[j] .. j - 1 do
                    let lkj = get k j
                    diag <- diag - lkj * lkj

                if diag <= 0.0 then
                    singular <- true
                else
                    set j j (sqrt diag)

        if singular then Error [ SingularMatrix ]
        else Ok sky

    /// Solve K·x = b given the factorised skyline matrix (containing Lᵀ).
    /// Performs forward substitution L·y = b then back substitution Lᵀ·x = y.
    let solve (sky: SkylineMatrix) (b: float[]) : float[] =
        let n  = sky.Size
        let p  = sky.Profile
        let d  = sky.Data
        let cs = sky.ColStart

        let inline off i j =
            if i < p.[j] then -1
            else cs.[j] + (i - p.[j])

        let inline get i j =
            let o = off i j
            if o < 0 then 0.0 else d.[o]

        let y = Array.copy b

        // Forward substitution: L·y = b
        // L[i,j] (lower) = sky[j,i] (upper) for j < i
        for i in 0 .. n - 1 do
            let mutable s = y.[i]
            for j in p.[i] .. i - 1 do
                s <- s - (get j i) * y.[j]
            y.[i] <- s / (get i i)

        // Back substitution: Lᵀ·x = y
        let x = Array.copy y
        for j in n - 1 .. -1 .. 0 do
            x.[j] <- x.[j] / (get j j)
            for i in p.[j] .. j - 1 do
                x.[i] <- x.[i] - (get i j) * x.[j]

        x

// ---------------------------------------------------------------------------
// SkylineLinearSolver – ILinearSolver backed by skyline Cholesky
// ---------------------------------------------------------------------------

/// Linear solver that converts the assembled dense K to skyline profile
/// format, applies boundary conditions by elimination, then solves via
/// in-place skyline Cholesky factorisation.
type SkylineLinearSolver() =
    interface ILinearSolver with
        member _.Solve(system, bcs, dofMap) =
            let n = system.TotalDofs
            let K = DenseMatrix.copy system.K
            let F = Array.copy system.F

            // Apply boundary conditions (same elimination as DenseLinearSolver)
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

            let sky = SkylineMatrix.FromDense K
            match SkylineCholesky.factorize sky with
            | Error e -> Error e
            | Ok fac  -> Ok (SkylineCholesky.solve fac F)
