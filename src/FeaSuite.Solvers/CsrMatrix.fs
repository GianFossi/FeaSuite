namespace FeaSuite.Solvers

open System.Collections.Generic
open FeaSuite.Core

// ---------------------------------------------------------------------------
// CSR (Compressed Sparse Row) matrix representation and helpers
// ---------------------------------------------------------------------------

/// Sparse matrix in Compressed Sparse Row (CSR) format.
/// Non-zero entries for row i are at ColIdx.[RowPtr.[i] .. RowPtr.[i+1]-1].
type CsrMatrix = {
    Rows   : int
    Cols   : int
    /// Length = Rows + 1; RowPtr.[i] is the index in ColIdx/Values of the
    /// first non-zero in row i; RowPtr.[Rows] = total nnz.
    RowPtr : int[]
    /// Column indices of non-zero entries.
    ColIdx : int[]
    /// Non-zero values (same length as ColIdx).
    Values : float[]
}

module CsrMatrix =

    /// Number of stored non-zeros.
    let nnz (A: CsrMatrix) = A.Values.Length

    // -----------------------------------------------------------------------
    // Construction
    // -----------------------------------------------------------------------

    /// Build a CSR matrix from an array of (row, col, value) triplets.
    /// Triplets must be sorted by (row, col); duplicate (row,col) pairs are
    /// summed into a single entry.
    let ofSortedTriplets (rows: int) (cols: int)
                         (triplets: struct(int * int * float)[]) : CsrMatrix =
        let n = rows
        let rowPtr = Array.zeroCreate (n + 1)
        for struct(r, _, _) in triplets do
            rowPtr.[r + 1] <- rowPtr.[r + 1] + 1
        for i in 1 .. n do
            rowPtr.[i] <- rowPtr.[i] + rowPtr.[i - 1]
        let colIdx = Array.zeroCreate triplets.Length
        let values = Array.zeroCreate triplets.Length
        let pos    = Array.copy rowPtr
        for struct(r, c, v) in triplets do
            let k = pos.[r]
            colIdx.[k] <- c
            values.[k] <- v
            pos.[r]    <- pos.[r] + 1
        { Rows   = rows
          Cols   = cols
          RowPtr = rowPtr
          ColIdx = colIdx
          Values = values }

    /// Build CSR from an (unsorted) sequence of COO triplets.
    /// Duplicate (row,col) pairs are summed.
    let ofCooEntries (rows: int) (cols: int)
                     (entries: (int * int * float) seq) : CsrMatrix =
        // Accumulate into a dictionary to sum duplicates
        let dict = Dictionary<struct(int * int), float>()
        for (r, c, v) in entries do
            let key = struct(r, c)
            match dict.TryGetValue key with
            | true, existing -> dict.[key] <- existing + v
            | _              -> dict.[key] <- v
        // Sort by (row, col)
        let sorted =
            dict
            |> Seq.map (fun kv ->
                let struct(r, c) = kv.Key
                struct(r, c, kv.Value))
            |> Seq.sortBy (fun struct(r, c, _) -> struct(r, c))
            |> Array.ofSeq
        ofSortedTriplets rows cols sorted

    /// Build a CSR matrix from a dense float[,] array (skips exact zeros).
    let ofDense (A: float[,]) : CsrMatrix =
        let n = Array2D.length1 A
        let m = Array2D.length2 A
        let triplets =
            [| for i in 0 .. n - 1 do
                   for j in 0 .. m - 1 do
                       if A.[i, j] <> 0.0 then
                           yield struct(i, j, A.[i, j]) |]
        ofSortedTriplets n m triplets

    // -----------------------------------------------------------------------
    // Algebra
    // -----------------------------------------------------------------------

    /// Sparse matrix-vector product: y = A · x.
    let mulVec (A: CsrMatrix) (x: float[]) : float[] =
        let y = Array.zeroCreate A.Rows
        for i in 0 .. A.Rows - 1 do
            let mutable s = 0.0
            for k in A.RowPtr.[i] .. A.RowPtr.[i + 1] - 1 do
                s <- s + A.Values.[k] * x.[A.ColIdx.[k]]
            y.[i] <- s
        y

    /// Return the main diagonal of A (one entry per row; sums any duplicates).
    let diagonal (A: CsrMatrix) : float[] =
        let d = Array.zeroCreate A.Rows
        for i in 0 .. A.Rows - 1 do
            for k in A.RowPtr.[i] .. A.RowPtr.[i + 1] - 1 do
                if A.ColIdx.[k] = i then
                    d.[i] <- d.[i] + A.Values.[k]
        d

    // -----------------------------------------------------------------------
    // Boundary condition elimination (in-place)
    // -----------------------------------------------------------------------

    /// Apply boundary conditions by DOF elimination to a *mutable copy* of the
    /// CSR matrix and the RHS vector F (both are modified in-place).
    ///
    /// For each constrained DOF `gdof` with prescribed displacement `v`:
    ///   1. Exploit symmetry: for each off-diagonal entry K[gdof,j], subtract
    ///      K[gdof,j]*v from F[j]  (equivalent to column elimination on symmetric K).
    ///   2. Zero row `gdof`; set diagonal K[gdof,gdof] = 1; F[gdof] = v.
    ///   3. Zero column `gdof` in all non-constrained rows.
    ///
    /// The caller is responsible for passing a copy of Values so that the
    /// original assembled CSR is preserved for reaction recovery.
    let applyBCs (A: CsrMatrix) (F: float[])
                 (bcs: BoundaryCondition list) (dofMap: DofMap) : unit =

        let constrained = HashSet<int>()
        let prescribed  = Dictionary<int, float>()
        for bc in bcs do
            match dofMap.TryFind (bc.NodeId, bc.LocalDofIndex) with
            | None -> ()
            | Some gdof ->
                let v = match bc.Constraint with Fixed -> 0.0 | Prescribed v -> v
                constrained.Add(gdof) |> ignore
                prescribed.[gdof] <- v

        // Phase 1: update RHS using symmetry (K is symmetric → K[gdof,j]=K[j,gdof])
        for KeyValue(gdof, pv) in prescribed do
            for k in A.RowPtr.[gdof] .. A.RowPtr.[gdof + 1] - 1 do
                let j = A.ColIdx.[k]
                if not (constrained.Contains j) then
                    F.[j] <- F.[j] - A.Values.[k] * pv

        // Phase 2: zero constrained rows, set diagonal = 1, fix F[gdof]
        for KeyValue(gdof, pv) in prescribed do
            for k in A.RowPtr.[gdof] .. A.RowPtr.[gdof + 1] - 1 do
                A.Values.[k] <-
                    if A.ColIdx.[k] = gdof then 1.0 else 0.0
            F.[gdof] <- pv

        // Phase 3: zero constrained columns in all non-constrained rows
        let n = A.Rows
        for i in 0 .. n - 1 do
            if not (constrained.Contains i) then
                for k in A.RowPtr.[i] .. A.RowPtr.[i + 1] - 1 do
                    if constrained.Contains A.ColIdx.[k] then
                        A.Values.[k] <- 0.0


// ---------------------------------------------------------------------------
// Interface for assembled systems that carry a sparse CSR stiffness matrix
// ---------------------------------------------------------------------------

/// An IAssembledSystem that also exposes the stiffness matrix in CSR format.
/// Enables sparse-aware solvers and reaction recovery without materialising
/// the full dense K.
type ISparseAssembledSystem =
    inherit IAssembledSystem
    /// The original (unmodified) global stiffness matrix in CSR format.
    abstract KCsr : CsrMatrix
