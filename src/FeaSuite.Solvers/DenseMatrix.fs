namespace FeaSuite.Solvers

open FeaSuite.Core

// ---------------------------------------------------------------------------
// Helpers for dense matrix algebra (float[,])
// ---------------------------------------------------------------------------

module DenseMatrix =

    /// Create an n×n zero matrix.
    let create (n: int) : float[,] = Array2D.zeroCreate n n

    /// Create a zero vector of length n.
    let createVec (n: int) : float[] = Array.zeroCreate n

    /// Add a sub-matrix ke into K at the given global DOF indices.
    let addSubMatrix (K: float[,]) (ke: float[,]) (globalDofs: int[]) : unit =
        let ndof = globalDofs.Length
        for i in 0 .. ndof - 1 do
            for j in 0 .. ndof - 1 do
                K.[globalDofs.[i], globalDofs.[j]] <- K.[globalDofs.[i], globalDofs.[j]] + ke.[i, j]

    /// Add contributions of a local force vector fe into global F.
    let addSubVector (F: float[]) (fe: float[]) (globalDofs: int[]) : unit =
        let ndof = globalDofs.Length
        for i in 0 .. ndof - 1 do
            F.[globalDofs.[i]] <- F.[globalDofs.[i]] + fe.[i]

    /// Copy a matrix (deep).
    let copy (A: float[,]) : float[,] =
        let n = Array2D.length1 A
        let m = Array2D.length2 A
        let B = Array2D.zeroCreate n m
        Array2D.blit A 0 0 B 0 0 n m
        B

    /// Matrix-vector product A·x.
    let mulVec (A: float[,]) (x: float[]) : float[] =
        let n = Array2D.length1 A
        let m = Array2D.length2 A
        let y = Array.zeroCreate n
        for i in 0 .. n - 1 do
            let mutable s = 0.0
            for j in 0 .. m - 1 do
                s <- s + A.[i, j] * x.[j]
            y.[i] <- s
        y

    /// Euclidean norm of a vector.
    let norm (v: float[]) : float =
        v |> Array.sumBy (fun x -> x * x) |> sqrt

    /// v1 - v2 element-wise.
    let subtract (v1: float[]) (v2: float[]) : float[] =
        Array.map2 (-) v1 v2
