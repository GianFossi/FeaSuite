module FeaSuite.Tests.SkylineMatrixTests

open Xunit
open FeaSuite.Core
open FeaSuite.Solvers

// ---------------------------------------------------------------------------
// SkylineMatrix and SkylineCholesky tests
// ---------------------------------------------------------------------------

let private tol = 1e-10

let private assertClose expected actual label =
    let diff = abs (expected - actual)
    if diff > tol then
        failwith (sprintf "%s — expected %.10g, got %.10g (diff=%.3e)" label expected actual diff)

// -----------------------------------------------------------------------
// SkylineMatrix element access
// -----------------------------------------------------------------------

[<Fact>]
let ``SkylineMatrix.Create: zero-initialised`` () =
    let profile = [| 0; 0; 1; 1 |]   // 4×4
    let sky = SkylineMatrix.Create(4, profile)
    for i in 0..3 do
        for j in 0..3 do
            Assert.Equal(0.0, sky.[i, j])

[<Fact>]
let ``SkylineMatrix: set and get diagonal entries`` () =
    let n       = 3
    let profile = [| 0; 1; 2 |]   // identity profile (diagonal-only)
    let sky = SkylineMatrix.Create(n, profile)
    sky.[0, 0] <- 1.0
    sky.[1, 1] <- 2.0
    sky.[2, 2] <- 3.0
    Assert.Equal(1.0, sky.[0, 0])
    Assert.Equal(2.0, sky.[1, 1])
    Assert.Equal(3.0, sky.[2, 2])

[<Fact>]
let ``SkylineMatrix: symmetry – set upper, read lower`` () =
    let profile = [| 0; 0; 0 |]   // full profile
    let sky = SkylineMatrix.Create(3, profile)
    sky.[0, 1] <- 5.0
    sky.[0, 2] <- 7.0
    Assert.Equal(5.0, sky.[1, 0])   // transpose
    Assert.Equal(7.0, sky.[2, 0])

[<Fact>]
let ``SkylineMatrix.Add: accumulates correctly`` () =
    let profile = [| 0; 0; 0 |]
    let sky = SkylineMatrix.Create(3, profile)
    sky.Add(1, 1, 3.0)
    sky.Add(1, 1, 4.0)
    Assert.Equal(7.0, sky.[1, 1])

[<Fact>]
let ``SkylineMatrix: entries outside profile read as zero`` () =
    // profile = [0, 1, 2] → only diagonal stored; off-diagonal = structural zero
    let profile = [| 0; 1; 2 |]
    let sky = SkylineMatrix.Create(3, profile)
    sky.[0, 0] <- 10.0
    Assert.Equal(0.0, sky.[0, 1])   // outside profile → zero
    Assert.Equal(0.0, sky.[1, 2])

[<Fact>]
let ``SkylineMatrix.FromDense: round-trips a 3×3 symmetric matrix`` () =
    let K = array2D [ [ 4.0; -1.0;  0.0 ]
                      [ -1.0; 4.0; -1.0 ]
                      [  0.0; -1.0; 4.0 ] ]
    let sky   = SkylineMatrix.FromDense K
    let dense = sky.ToDense()
    for i in 0..2 do
        for j in 0..2 do
            assertClose K.[i, j] dense.[i, j] (sprintf "K[%d,%d]" i j)

// -----------------------------------------------------------------------
// Cholesky factorisation
// -----------------------------------------------------------------------

[<Fact>]
let ``SkylineCholesky: factors 2×2 SPD matrix`` () =
    // K = [[4, 2], [2, 3]]  → L = [[2, 0], [1, sqrt(2)]]
    let K = array2D [ [ 4.0; 2.0 ]; [ 2.0; 3.0 ] ]
    let sky = SkylineMatrix.FromDense K
    match SkylineCholesky.factorize sky with
    | Error e -> failwith (sprintf "Unexpected error: %A" e)
    | Ok fac  ->
        // L[0,0] = 2, L[1,0] = 1, L[1,1] = sqrt(2)
        assertClose 2.0          fac.[0, 0] "L00"
        assertClose 1.0          fac.[0, 1] "L01 (=L10)"
        assertClose (sqrt 2.0)   fac.[1, 1] "L11"

[<Fact>]
let ``SkylineCholesky: detects non-SPD matrix`` () =
    // Indefinite matrix → should fail
    let K = array2D [ [ -1.0; 0.0 ]; [ 0.0; 2.0 ] ]
    let sky = SkylineMatrix.FromDense K
    match SkylineCholesky.factorize sky with
    | Error [ SingularMatrix ] -> ()
    | other -> failwith (sprintf "Expected SingularMatrix, got %A" other)

[<Fact>]
let ``SkylineCholesky.solve: 2×2 system`` () =
    // K·x = b  →  [[4,2],[2,3]] · [x0;x1] = [8;7]  →  x = [1.5; 1.0]
    let K = array2D [ [ 4.0; 2.0 ]; [ 2.0; 3.0 ] ]
    let b = [| 8.0; 7.0 |]
    let sky = SkylineMatrix.FromDense K
    match SkylineCholesky.factorize sky with
    | Error e -> failwith (sprintf "Factorize failed: %A" e)
    | Ok fac  ->
        let x = SkylineCholesky.solve fac b
        assertClose 1.25 x.[0] "x0"
        assertClose 1.5  x.[1] "x1"

[<Fact>]
let ``SkylineCholesky.solve: 3×3 tridiagonal system`` () =
    // Same stiffness structure as a 3-bar chain
    let K = array2D [ [ 4.0; -1.0;  0.0 ]
                      [ -1.0; 4.0; -1.0 ]
                      [  0.0; -1.0; 4.0 ] ]
    let b = [| 1.0; 0.0; 1.0 |]
    // Exact: solve with standard Gaussian first to get reference
    match GaussianElimination.solve (DenseMatrix.copy K) (Array.copy b) with
    | Error e -> failwith (sprintf "Gaussian failed: %A" e)
    | Ok xRef ->
        let sky = SkylineMatrix.FromDense K
        match SkylineCholesky.factorize sky with
        | Error e -> failwith (sprintf "Cholesky failed: %A" e)
        | Ok fac  ->
            let x = SkylineCholesky.solve fac b
            for i in 0..2 do
                assertClose xRef.[i] x.[i] (sprintf "x[%d]" i)

// -----------------------------------------------------------------------
// SkylineLinearSolver – end-to-end via ILinearSolver interface
// -----------------------------------------------------------------------

[<Fact>]
let ``SkylineLinearSolver: bar1D gives same result as DenseLinearSolver`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 1000.0
    let model, lc = Helpers.buildBar1DModel E A L F
    let dofMap, _ = FEAModel.buildDofMap model

    let assembler = DenseAssembler() :> IAssembler
    match assembler.Assemble(model, lc) with
    | Error e -> failwith (sprintf "Assembly failed: %A" e)
    | Ok system ->
        let denseSolver   = DenseLinearSolver()   :> ILinearSolver
        let skylineSolver = SkylineLinearSolver() :> ILinearSolver

        match denseSolver.Solve(system, lc.BoundaryConditions, dofMap),
              skylineSolver.Solve(system, lc.BoundaryConditions, dofMap) with
        | Ok uDense, Ok uSky ->
            for i in 0 .. uDense.Length - 1 do
                assertClose uDense.[i] uSky.[i] (sprintf "u[%d]" i)
        | Error e, _ -> failwith (sprintf "Dense solver failed: %A" e)
        | _, Error e -> failwith (sprintf "Skyline solver failed: %A" e)

// -----------------------------------------------------------------------
// MathNet.Numerics solvers
// -----------------------------------------------------------------------

[<Fact>]
let ``MathNetDenseLinearSolver: bar1D gives correct displacement`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 1000.0
    let model, lc = Helpers.buildBar1DModel E A L F
    let dofMap, _ = FEAModel.buildDofMap model

    let assembler = DenseAssembler() :> IAssembler
    match assembler.Assemble(model, lc) with
    | Error e -> failwith (sprintf "Assembly failed: %A" e)
    | Ok system ->
        let solver = MathNetDenseLinearSolver() :> ILinearSolver
        match solver.Solve(system, lc.BoundaryConditions, dofMap) with
        | Error e -> failwith (sprintf "Solver failed: %A" e)
        | Ok u    ->
            let u2Expected = F * L / (E * A)
            assertClose u2Expected u.[1] "u2 (MathNet dense)"

[<Fact>]
let ``MathNetSparseLinearSolver: bar1D gives correct displacement`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 1000.0
    let model, lc = Helpers.buildBar1DModel E A L F
    let dofMap, _ = FEAModel.buildDofMap model

    let assembler = DenseAssembler() :> IAssembler
    match assembler.Assemble(model, lc) with
    | Error e -> failwith (sprintf "Assembly failed: %A" e)
    | Ok system ->
        let solver = MathNetSparseLinearSolver() :> ILinearSolver
        match solver.Solve(system, lc.BoundaryConditions, dofMap) with
        | Error e -> failwith (sprintf "Solver failed: %A" e)
        | Ok u    ->
            let u2Expected = F * L / (E * A)
            assertClose u2Expected u.[1] "u2 (MathNet sparse)"
