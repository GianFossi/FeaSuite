# FeaSuite

**FeaSuite** is a modular, reusable Finite Element Analysis (FEA) library written in F#.  
It provides a complete mini-solver pipeline — from model definition to result recovery — and is designed to serve as a foundation for more sophisticated structural analysis tools.

---

## Objectives

| Goal | Status |
|------|--------|
| Modular FEA architecture (Core / Solvers / Storage / Post) | ✅ v1.0 |
| Linear FEA solver (dense, Gaussian elimination) | ✅ v1.0 |
| Non-linear solver (incremental Newton-Raphson) | ✅ v1.0 |
| Out-of-core paged storage (file-backed vectors and matrices) | ✅ v1.0 |
| Railway-Oriented error handling (ROP / `Validation<'T>`) | ✅ v1.0 |
| Geometry adapter (compatible with GianFossi/Geometry) | ✅ v1.0 |
| CG / BiCGSTAB sparse iterative solvers (large systems) | ✅ v1.1 |
| Sparse CSR assembly backed by `PagedMatrixStore` | ✅ v1.1 |
| Direct sparse solver (Skyline Cholesky) | ✅ v1.2 |
| Iterative PCG solver (Jacobi-preconditioned CG) | ✅ v1.2 |
| Modal eigensolver (natural frequencies + mode shapes) | ✅ v1.2 |
| Beam / Shell / Solid elements | 🗺 Roadmap |

---

## Architecture

```
FeaSuite.slnx
├── src/
│   ├── FeaSuite.Core/           Core domain types, interfaces and validation
│   │   ├── Geometry.fs          Point3D / Vector3D adapter (→ GianFossi/Geometry)
│   │   ├── ROP.fs               FEAError DU + Validation<'T> CE (→ GianFossi/ROP)
│   │   ├── Domain.fs            Node, Element, Material, BC, Load, FEAModel
│   │   ├── Interfaces.fs        IAssembler, ILinearSolver, INonlinearSolver,
│   │   │                        IResultRecovery, IPagedVector<'T>, IPagedMatrixStore
│   │   └── Validation.fs        ModelValidation.validate (ROP-based)
│   │
│   ├── FeaSuite.Solvers/        Linear and non-linear solvers
│   │   ├── DenseMatrix.fs       Dense array helpers (add submatrix, norm, mul)
│   │   ├── Assembler.fs         DenseAssembler – Bar1D and Truss3D stiffness + assembly
│   │   ├── CsrMatrix.fs         CSR sparse matrix type, SpMV, BC elimination, ISparseAssembledSystem
│   │   ├── LinearSolver.fs      DenseLinearSolver – Gaussian elimination with BCs
│   │   ├── SkylineMatrix.fs     Skyline (profile) sparse matrix + Cholesky solver
│   │   ├── MathNetSolver.fs     MathNet dense LU + BiCGSTAB solvers
│   │   ├── IterativeSolvers.fs  CgSolver (PCG) and BiCgStabSolver – pure F# iterative solvers
│   │   ├── SparseAssembler.fs   SparseAssembler – PagedMatrixStore-backed CSR assembly
│   │   ├── NonlinearSolver.fs   NewtonRaphsonSolver – incremental N-R with correction BCs
│   │   ├── MassMatrix.fs        ElementMass + MassAssembler – lumped diagonal mass matrix
│   │   ├── ModalSolver.fs       MathNetModalSolver – modal eigensolver (K·φ = ω²·M·φ)
│   │   └── Pipeline.fs          FeaPipeline.run – end-to-end solve, solver selection
│   │
│   ├── FeaSuite.Storage/        Out-of-core paged storage
│   │   ├── PagedVector.fs       File-backed float vector with LRU page cache
│   │   └── PagedMatrixStore.fs  File-backed COO sparse matrix store
│   │
│   └── FeaSuite.Post/           Post-processing and result recovery
│       ├── ResultTypes.fs       NodalDisplacements, NodalReactions, ElementResult1D
│       └── ResultRecovery.fs    Displacement / reaction / Bar1D stress recovery
│
└── tests/
    └── FeaSuite.Tests/
        ├── Helpers.fs            Shared test model builders
        ├── ModelValidationTests  ROP validation pipeline tests
        ├── LinearSolverTests     1-D bar analytical verification
        ├── PagedStorageTests     File-backed vector and matrix tests
        └── ROPFlowTests          Validation<'T> CE and error propagation tests
```

### Design principles

* **Immutable domain model** – `FEAModel`, `Node`, `Element` etc. are F# record types.  
* **Railway-Oriented errors** – every fallible operation returns `Validation<'T>` (`Result<'T, FEAError list>`), collecting all errors rather than short-circuiting on the first one (except the pipeline which short-circuits for efficiency).  
* **Pluggable solvers** – `ILinearSolver`, `INonlinearSolver`, `IAssembler` are interfaces; swap implementations without changing callers.  
* **Paged storage** – `PagedVector` and `PagedMatrixStore` abstract away whether data lives in RAM or on disk, enabling future support for very large models.

---

## Dependencies

The two external repositories are currently provided as **compatible adapter types** in `FeaSuite.Core`:

| External repo | Location in FeaSuite | Integration plan |
|---|---|---|
| [GianFossi/Geometry](https://github.com/GianFossi/Geometry) | `src/FeaSuite.Core/Geometry.fs` | Replace `Point3D`/`Vector3D` stubs with a NuGet package reference once the package is published. |
| [GianFossi/ROP](https://github.com/GianFossi/ROP) | `src/FeaSuite.Core/ROP.fs` | Replace `FEAError`/`Validation<'T>` stubs with a NuGet package reference; the discriminated union is already shaped to be compatible. |

---

## Minimum usage example

```fsharp
open FeaSuite.Core
open FeaSuite.Solvers

// --- Build a 2-node, 1-D bar model ---
// Node 1 (x=0) --[EA=2e7 N]-- Node 2 (x=1 m)
// BC: u1 = 0 (fixed)    Load: F = 1000 N at Node 2

let mat = {
    Id = MaterialId 1; Name = "Steel"
    YoungModulus = 2e11; PoissonRatio = 0.3
    Density = 7850.0; CrossSectionArea = Some 1e-4
}
let n1 = { Id = NodeId 1; Position = Point3D.ofXY 0.0 0.0; DegreesOfFreedom = 1 }
let n2 = { Id = NodeId 2; Position = Point3D.ofXY 1.0 0.0; DegreesOfFreedom = 1 }
let el = { Id = ElementId 1; Type = Bar1D; NodeIds = [NodeId 1; NodeId 2]
           MaterialId = MaterialId 1; Properties = Map.empty }
let bc = { NodeId = NodeId 1; LocalDofIndex = 0; Constraint = Fixed }
let ld = { NodeId = NodeId 2; LocalDofIndex = 0; Value = 1000.0 }
let lc = { Id = LoadCaseId 1; Name = "LC1"; Loads = [ld]; BoundaryConditions = [bc] }

let model =
    FEAModel.empty
    |> FEAModel.addMaterial mat
    |> FEAModel.addNode n1
    |> FEAModel.addNode n2
    |> FEAModel.addElement el
    |> FEAModel.addLoadCase lc

// --- Run the linear pipeline ---
let input = { Model = model; LoadCaseIndex = 0; UseNonlinear = false
              NonlinearConfig = NonlinearConfig.defaults
              LinearSolverKind = Dense; UseSparseAssembler = false }

match FeaPipeline.run input with
| Error errors ->
    printfn "FEA failed: %A" errors
| Ok output ->
    // Analytical answer: u2 = F·L/(E·A) = 1000·1/(2e11·1e-4) = 5e-8 m
    printfn "u2 = %.3e m" output.Displacements.[NodeId 2].[0]
    printfn "R1 = %.3f N"  output.Reactions.[(NodeId 1, 0)]
```

### Non-linear (Newton-Raphson)

```fsharp
let nlConfig = { MaxIterations = 50; ResidualTolerance = 1e-8; IncrementCount = 10 }
let nlInput  = { input with UseNonlinear = true; NonlinearConfig = nlConfig }

match FeaPipeline.run nlInput with
| Ok out  -> printfn "NL u2 = %.3e m" out.Displacements.[NodeId 2].[0]
| Error e -> printfn "NL failed: %A" e
```

### Out-of-core paged vector

```fsharp
open FeaSuite.Storage

use vec = PagedVector.Create("/tmp/my_vector.dat", length = 1_000_000, pageSize = 4096)
vec.[0]   <- 1.0
vec.[999_999] <- 42.0
vec.Flush()
// Only `pageSize` floats are in RAM at any time.
```

### Direct sparse solver (Skyline Cholesky)

Use `LinearSolverKind = SparseDirect` to select the skyline (profile) Cholesky
direct sparse solver.  It is more memory-efficient than the dense solver for
banded or sparse stiffness matrices and requires a symmetric positive-definite
system.

```fsharp
let sparseDirectInput = { input with LinearSolverKind = SparseDirect }
match FeaPipeline.run sparseDirectInput with
| Ok out  -> printfn "SparseDirect u2 = %.3e m" out.Displacements.[NodeId 2].[0]
| Error e -> printfn "failed: %A" e
```

### Iterative solvers (PCG / BiCGSTAB)

Use `LinearSolverKind` to select an iterative solver for large sparse systems.
Both solvers use diagonal (Jacobi) preconditioning and work with any `IAssembledSystem`.

```fsharp
// Conjugate Gradient – best for symmetric positive-definite systems
let cgInput = { input with LinearSolverKind = SparseCg (10_000, 1e-10) }
match FeaPipeline.run cgInput with
| Ok out  -> printfn "CG u2 = %.3e m" out.Displacements.[NodeId 2].[0]
| Error e -> printfn "CG failed: %A" e

// BiCGSTAB – handles general (possibly non-symmetric) systems
let bicgInput = { input with LinearSolverKind = SparseBiCgStab (10_000, 1e-10) }
match FeaPipeline.run bicgInput with
| Ok out  -> printfn "BiCGSTAB u2 = %.3e m" out.Displacements.[NodeId 2].[0]
| Error e -> printfn "BiCGSTAB failed: %A" e

// Access diagnostics (iterations, residual norm, converged flag)
// by casting the solver directly; the pipeline also wraps this automatically.
let solver = CgSolver(maxIterations = 5000, tolerance = 1e-12)
let sys    = (* your IAssembledSystem *) ...
match (solver :> ILinearSolver).Solve(sys, bcs, dofMap) with
| Ok u ->
    let diag = (solver :> IIterativeSolverDiagnostics).LastDiagnostics.Value
    printfn "Converged=%b  Iters=%d  ||r||=%.3e" diag.Converged diag.Iterations diag.ResidualNorm
| Error e -> printfn "Solver failed: %A" e
```

Default shorthand helpers:

```fsharp
LinearSolverKind.defaultCg       // SparseCg (10_000, 1e-10)
LinearSolverKind.defaultBiCgStab // SparseBiCgStab (10_000, 1e-10)
```

### Sparse CSR assembly (`SparseAssembler` + `PagedMatrixStore`)

For large models, use `UseSparseAssembler = true` to assemble via a
file-backed `PagedMatrixStore` (COO format) and finalise to CSR.
This keeps memory use bounded during assembly.

```fsharp
let sparseInput = {
    input with
        UseSparseAssembler = true
        LinearSolverKind   = LinearSolverKind.defaultCg
}
match FeaPipeline.run sparseInput with
| Ok out  -> printfn "sparse+CG u2 = %.3e m" out.Displacements.[NodeId 2].[0]
| Error e -> printfn "sparse pipeline failed: %A" e
```

The resulting `IAssembledSystem` also implements `ISparseAssembledSystem`, which
exposes the raw CSR arrays for callers that need direct access:

```fsharp
let asm = SparseAssembler() :> IAssembler
match asm.Assemble(model, loadCase) with
| Ok sys ->
    let sparse = sys :?> ISparseAssembledSystem
    let csr    = sparse.KCsr
    printfn "NNZ = %d  rows = %d" csr.Values.Length csr.Rows
    // csr.RowPtr, csr.ColIdx, csr.Values are standard CSR arrays
| Error e -> printfn "Assembly failed: %A" e
```

**Tradeoff note:** `SparseAssembler` converts the COO file to an in-memory CSR
matrix after assembly. For very large models, ensure sufficient RAM for the CSR
representation of K. The temporary COO file is deleted after conversion.

### Modal analysis (natural frequencies and mode shapes)

`ModalPipeline.run` solves the generalised eigenvalue problem **K·φ = ω²·M·φ**
to extract natural frequencies and mode shapes.  The load case supplies only the
boundary conditions (rigid supports); no loads are required.

```fsharp
open FeaSuite.Solvers

// Reuse the same model; only BCs matter for modal analysis.
let modalInput = {
    ModalSolveInput.defaults with
        Model         = model
        LoadCaseIndex = 0           // load case that carries the BCs
        Config        = { NumberOfModes = 10 }
}

match ModalPipeline.run modalInput with
| Error e -> printfn "Modal failed: %A" e
| Ok out  ->
    for m in out.Modes do
        printfn "Mode %d:  f = %.2f Hz  (ω = %.2f rad/s)" m.Index m.NaturalFrequency m.AngularFrequency
```

Each `ModalMode` carries:

| Field | Description |
|---|---|
| `Index` | 1-based mode number, sorted by ascending frequency |
| `AngularFrequency` | ω_i [rad/s] = √λ_i |
| `NaturalFrequency` | f_i [Hz] = ω_i / (2π) |
| `ModeShape` | Full displacement vector (constrained DOFs = 0) |

The solver uses a **lumped (diagonal) mass matrix** assembled by `MassAssembler`
and a dense symmetric EVD backed by **MathNet.Numerics** (MKL-accelerated when
available).  It is suitable for models up to a few thousand free DOFs.

---

## Roadmap

### Near-term
- [x] **Truss3D pipeline test** (full 3-D truss with multiple load cases)
- [x] **CG / BiCGSTAB sparse iterative solver** for large systems
- [x] **Sparse CSR matrix assembly** backed by `PagedMatrixStore`
- [x] **Direct sparse solver** (Skyline Cholesky, `SparseDirect`)
- [x] **Modal eigensolver** (natural frequencies + mode shapes via EVD)

### Medium-term
- [ ] **Beam2D element** (Euler-Bernoulli, 3 DOFs/node: ux, uy, rz)
- [ ] **Beam3D element** (6 DOFs/node: ux, uy, uz, rx, ry, rz)
- [ ] **Geometric nonlinearity** (corotational formulation for large displacements)
- [ ] **Multiple load case batch solve** with result aggregation

### Long-term
- [ ] **Shell4 element** (MITC4 formulation)
- [ ] **Solid8 element** (8-node hexahedral, reduced integration)
- [ ] **Dynamic time-history integration** (Newmark-β, HHT-α)
- [ ] **Parallel assembly** using .NET `Parallel.ForEach`
- [ ] **NuGet package** publishing

---

## Building and testing

```bash
dotnet build FeaSuite.slnx
dotnet test  FeaSuite.slnx
```

Requires .NET 8 SDK or later.

---

## License

MIT

