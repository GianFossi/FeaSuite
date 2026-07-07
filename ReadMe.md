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
│   │   ├── LinearSolver.fs      DenseLinearSolver – Gaussian elimination with BCs
│   │   ├── NonlinearSolver.fs   NewtonRaphsonSolver – incremental N-R with correction BCs
│   │   └── Pipeline.fs          FeaPipeline.run – end-to-end solve
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
              NonlinearConfig = NonlinearConfig.defaults }

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

---

## Roadmap

### Near-term
- [ ] **Truss3D pipeline test** (full 3-D truss with multiple load cases)
- [ ] **CG / BiCGSTAB sparse iterative solver** for large systems
- [ ] **Sparse CSR matrix assembly** backed by `PagedMatrixStore`

### Medium-term
- [ ] **Beam2D element** (Euler-Bernoulli, 3 DOFs/node: ux, uy, rz)
- [ ] **Beam3D element** (6 DOFs/node: ux, uy, uz, rx, ry, rz)
- [ ] **Geometric nonlinearity** (corotational formulation for large displacements)
- [ ] **Multiple load case batch solve** with result aggregation

### Long-term
- [ ] **Shell4 element** (MITC4 formulation)
- [ ] **Solid8 element** (8-node hexahedral, reduced integration)
- [ ] **Dynamic analysis** (modal, time-history integration)
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

