module FeaSuite.Tests.SparseAssemblerTests

open Xunit
open FeaSuite.Core
open FeaSuite.Solvers

// ---------------------------------------------------------------------------
// Tests for SparseAssembler / CsrAssembledSystem / CsrMatrix
//
// 1.  CsrMatrix helpers: ofDense, mulVec, diagonal, ofCooEntries.
// 2.  SparseAssembler produces the same K and F as DenseAssembler on the
//     representative Bar1D model.
// 3.  Full pipeline with sparse assembler + dense solver gives same answer.
// 4.  Full pipeline with sparse assembler + CG gives same answer.
// 5.  Reaction recovery uses CSR SpMV and gives the correct result.
// ---------------------------------------------------------------------------

// -----------------------------------------------------------------------
// CsrMatrix unit tests
// -----------------------------------------------------------------------

[<Fact>]
let ``CsrMatrix.ofDense: round-trip dense to CSR and SpMV`` () =
    // K = [[2, -1], [-1, 2]]   x = [1, 1]   → K·x = [1, 1]
    let K = array2D [ [ 2.0; -1.0 ]; [ -1.0; 2.0 ] ]
    let csr = CsrMatrix.ofDense K
    let x   = [| 1.0; 1.0 |]
    let y   = CsrMatrix.mulVec csr x
    Assert.Equal( 1.0, y.[0])
    Assert.Equal( 1.0, y.[1])


[<Fact>]
let ``CsrMatrix.diagonal: returns correct main diagonal`` () =
    let K = array2D [ [ 3.0; 1.0; 0.0 ]
                      [ 1.0; 5.0; 2.0 ]
                      [ 0.0; 2.0; 4.0 ] ]
    let csr  = CsrMatrix.ofDense K
    let diag = CsrMatrix.diagonal csr
    Assert.Equal(3.0, diag.[0])
    Assert.Equal(5.0, diag.[1])
    Assert.Equal(4.0, diag.[2])


[<Fact>]
let ``CsrMatrix.ofCooEntries: sums duplicate triplets`` () =
    // Two entries at (0,0): 1.0 + 2.0 = 3.0; one at (1,1): 5.0
    let entries = seq {
        yield (0, 0, 1.0)
        yield (0, 0, 2.0)
        yield (1, 1, 5.0)
    }
    let csr = CsrMatrix.ofCooEntries 2 2 entries
    let y   = CsrMatrix.mulVec csr [| 1.0; 1.0 |]
    Assert.Equal(3.0, y.[0])
    Assert.Equal(5.0, y.[1])


[<Fact>]
let ``CsrMatrix.mulVec: identity matrix times vector returns vector`` () =
    let I = array2D [ [ 1.0; 0.0; 0.0 ]
                      [ 0.0; 1.0; 0.0 ]
                      [ 0.0; 0.0; 1.0 ] ]
    let csr = CsrMatrix.ofDense I
    let x   = [| 7.0; -3.0; 5.0 |]
    let y   = CsrMatrix.mulVec csr x
    Assert.Equal(x.[0], y.[0])
    Assert.Equal(x.[1], y.[1])
    Assert.Equal(x.[2], y.[2])


// -----------------------------------------------------------------------
// SparseAssembler vs DenseAssembler consistency
// -----------------------------------------------------------------------

[<Fact>]
let ``SparseAssembler: assembled K (dense view) matches DenseAssembler on bar1D`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 500.0
    let model, lc = Helpers.buildBar1DModel E A L F

    let denseAsm  = DenseAssembler()  :> IAssembler
    let sparseAsm = SparseAssembler() :> IAssembler

    match denseAsm.Assemble(model, lc), sparseAsm.Assemble(model, lc) with
    | Error e, _   -> failwith (sprintf "DenseAssembler failed: %A" e)
    | _, Error e   -> failwith (sprintf "SparseAssembler failed: %A" e)
    | Ok dense, Ok sparse ->
        let n = dense.TotalDofs
        Assert.Equal(n, sparse.TotalDofs)
        // Compare every entry
        let tol = 1e-10
        for i in 0 .. n - 1 do
            for j in 0 .. n - 1 do
                let diff = abs (dense.K.[i, j] - sparse.K.[i, j])
                Assert.True(diff < tol,
                    sprintf "K[%d,%d]: dense=%.6g sparse=%.6g diff=%.3e" i j dense.K.[i,j] sparse.K.[i,j] diff)
        for i in 0 .. n - 1 do
            let diff = abs (dense.F.[i] - sparse.F.[i])
            Assert.True(diff < tol,
                sprintf "F[%d]: dense=%.6g sparse=%.6g diff=%.3e" i dense.F.[i] sparse.F.[i] diff)


[<Fact>]
let ``SparseAssembler: ISparseAssembledSystem exposes KCsr`` () =
    let model, lc = Helpers.buildBar1DModel 2e11 1e-4 1.0 1000.0
    let asm = SparseAssembler() :> IAssembler
    match asm.Assemble(model, lc) with
    | Error e -> failwith (sprintf "Assemble failed: %A" e)
    | Ok sys  ->
        Assert.True(sys :? ISparseAssembledSystem,
                    "SparseAssembler result should implement ISparseAssembledSystem")
        let sparse = sys :?> ISparseAssembledSystem
        let csr    = sparse.KCsr
        Assert.Equal(sys.TotalDofs, csr.Rows)
        Assert.Equal(sys.TotalDofs, csr.Cols)
        Assert.True(csr.Values.Length > 0, "CSR should have non-zero entries")
        // RowPtr last entry = nnz
        Assert.Equal(csr.Values.Length, csr.RowPtr.[csr.Rows])


// -----------------------------------------------------------------------
// End-to-end pipeline tests with sparse assembler
// -----------------------------------------------------------------------

[<Fact>]
let ``pipeline: sparse assembler + dense solver gives analytical bar1D result`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 1000.0
    let model, _ = Helpers.buildBar1DModel E A L F
    let input = {
        Model              = model
        LoadCaseIndex      = 0
        UseNonlinear       = false
        NonlinearConfig    = NonlinearConfig.defaults
        LinearSolverKind   = Dense
        UseSparseAssembler = true
    }
    match FeaPipeline.run input with
    | Error e -> failwith (sprintf "Pipeline failed: %A" e)
    | Ok out  ->
        let u2_expected = F * L / (E * A)
        let u2_actual   = out.Displacements.[NodeId 2].[0]
        Helpers.assertClose u2_expected u2_actual "sparse+dense u2"


[<Fact>]
let ``pipeline: sparse assembler + CG gives same result as dense assembler + dense`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 1000.0
    let model, _ = Helpers.buildBar1DModel E A L F

    let runWith (sparse: bool) (kind: LinearSolverKind) =
        let input = {
            Model              = model
            LoadCaseIndex      = 0
            UseNonlinear       = false
            NonlinearConfig    = NonlinearConfig.defaults
            LinearSolverKind   = kind
            UseSparseAssembler = sparse
        }
        match FeaPipeline.run input with
        | Ok out  -> out.Displacements.[NodeId 2].[0]
        | Error e -> failwith (sprintf "%A" e)

    let uDense      = runWith false Dense
    let uSparseCg   = runWith true  (LinearSolverKind.defaultCg)

    let diff = abs (uDense - uSparseCg)
    Assert.True(diff < 1e-9,
        sprintf "sparse+CG vs dense+dense: diff=%.3e (dense=%.10g cg=%.10g)"
                diff uDense uSparseCg)


[<Fact>]
let ``pipeline: sparse assembler + BiCGSTAB gives correct reaction`` () =
    let E = 2e11
    let A = 1e-4
    let L = 1.0
    let F = 1000.0
    let model, _ = Helpers.buildBar1DModel E A L F
    let input = {
        Model              = model
        LoadCaseIndex      = 0
        UseNonlinear       = false
        NonlinearConfig    = NonlinearConfig.defaults
        LinearSolverKind   = LinearSolverKind.defaultBiCgStab
        UseSparseAssembler = true
    }
    match FeaPipeline.run input with
    | Error e -> failwith (sprintf "Pipeline failed: %A" e)
    | Ok out  ->
        match out.Reactions.TryFind (NodeId 1, 0) with
        | None   -> failwith "No reaction at Node 1"
        | Some r -> Helpers.assertClose (-F) r "sparse+BiCGSTAB reaction R1"


[<Fact>]
let ``pipeline: sparse assembler matches dense assembler on Truss3D`` () =
    let e = 210e9
    let a = 1.0e-4
    let mat = {
        Id = MaterialId 1; Name = "Steel"
        YoungModulus = e; PoissonRatio = 0.3; Density = 7850.0
        CrossSectionArea = Some a
    }
    let n1 = { Id = NodeId 1; Position = { X = 0.0; Y = 0.0; Z = 0.0 }; DegreesOfFreedom = 3 }
    let n2 = { Id = NodeId 2; Position = { X = 1.0; Y = 0.0; Z = 0.0 }; DegreesOfFreedom = 3 }
    let n3 = { Id = NodeId 3; Position = { X = 0.0; Y = 1.0; Z = 0.0 }; DegreesOfFreedom = 3 }
    let n4 = { Id = NodeId 4; Position = { X = 0.0; Y = 0.0; Z = 1.0 }; DegreesOfFreedom = 3 }

    let mkBar eid i j =
        { Id = ElementId eid; Type = Beam Truss3D
          NodeIds = [ NodeId i; NodeId j ]; MaterialId = MaterialId 1
          Properties = Map.empty }

    let elements = [ mkBar 1 1 2; mkBar 2 1 3; mkBar 3 1 4
                     mkBar 4 2 3; mkBar 5 2 4; mkBar 6 3 4 ]

    let baseBcs =
        [ for nid in [1;2;3] do
              for dof in [0;1;2] do
                  yield { NodeId = NodeId nid; LocalDofIndex = dof; Constraint = Fixed } ]

    let loads =
        [ { NodeId = NodeId 4; LocalDofIndex = 0; Value = 3000.0 }
          { NodeId = NodeId 4; LocalDofIndex = 1; Value = -2000.0 }
          { NodeId = NodeId 4; LocalDofIndex = 2; Value = -8000.0 } ]
    let lc = { Id = LoadCaseId 1; Name = "LC1"; Loads = loads; BoundaryConditions = baseBcs }

    let model =
        FEAModel.empty
        |> FEAModel.addMaterial mat
        |> FEAModel.addNode n1 |> FEAModel.addNode n2
        |> FEAModel.addNode n3 |> FEAModel.addNode n4
        |> (fun m -> elements |> List.fold (fun acc e -> FEAModel.addElement e acc) m)
        |> FEAModel.addLoadCase lc

    let runWith sparse =
        let input = {
            Model              = model
            LoadCaseIndex      = 0
            UseNonlinear       = false
            NonlinearConfig    = NonlinearConfig.defaults
            LinearSolverKind   = Dense
            UseSparseAssembler = sparse
        }
        match FeaPipeline.run input with
        | Ok out  -> out.Displacements.[NodeId 4]
        | Error e -> failwith (sprintf "%A" e)

    let uDense  = runWith false
    let uSparse = runWith true

    for d in 0 .. 2 do
        let diff = abs (uDense.[d] - uSparse.[d])
        Assert.True(diff < 1e-10,
            sprintf "Truss3D DOF %d: dense=%.10g sparse=%.10g diff=%.3e"
                    d uDense.[d] uSparse.[d] diff)
