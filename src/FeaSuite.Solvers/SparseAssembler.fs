namespace FeaSuite.Solvers

open System
open System.IO
open FeaSuite.Core
open FeaSuite.Storage

// ---------------------------------------------------------------------------
// Sparse assembled system backed by a CSR matrix
// ---------------------------------------------------------------------------

/// IAssembledSystem that stores the stiffness matrix as CSR for memory
/// efficiency.  The dense K property is computed lazily from the CSR data
/// and cached; for large systems callers should use KCsr directly.
type CsrAssembledSystem
        (totalDofs : int,
         rowPtr    : int[],
         colIdx    : int[],
         values    : float[],
         f         : float[]) =

    let kCsr = { Rows   = totalDofs
                 Cols   = totalDofs
                 RowPtr = rowPtr
                 ColIdx = colIdx
                 Values = values }

    let denseK =
        Lazy<float[,]>(fun () ->
            let A = Array2D.zeroCreate totalDofs totalDofs
            for i in 0 .. totalDofs - 1 do
                for k in rowPtr.[i] .. rowPtr.[i + 1] - 1 do
                    A.[i, colIdx.[k]] <- A.[i, colIdx.[k]] + values.[k]
            A)

    interface IAssembledSystem with
        member _.TotalDofs = totalDofs
        /// Dense representation (materialised lazily; expensive for large n).
        member _.K         = denseK.Value
        member _.F         = f

    interface ISparseAssembledSystem with
        member _.KCsr = kCsr


// ---------------------------------------------------------------------------
// Sparse assembler: builds global K using PagedMatrixStore as COO backend
// then finalises to CSR
// ---------------------------------------------------------------------------

/// Assembles the global stiffness matrix K into a file-backed
/// PagedMatrixStore (COO format) and then converts the result to a CSR
/// matrix, keeping memory use bounded during assembly.
///
/// Parameters:
///   tempPath – path for the temporary PagedMatrixStore file.
///              If None a path under Path.GetTempPath() is used.
///   pageBufferSize – write-buffer size (triplet count) for PagedMatrixStore.
type SparseAssembler(?tempPath: string, ?pageBufferSize: int) =

    let bufSize = defaultArg pageBufferSize 256

    let makeTempPath () =
        let dir = Path.GetTempPath()
        Path.Combine(dir, "feasuite_" + Guid.NewGuid().ToString("N") + ".kmat")

    interface IAssembler with
        member _.Assemble(model, loadCase) =
            let dofMap, totalDofs = FEAModel.buildDofMap model

            let filePath = defaultArg tempPath (makeTempPath())
            let deleteFile () =
                try if File.Exists filePath then File.Delete filePath
                with _ -> ()

            let F = Array.zeroCreate totalDofs

            try
                // Phase 1: assemble element stiffness contributions into the
                // file-backed COO store.  The store is disposed at the end of
                // this inner scope so that the file stream is released before
                // we re-open it read-only in Phase 2.
                let assemblyErrors =
                    let mutable errors : FEAError list = []
                    use store = new PagedMatrixStore(filePath, totalDofs, totalDofs, bufSize)

                    for kvp in model.Elements do
                        let elem = kvp.Value
                        match model.Materials.TryFind elem.MaterialId with
                        | None ->
                            errors <- errors @ [ MaterialNotFound (MaterialId.value elem.MaterialId) ]
                        | Some mat ->
                            match ElementStiffness.computeKe elem model.Nodes mat with
                            | Error errs ->
                                errors <- errors @ errs
                            | Ok (ke, _) ->
                                let globalDofs =
                                    [| for nid in elem.NodeIds do
                                           let node = model.Nodes.[nid]
                                           for d in 0 .. node.DegreesOfFreedom - 1 do
                                               yield dofMap.[(nid, d)] |]
                                let ndof = globalDofs.Length
                                for i in 0 .. ndof - 1 do
                                    for j in 0 .. ndof - 1 do
                                        let v = ke.[i, j]
                                        if v <> 0.0 then
                                            store.Add(globalDofs.[i], globalDofs.[j], v)

                    // Flush ensures all buffered entries are written before close
                    store.Flush()
                    errors
                // PagedMatrixStore stream is now closed (use scope ended above)

                if not (List.isEmpty assemblyErrors) then
                    deleteFile ()
                    Error assemblyErrors
                else

                // --- Assemble F (nodal loads) ---
                for load in loadCase.Loads do
                    match dofMap.TryFind (load.NodeId, load.LocalDofIndex) with
                    | Some gdof -> F.[gdof] <- F.[gdof] + load.Value
                    | None      -> ()

                // --- Assemble F (body forces from acceleration / gravity) ---
                ElementBodyForce.assembleBodyForces model loadCase dofMap F

                // Phase 2: read COO entries and convert to CSR.
                // File stream is closed so OpenReadOnly can access the file.
                let csr =
                    let (_, _, _, entries) = PagedMatrixStore.OpenReadOnly filePath
                    CsrMatrix.ofCooEntries totalDofs totalDofs entries

                deleteFile ()

                Ok (CsrAssembledSystem(totalDofs,
                                       csr.RowPtr,
                                       csr.ColIdx,
                                       csr.Values,
                                       F) :> IAssembledSystem)

            with ex ->
                deleteFile ()
                Error [ SolverError ex.Message ]
