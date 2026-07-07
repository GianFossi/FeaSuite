namespace FeaSuite.Solvers

open FeaSuite.Core

// ---------------------------------------------------------------------------
// Assembler: builds global K and F from the FEA model
// ---------------------------------------------------------------------------

/// Concrete assembled system using dense arrays (suitable for small models).
type DenseAssembledSystem(totalDofs: int, k: float[,], f: float[]) =
    interface IAssembledSystem with
        member _.TotalDofs = totalDofs
        member _.K         = k
        member _.F         = f

// ---------------------------------------------------------------------------
// Element stiffness routines
// ---------------------------------------------------------------------------

module ElementStiffness =

    /// 1-D bar element stiffness matrix.
    ///   k_e = (E·A / L) * [[1, -1], [-1, 1]]
    let bar1D (e: Element) (nodeI: Node) (nodeJ: Node) (mat: Material) : float[,] =
        let L = Point3D.distanceTo nodeI.Position nodeJ.Position
        if L < 1e-15 then
            failwith (sprintf "Bar1D element %d: zero length." (ElementId.value e.Id))
        let ea = mat.YoungModulus * (mat.CrossSectionArea |> Option.defaultValue 1.0)
        let k  = ea / L
        array2D [ [ k; -k ]; [ -k; k ] ]

    /// 3-D truss element stiffness matrix (6×6), returned in global coordinates.
    ///   Uses direction cosines l, m, n.
    let truss3D (e: Element) (nodeI: Node) (nodeJ: Node) (mat: Material) : float[,] =
        let v = Point3D.vectorTo nodeI.Position nodeJ.Position
        let L = Vector3D.magnitude v
        if L < 1e-15 then
            failwith (sprintf "Truss3D element %d: zero length." (ElementId.value e.Id))
        let l  = v.Dx / L
        let m  = v.Dy / L
        let n  = v.Dz / L
        let ea = mat.YoungModulus * (mat.CrossSectionArea |> Option.defaultValue 1.0)
        let k  = ea / L
        // T = [l, m, n, 0, 0, 0; 0, 0, 0, l, m, n]
        // ke = k * T^T * [[1,-1];[-1,1]] * T
        let c = [| l; m; n |]
        let ke = Array2D.zeroCreate<float> 6 6
        for i in 0 .. 2 do
            for j in 0 .. 2 do
                ke.[i,   j  ] <-  k * c.[i] * c.[j]
                ke.[i,   j+3] <- -k * c.[i] * c.[j]
                ke.[i+3, j  ] <- -k * c.[i] * c.[j]
                ke.[i+3, j+3] <-  k * c.[i] * c.[j]
        ke

    /// Dispatch to the correct stiffness routine for the given element type.
    let computeKe (e: Element) (nodes: Map<NodeId, Node>) (mat: Material)
                  : Validation<float[,] * int[]> =
        match e.Type with
        | Bar1D ->
            match e.NodeIds with
            | [ nidI; nidJ ] ->
                match nodes.TryFind nidI, nodes.TryFind nidJ with
                | Some nI, Some nJ ->
                    let ke = bar1D e nI nJ mat
                    Ok (ke, [| 0; 1 |]) // local DOF indices (will be mapped globally later)
                | None, _ -> Validation.fail (NodeNotFound (NodeId.value nidI))
                | _, None -> Validation.fail (NodeNotFound (NodeId.value nidJ))
            | _ -> Validation.fail (InvalidInput (sprintf "Bar1D element %d must have exactly 2 nodes." (ElementId.value e.Id)))

        | Truss3D ->
            match e.NodeIds with
            | [ nidI; nidJ ] ->
                match nodes.TryFind nidI, nodes.TryFind nidJ with
                | Some nI, Some nJ ->
                    let ke = truss3D e nI nJ mat
                    Ok (ke, [| 0; 1; 2; 3; 4; 5 |])
                | None, _ -> Validation.fail (NodeNotFound (NodeId.value nidI))
                | _, None -> Validation.fail (NodeNotFound (NodeId.value nidJ))
            | _ -> Validation.fail (InvalidInput (sprintf "Truss3D element %d must have exactly 2 nodes." (ElementId.value e.Id)))

        | t ->
            Validation.fail (NotImplemented (sprintf "Element type %A is not yet implemented." t))

// ---------------------------------------------------------------------------
// Main Assembler
// ---------------------------------------------------------------------------

/// Assembles the global K (dense) and F from a validated FEA model + load case.
type DenseAssembler() =
    interface IAssembler with
        member _.Assemble(model, loadCase) =
            let dofMap, totalDofs = FEAModel.buildDofMap model

            let K = DenseMatrix.create totalDofs
            let F = DenseMatrix.createVec totalDofs

            // --- Assemble K ---
            let mutable assemblyErrors : FEAError list = []
            for KeyValue(_, elem) in model.Elements do
                match model.Materials.TryFind elem.MaterialId with
                | None ->
                    assemblyErrors <- assemblyErrors @ [ MaterialNotFound (MaterialId.value elem.MaterialId) ]
                | Some mat ->
                    match ElementStiffness.computeKe elem model.Nodes mat with
                    | Error errs -> assemblyErrors <- assemblyErrors @ errs
                    | Ok (ke, _localDofIdx) ->
                        // Map element nodes/DOFs to global indices
                        let globalDofs =
                            [| for nid in elem.NodeIds do
                                   let node = model.Nodes.[nid]
                                   for d in 0 .. node.DegreesOfFreedom - 1 do
                                       yield dofMap.[(nid, d)] |]
                        DenseMatrix.addSubMatrix K ke globalDofs

            if not (List.isEmpty assemblyErrors) then
                Error assemblyErrors
            else

            // --- Assemble F ---
            for load in loadCase.Loads do
                match dofMap.TryFind (load.NodeId, load.LocalDofIndex) with
                | Some gdof -> F.[gdof] <- F.[gdof] + load.Value
                | None      ->
                    // Node or DOF not mapped – skip (BC will pin it anyway)
                    ()

            Ok (DenseAssembledSystem(totalDofs, K, F) :> IAssembledSystem)
