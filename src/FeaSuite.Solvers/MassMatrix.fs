namespace FeaSuite.Solvers

open FeaSuite.Core

// ---------------------------------------------------------------------------
// Lumped mass matrix assembly for Bar1D and Truss3D elements.
//
// Lumped (diagonal) mass matrix: the element mass m_e = ρ·A·L is split
// equally between the two end nodes.  Each node therefore receives m_e/2
// for every one of its translational DOFs.
//
//   Bar1D   (1 DOF/node): M_lump = diag(m_e/2, m_e/2)
//   Truss3D (3 DOFs/node): M_lump = diag(m_e/2 × 1₆)
//
// The assembled global mass vector M[i] gives the diagonal entry of the
// global lumped mass matrix at global DOF i.
// ---------------------------------------------------------------------------

module ElementMass =

    /// Lumped mass contributed to each end node for a Bar1D element.
    /// m_e = ρ · A · L  →  m_e / 2 per node.
    let bar1DPerNode (nodeI: Node) (nodeJ: Node) (mat: Material) : float =
        let L = Point3D.distanceTo nodeI.Position nodeJ.Position
        let A = mat.CrossSectionArea |> Option.defaultValue 1.0
        mat.Density * A * L / 2.0

    /// Lumped mass contributed to each end node for a Truss3D element.
    let truss3DPerNode (nodeI: Node) (nodeJ: Node) (mat: Material) : float =
        let v = Point3D.vectorTo nodeI.Position nodeJ.Position
        let L = Vector3D.magnitude v
        let A = mat.CrossSectionArea |> Option.defaultValue 1.0
        mat.Density * A * L / 2.0

    /// Compute the lumped mass contributions for a single element and return
    /// a list of (global DOF index, mass value) pairs to be summed into M.
    let computeMe
            (e      : Element)
            (nodes  : Map<NodeId, Node>)
            (dofMap : DofMap)
            (mat    : Material)
            : Validation<(int * float) list> =

        match e.Type with
        | Beam Bar1D ->
            match e.NodeIds with
            | [ nidI; nidJ ] ->
                match nodes.TryFind nidI, nodes.TryFind nidJ with
                | Some nI, Some nJ ->
                    let m = bar1DPerNode nI nJ mat
                    let dofsI = [ for d in 0 .. nI.DegreesOfFreedom - 1 -> dofMap.[(nidI, d)], m ]
                    let dofsJ = [ for d in 0 .. nJ.DegreesOfFreedom - 1 -> dofMap.[(nidJ, d)], m ]
                    Ok (dofsI @ dofsJ)
                | None, _ -> Validation.fail (NodeNotFound (NodeId.value nidI))
                | _, None -> Validation.fail (NodeNotFound (NodeId.value nidJ))
            | _ ->
                Validation.fail (InvalidInput (sprintf "Bar1D element %d must have exactly 2 nodes."
                                                       (ElementId.value e.Id)))

        | Beam Truss3D ->
            match e.NodeIds with
            | [ nidI; nidJ ] ->
                match nodes.TryFind nidI, nodes.TryFind nidJ with
                | Some nI, Some nJ ->
                    let m = truss3DPerNode nI nJ mat
                    let dofsI = [ for d in 0 .. nI.DegreesOfFreedom - 1 -> dofMap.[(nidI, d)], m ]
                    let dofsJ = [ for d in 0 .. nJ.DegreesOfFreedom - 1 -> dofMap.[(nidJ, d)], m ]
                    Ok (dofsI @ dofsJ)
                | None, _ -> Validation.fail (NodeNotFound (NodeId.value nidI))
                | _, None -> Validation.fail (NodeNotFound (NodeId.value nidJ))
            | _ ->
                Validation.fail (InvalidInput (sprintf "Truss3D element %d must have exactly 2 nodes."
                                                       (ElementId.value e.Id)))

        | t ->
            Validation.fail (NotImplemented
                (sprintf "Lumped mass for element type %A is not yet implemented." t))


/// Assembles the global diagonal lumped mass vector for a model.
module MassAssembler =

    /// Build the global diagonal lumped mass vector M (length = totalDofs).
    /// M.[i] accumulates the lumped mass at global DOF i from all elements.
    let assemble (model: FEAModel) : Validation<float[]> =
        let dofMap, totalDofs = FEAModel.buildDofMap model
        let M = Array.zeroCreate<float> totalDofs

        let mutable errors : FEAError list = []

        for KeyValue(_, elem) in model.Elements do
            match model.Materials.TryFind elem.MaterialId with
            | None ->
                errors <- errors @ [ MaterialNotFound (MaterialId.value elem.MaterialId) ]
            | Some mat ->
                match ElementMass.computeMe elem model.Nodes dofMap mat with
                | Error errs -> errors <- errors @ errs
                | Ok massEntries ->
                    for (gdof, m) in massEntries do
                        M.[gdof] <- M.[gdof] + m

        if not (List.isEmpty errors) then Error errors
        else Ok M
