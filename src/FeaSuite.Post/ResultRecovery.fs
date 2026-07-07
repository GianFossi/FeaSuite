namespace FeaSuite.Post

open FeaSuite.Core
open FeaSuite.Solvers

// ---------------------------------------------------------------------------
// Result recovery from a solved displacement vector
// ---------------------------------------------------------------------------

module ResultRecovery =

    /// Extract nodal displacements from the global solution vector.
    let recoverDisplacements
            (uVec   : float[])
            (dofMap : DofMap)
            (model  : FEAModel)
            : NodalDisplacements =
        let values =
            model.Nodes
            |> Map.map (fun nid node ->
                [| for d in 0 .. node.DegreesOfFreedom - 1 do
                       match dofMap.TryFind (nid, d) with
                       | Some gdof -> yield uVec.[gdof]
                       | None      -> yield 0.0 |])
        { Values = values }

    /// Compute reaction forces: R = K·u − F (at constrained DOFs only).
    let recoverReactions
            (uVec   : float[])
            (system : IAssembledSystem)
            (bcs    : BoundaryCondition list)
            (dofMap : DofMap)
            : NodalReactions =
        let ku     = DenseMatrix.mulVec system.K uVec
        let values =
            bcs
            |> List.choose (fun bc ->
                match dofMap.TryFind (bc.NodeId, bc.LocalDofIndex) with
                | Some gdof -> Some ((bc.NodeId, bc.LocalDofIndex), ku.[gdof] - system.F.[gdof])
                | None      -> None)
            |> Map.ofList
        { Values = values }

    /// Compute element results for Bar1D elements.
    let recoverBar1DResults
            (uVec   : float[])
            (dofMap : DofMap)
            (model  : FEAModel)
            : ElementResult1D list =
        [ for KeyValue(_, elem) in model.Elements do
            if elem.Type = Beam Bar1D then
                match elem.NodeIds with
                | [ nidI; nidJ ] ->
                    match model.Materials.TryFind elem.MaterialId,
                          model.Nodes.TryFind nidI,
                          model.Nodes.TryFind nidJ with
                    | Some mat, Some nodeI, Some nodeJ ->
                        let L  = Point3D.distanceTo nodeI.Position nodeJ.Position
                        let A  = mat.CrossSectionArea |> Option.defaultValue 1.0
                        let E  = mat.YoungModulus
                        let uI = uVec.[dofMap.[(nidI, 0)]]
                        let uJ = uVec.[(dofMap.[(nidJ, 0)])]
                        let strain = (uJ - uI) / L
                        let stress = E * strain
                        let force  = stress * A
                        yield {
                            ElementId   = elem.Id
                            AxialForce  = force
                            AxialStress = stress
                            AxialStrain = strain
                        }
                    | _ -> ()
                | _ -> () ]

    /// Run complete post-processing given a SolveOutput from FeaPipeline.
    let postProcess
            (output  : FeaSuite.Solvers.SolveOutput)
            (system  : IAssembledSystem)
            (model   : FEAModel)
            (loadCase: LoadCase)
            : FEAResults =
        let dofMap, _ = FEAModel.buildDofMap model
        {
            Displacements  = recoverDisplacements output.RawVector dofMap model
            Reactions      = recoverReactions output.RawVector system loadCase.BoundaryConditions dofMap
            ElementResults = recoverBar1DResults output.RawVector dofMap model
        }
