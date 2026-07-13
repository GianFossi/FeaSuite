namespace FeaSuite.Post

open FeaSuite.Core
open FeaSuite.Solvers

// ---------------------------------------------------------------------------
// Result recovery from a solved displacement vector
// ---------------------------------------------------------------------------

module ResultRecovery =

    // -----------------------------------------------------------------------
    // Internal helpers
    // -----------------------------------------------------------------------

    /// Returns the local DOF index of the temperature degree of freedom for a
    /// given element type, or None if the element carries no thermal DOF.
    let private tempDofIndex (elemType: ElementType) : int option =
        match elemType with
        | Axisymmetric Plane75   -> Some 0   // 1 DOF: TEMP
        | Axisymmetric Plane78   -> Some 0   // 1 DOF: TEMP
        | Link Link31            -> Some 0   // 1 DOF: TEMP
        | Link Link33            -> Some 0   // 1 DOF: TEMP
        | Link Link34            -> Some 0   // 1 DOF: TEMP
        | Link Link68            -> Some 0   // 2 DOFs: [TEMP, VOLT]
        | Special Mass71         -> Some 0   // 1 DOF: TEMP
        | Special Fluid116       -> Some 1   // 2 DOFs: [PRES, TEMP]
        | Link Link228           -> Some 3   // 5 DOFs: [UX, UY, UZ, TEMP, VOLT]
        | Special Cpt212
        | Special Cpt213         -> Some 3   // 4 DOFs: [UX, UY, PRES, TEMP]
        | Special Cpt215
        | Special Cpt216
        | Special Cpt217         -> Some 4   // 5 DOFs: [UX, UY, UZ, PRES, TEMP]
        | Special Infin47        -> Some 1   // 2 DOFs: [MAG, TEMP]
        | Special Infin110       -> Some 2   // 3 DOFs: [AZ, VOLT, TEMP]
        | Special Infin111       -> Some 3   // 4 DOFs: [MAG, AZ, VOLT, TEMP]
        | Special Fluid220
        | Special Fluid221       -> Some 8   // 9 DOFs: [..., TEMP]
        | Special Fluid243
        | Special Fluid244       -> Some 6   // 7 DOFs: [..., TEMP]
        | _                      -> None     // no thermal DOF

    // -----------------------------------------------------------------------
    // 1. Nodal Forces / Flux
    // -----------------------------------------------------------------------

    /// Compute nodal internal forces (or heat flux for thermal elements) as
    /// f_int = K·u scattered back to individual nodes via the DOF map.
    let recoverNodalForces
            (uVec   : float[])
            (system : IAssembledSystem)
            (dofMap : DofMap)
            (model  : FEAModel)
            : NodalForces =
        let fInt = DenseMatrix.mulVec system.K uVec
        let values =
            model.Nodes
            |> Map.map (fun nid node ->
                [| for d in 0 .. node.DegreesOfFreedom - 1 do
                       match dofMap.TryFind (nid, d) with
                       | Some gdof -> yield fInt.[gdof]
                       | None      -> yield 0.0 |])
        { Values = values }

    // -----------------------------------------------------------------------
    // 2. Reaction Forces (exposed here for use in postProcess)
    // -----------------------------------------------------------------------

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

    // -----------------------------------------------------------------------
    // 3. Nodal Displacements and Temperatures
    // -----------------------------------------------------------------------

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

    /// Extract nodal temperatures from elements that carry a thermal DOF.
    /// For each node connected to a thermal element the temperature is read
    /// from the appropriate local DOF index (see tempDofIndex).
    let recoverTemperatures
            (uVec   : float[])
            (dofMap : DofMap)
            (model  : FEAModel)
            : NodalTemperatures =
        // Build node → thermal-DOF-index map from element connectivity.
        let mutable thermalDofs : Map<NodeId, int> = Map.empty
        for (_, elem) in Map.toSeq model.Elements do
            match tempDofIndex elem.Type with
            | None -> ()
            | Some tIdx ->
                for nid in elem.NodeIds do
                    if not (thermalDofs.ContainsKey nid) then
                        thermalDofs <- thermalDofs.Add(nid, tIdx)
        let values =
            thermalDofs
            |> Map.toSeq
            |> Seq.choose (fun (nid, tIdx) ->
                match dofMap.TryFind (nid, tIdx) with
                | Some gdof -> Some (nid, uVec.[gdof])
                | None      -> None)
            |> Map.ofSeq
        { Values = values }

    // -----------------------------------------------------------------------
    // 4 & 5 & 6. Stress, Strain, Von Mises, Tresca, Equivalent Strain,
    //            Plasticity Condition, and Cumulative Strain
    // -----------------------------------------------------------------------

    /// Build an ElementStressStrainResult for a uniaxial stress state.
    /// The full 3-D stress and strain tensors are expressed in global
    /// coordinates using direction cosines (l, m, n) of the element axis
    /// and the Poisson effect for transverse strains.
    let private makeUniaxialResult
            (eid      : ElementId)
            (l        : float)          // direction cosine X
            (m        : float)          // direction cosine Y
            (n        : float)          // direction cosine Z
            (eAxial   : float)          // axial strain
            (sAxial   : float)          // axial stress [Pa]
            (nu       : float)          // Poisson's ratio
            (E        : float)          // Young's modulus [Pa]
            (yieldSt  : float option)   // yield stress [Pa], None = not assessed
            : ElementStressStrainResult =

        // --- Stress tensor in global frame (σ_ij = l_i · l_j · σ_axial) ---
        let stress = {
            Sxx = l*l * sAxial
            Syy = m*m * sAxial
            Szz = n*n * sAxial
            Sxy = l*m * sAxial
            Syz = m*n * sAxial
            Sxz = l*n * sAxial
        }

        // --- Strain tensor in global frame ----------------------------------
        // Local principal strains: ε₁=eAxial, ε₂=ε₃=−ν·eAxial
        // After rotation: ε_global_ij = (1+ν)·l_i·l_j·eAxial − ν·eAxial·δ_ij
        let strain = {
            Exx = (1.0+nu)*l*l*eAxial - nu*eAxial
            Eyy = (1.0+nu)*m*m*eAxial - nu*eAxial
            Ezz = (1.0+nu)*n*n*eAxial - nu*eAxial
            // Engineering shear (γ = 2ε): 2·(1+ν)·lᵢ·lⱼ·eAxial
            Exy = 2.0*(1.0+nu)*l*m*eAxial
            Eyz = 2.0*(1.0+nu)*m*n*eAxial
            Exz = 2.0*(1.0+nu)*l*n*eAxial
        }

        // --- Derived scalars ------------------------------------------------
        let vonMises = abs sAxial      // uniaxial: σ_vm = |σ_axial|
        let tresca   = abs sAxial      // uniaxial: σ_T  = |σ_axial|
        let eqStrain = StrainTensor.vonMisesEquivalent strain

        // --- Plasticity & cumulative strain ---------------------------------
        let isPlastic, cumStrain =
            match yieldSt with
            | None ->
                false, 0.0
            | Some sy ->
                if vonMises > sy then
                    // Cumulative plastic strain estimated as ε_eq minus the elastic
                    // equivalent (σ_vm/E) for proportional single-step loading.
                    // For multi-step non-linear analyses this should be integrated
                    // over all converged load increments.
                    let eqElastic = sy / E
                    true, max 0.0 (eqStrain - eqElastic)
                else
                    false, 0.0

        { ElementId       = eid
          Stress          = stress
          Strain          = strain
          VonMisesStress  = vonMises
          TrescaStress    = tresca
          EquivalentStrain = eqStrain
          IsPlastic        = isPlastic
          CumulativeStrain = cumStrain }

    /// Recover stress/strain results for all Bar1D elements.
    let private recoverBar1DStressStrains
            (uVec        : float[])
            (dofMap      : DofMap)
            (model       : FEAModel)
            (yieldByMat  : Map<MaterialId, float>)
            : ElementStressStrainResult list =
        [ for (_, elem) in Map.toSeq model.Elements do
            if elem.Type = Beam Bar1D then
                match elem.NodeIds with
                | [ nidI; nidJ ] ->
                    match model.Materials.TryFind elem.MaterialId,
                          model.Nodes.TryFind nidI,
                          model.Nodes.TryFind nidJ with
                    | Some mat, Some nodeI, Some nodeJ ->
                        let L = Point3D.distanceTo nodeI.Position nodeJ.Position
                        let E = mat.YoungModulus
                        let nu = mat.PoissonRatio
                        let uI = uVec.[dofMap.[(nidI, 0)]]
                        let uJ = uVec.[dofMap.[(nidJ, 0)]]
                        let eAxial = (uJ - uI) / L
                        let sAxial = E * eAxial
                        let yieldSt = yieldByMat.TryFind elem.MaterialId
                        yield makeUniaxialResult elem.Id 1.0 0.0 0.0 eAxial sAxial nu E yieldSt
                    | _ -> ()
                | _ -> () ]

    /// Recover stress/strain results for all Truss3D elements.
    let private recoverTruss3DStressStrains
            (uVec        : float[])
            (dofMap      : DofMap)
            (model       : FEAModel)
            (yieldByMat  : Map<MaterialId, float>)
            : ElementStressStrainResult list =
        [ for (_, elem) in Map.toSeq model.Elements do
            if elem.Type = Beam Truss3D then
                match elem.NodeIds with
                | [ nidI; nidJ ] ->
                    match model.Materials.TryFind elem.MaterialId,
                          model.Nodes.TryFind nidI,
                          model.Nodes.TryFind nidJ with
                    | Some mat, Some nodeI, Some nodeJ ->
                        let axisVec = Point3D.vectorTo nodeI.Position nodeJ.Position
                        let L    = Vector3D.magnitude axisVec
                        let cosX = axisVec.Dx / L    // direction cosine X
                        let cosY = axisVec.Dy / L    // direction cosine Y
                        let cosZ = axisVec.Dz / L    // direction cosine Z
                        let E    = mat.YoungModulus
                        let nu   = mat.PoissonRatio
                        // Axial elongation: δ = (u_J − u_I) · axis_unit_vector
                        let uxI = uVec.[dofMap.[(nidI, 0)]]
                        let uyI = uVec.[dofMap.[(nidI, 1)]]
                        let uzI = uVec.[dofMap.[(nidI, 2)]]
                        let uxJ = uVec.[dofMap.[(nidJ, 0)]]
                        let uyJ = uVec.[dofMap.[(nidJ, 1)]]
                        let uzJ = uVec.[dofMap.[(nidJ, 2)]]
                        let delta  = cosX*(uxJ-uxI) + cosY*(uyJ-uyI) + cosZ*(uzJ-uzI)
                        let eAxial = delta / L
                        let sAxial = E * eAxial
                        let yieldSt = yieldByMat.TryFind elem.MaterialId
                        yield makeUniaxialResult elem.Id cosX cosY cosZ eAxial sAxial nu E yieldSt
                    | _ -> ()
                | _ -> () ]

    /// Recover general stress/strain results for all implemented element types,
    /// optionally assessing plasticity against the supplied yield-stress map.
    let recoverElementStressStrains
            (uVec        : float[])
            (dofMap      : DofMap)
            (model       : FEAModel)
            (yieldByMat  : Map<MaterialId, float>)
            : ElementStressStrainResult list =
        recoverBar1DStressStrains   uVec dofMap model yieldByMat @
        recoverTruss3DStressStrains uVec dofMap model yieldByMat

    // -----------------------------------------------------------------------
    // Legacy 1-D bar result recovery (kept for backward compatibility)
    // -----------------------------------------------------------------------

    /// Compute element results for Bar1D elements.
    let recoverBar1DResults
            (uVec   : float[])
            (dofMap : DofMap)
            (model  : FEAModel)
            : ElementResult1D list =
        [ for (_, elem) in Map.toSeq model.Elements do
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

    // -----------------------------------------------------------------------
    // Top-level post-processor
    // -----------------------------------------------------------------------

    /// Run complete post-processing given a SolveOutput from FeaPipeline.
    /// Plasticity is not assessed (IsPlastic = false, CumulativeStrain = 0).
    /// Use postProcessWithPlasticity to include yield-condition checking.
    let postProcess
            (output  : FeaSuite.Solvers.SolveOutput)
            (system  : IAssembledSystem)
            (model   : FEAModel)
            (loadCase: LoadCase)
            : FEAResults =
        let dofMap, _ = FEAModel.buildDofMap model
        {
            Displacements        = recoverDisplacements output.RawVector dofMap model
            Temperatures         = recoverTemperatures  output.RawVector dofMap model
            NodalForces          = recoverNodalForces   output.RawVector system dofMap model
            Reactions            = recoverReactions output.RawVector system loadCase.BoundaryConditions dofMap
            ElementResults       = recoverBar1DResults       output.RawVector dofMap model
            ElementStressStrains = recoverElementStressStrains output.RawVector dofMap model Map.empty
        }

    /// Run complete post-processing with plasticity assessment.
    /// yieldStressByMat maps MaterialId → yield stress [Pa].
    /// Elements whose Von Mises stress exceeds the yield stress are marked
    /// IsPlastic = true and a cumulative (plastic) strain estimate is given.
    let postProcessWithPlasticity
            (output         : FeaSuite.Solvers.SolveOutput)
            (system         : IAssembledSystem)
            (model          : FEAModel)
            (loadCase       : LoadCase)
            (yieldByMat     : Map<MaterialId, float>)
            : FEAResults =
        let dofMap, _ = FEAModel.buildDofMap model
        {
            Displacements        = recoverDisplacements output.RawVector dofMap model
            Temperatures         = recoverTemperatures  output.RawVector dofMap model
            NodalForces          = recoverNodalForces   output.RawVector system dofMap model
            Reactions            = recoverReactions output.RawVector system loadCase.BoundaryConditions dofMap
            ElementResults       = recoverBar1DResults       output.RawVector dofMap model
            ElementStressStrains = recoverElementStressStrains output.RawVector dofMap model yieldByMat
        }
