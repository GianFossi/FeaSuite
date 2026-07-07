namespace FeaSuite.Solvers

open FeaSuite.Core

// ---------------------------------------------------------------------------
// End-to-end FEA pipeline
// Validate → Assemble → Solve → Recover results
// ---------------------------------------------------------------------------

/// Input for a single-load-case solve.
type SolveInput = {
    Model          : FEAModel
    LoadCaseIndex  : int        // index into Model.LoadCases
    UseNonlinear   : bool
    NonlinearConfig : NonlinearConfig
}

/// Result of a successful solve.
type SolveOutput = {
    Displacements : Map<NodeId, float[]>
    Reactions     : Map<NodeId * int, float>
    RawVector     : float[]
    TotalDofs     : int
}

module FeaPipeline =

    let private assembler    = DenseAssembler()    :> IAssembler
    let private linSolver    = DenseLinearSolver() :> ILinearSolver
    let private nlSolver     = NewtonRaphsonSolver():> INonlinearSolver

    /// Run the full FEA pipeline for the given input.
    let run (input: SolveInput) : Validation<SolveOutput> =
        // 1. Validate model
        match ModelValidation.validate input.Model with
        | Error e -> Error e
        | Ok model ->

        // 2. Pick load case
        let loadCases = model.LoadCases
        if input.LoadCaseIndex < 0 || input.LoadCaseIndex >= List.length loadCases then
            Validation.fail (InvalidInput "LoadCaseIndex out of range.")
        else

        let loadCase = loadCases.[input.LoadCaseIndex]
        let dofMap, totalDofs = FEAModel.buildDofMap model

        // 3. Assemble (need original K for reaction recovery)
        match assembler.Assemble(model, loadCase) with
        | Error e -> Error e
        | Ok system ->

        // 4. Solve
        let solveResult =
            if input.UseNonlinear then
                nlSolver.Solve(assembler, model, loadCase, input.NonlinearConfig)
            else
                linSolver.Solve(system, loadCase.BoundaryConditions, dofMap)

        match solveResult with
        | Error e -> Error e
        | Ok uVec ->

        // 5. Recover displacements
        let displacements =
            model.Nodes
            |> Map.map (fun nid node ->
                [| for d in 0 .. node.DegreesOfFreedom - 1 do
                       let gdof = dofMap.[(nid, d)]
                       yield uVec.[gdof] |])

        // 6. Recover reactions R = K·u − F (at constrained DOFs)
        let ku = DenseMatrix.mulVec system.K uVec
        let reactions =
            [ for bc in loadCase.BoundaryConditions do
                  match dofMap.TryFind (bc.NodeId, bc.LocalDofIndex) with
                  | Some gdof ->
                      let reaction = ku.[gdof] - system.F.[gdof]
                      yield (bc.NodeId, bc.LocalDofIndex), reaction
                  | None -> () ]
            |> Map.ofList

        Ok {
            Displacements = displacements
            Reactions     = reactions
            RawVector     = uVec
            TotalDofs     = totalDofs
        }
