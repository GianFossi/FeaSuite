namespace FeaSuite.Solvers

open FeaSuite.Core

// ---------------------------------------------------------------------------
// Non-linear solver: incremental Newton-Raphson
//
// Algorithm (for each load increment λ_i):
//   1. Apply incremental load: F_ext = λ_i · F_total
//   2. Newton loop until ||r_free|| < tol or iter > maxIter:
//      a. Compute f_int = K · u  (for linear elems; extendable to nonlinear)
//      b. Residual r = F_ext − f_int
//      c. Build correction system: K_copy with BCs applied as Δu = 0
//         at constrained DOFs (correction BCs), r[constrained] = 0
//      d. Solve K_copy · Δu = r
//      e. Update u ← u + Δu
//   3. If not converged → return NonConvergence error.
// ---------------------------------------------------------------------------

module private NRHelpers =
    /// Apply "correction" BCs to a copy of K and the residual r.
    /// For each constrained DOF, we enforce Δu = 0 by zeroing the row/col
    /// and setting diagonal = 1, rhs = 0 (the increment at a BC DOF is zero
    /// because the displacement is already set to its prescribed value).
    let applyBCsToCorrection
            (K    : float[,])
            (r    : float[])
            (bcs  : BoundaryCondition list)
            (dofMap : DofMap)
            : unit =
        let n = Array2D.length1 K
        for bc in bcs do
            match dofMap.TryFind (bc.NodeId, bc.LocalDofIndex) with
            | Some gdof ->
                for k in 0 .. n - 1 do
                    K.[gdof, k] <- 0.0
                    K.[k, gdof] <- 0.0
                K.[gdof, gdof] <- 1.0
                r.[gdof]       <- 0.0      // correction = 0 at constrained DOF
            | None -> ()

type NewtonRaphsonSolver() =
    interface INonlinearSolver with
        member _.Solve(assembler, model, loadCase, config) =

            let dofMap, totalDofs = FEAModel.buildDofMap model

            // Assemble constant reference stiffness K and full load F
            match assembler.Assemble(model, loadCase) with
            | Error e -> Error e
            | Ok refSystem ->

            let F_total  = Array.copy refSystem.F
            let K_ref    = refSystem.K          // reference stiffness (shared)
            let u        = Array.zeroCreate totalDofs

            // Apply initial prescribed displacements
            for bc in loadCase.BoundaryConditions do
                match dofMap.TryFind (bc.NodeId, bc.LocalDofIndex) with
                | Some gdof ->
                    u.[gdof] <-
                        match bc.Constraint with
                        | Fixed            -> 0.0
                        | Prescribed value -> value
                | None -> ()

            let mutable solveResult : Validation<float[]> = Ok u
            let mutable continueLoop = true
            let mutable incrIdx = 0

            while continueLoop && incrIdx < config.IncrementCount do
                let lambda = float (incrIdx + 1) / float config.IncrementCount
                let F_ext  = F_total |> Array.map (fun v -> lambda * v)

                let mutable iterConverged = false
                let mutable iter          = 0

                while not iterConverged && iter < config.MaxIterations do
                    // f_int = K · u
                    let f_int = DenseMatrix.mulVec K_ref u

                    // Residual r = F_ext − f_int
                    let r = Array.map2 (fun fe fi -> fe - fi) F_ext f_int

                    // Norm of free-DOF residual (excluding constrained DOFs)
                    let constrainedGdofs =
                        loadCase.BoundaryConditions
                        |> List.choose (fun bc -> dofMap.TryFind (bc.NodeId, bc.LocalDofIndex))
                        |> Set.ofList
                    let rFreeNorm =
                        r
                        |> Array.indexed
                        |> Array.filter (fun (i, _) -> not (constrainedGdofs.Contains i))
                        |> Array.sumBy (fun (_, v) -> v * v)
                        |> sqrt

                    if rFreeNorm < config.ResidualTolerance then
                        iterConverged <- true
                    else
                        // Solve correction system: K_copy · Δu = r (with correction BCs)
                        let K_copy = DenseMatrix.copy K_ref
                        let r_copy = Array.copy r
                        NRHelpers.applyBCsToCorrection K_copy r_copy loadCase.BoundaryConditions dofMap
                        match GaussianElimination.solve K_copy r_copy with
                        | Error e ->
                            solveResult  <- Error e
                            iterConverged <- true
                            continueLoop <- false
                        | Ok du ->
                            for d in 0 .. totalDofs - 1 do
                                u.[d] <- u.[d] + du.[d]

                    iter <- iter + 1

                if not iterConverged && continueLoop then
                    let f_int = DenseMatrix.mulVec K_ref u
                    let r = Array.map2 (fun fe fi -> fe - fi) F_ext f_int
                    let rNorm = DenseMatrix.norm r
                    solveResult  <- Error [ NonConvergence (iter, rNorm) ]
                    continueLoop <- false

                incrIdx <- incrIdx + 1

            match solveResult with
            | Ok _ -> Ok u
            | err  -> err
