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
        let area =
            match e.Properties with
            | BarSection p -> p.Area
            | _            -> mat.CrossSectionArea |> Option.defaultValue 1.0
        let k = mat.YoungModulus * area / L
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
        let area =
            match e.Properties with
            | BarSection p -> p.Area
            | _            -> mat.CrossSectionArea |> Option.defaultValue 1.0
        let k  = mat.YoungModulus * area / L
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

    /// Euler-Bernoulli 2-D beam element stiffness (6×6) in global coordinates.
    ///   Requires Beam2DSection element properties.
    ///   DOF order per node: UX, UY, ROTZ.
    let beam2D (e: Element) (nodeI: Node) (nodeJ: Node) (mat: Material) : Validation<float[,]> =
        let v = Point3D.vectorTo nodeI.Position nodeJ.Position
        let L = Vector3D.magnitude v
        if L < 1e-15 then
            Validation.fail (InvalidInput (sprintf "Beam2D element %d: zero length." (ElementId.value e.Id)))
        else
        match e.Properties with
        | Beam2DSection props ->
            let ea  = mat.YoungModulus * props.Area
            let eiz = mat.YoungModulus * props.Iz
            let l   = v.Dx / L
            let m   = v.Dy / L
            let a   = ea  / L
            let c   = 12.0 * eiz / (L * L * L)
            let d   =  6.0 * eiz / (L * L)
            let e4  =  4.0 * eiz / L
            let e2  =  2.0 * eiz / L
            // Local stiffness — DOF order: u1 v1 θz1 u2 v2 θz2
            let kL  = Array2D.zeroCreate<float> 6 6
            kL.[0,0] <-  a;  kL.[0,3] <- -a
            kL.[3,0] <- -a;  kL.[3,3] <-  a
            kL.[1,1] <-  c;  kL.[1,2] <-  d;  kL.[1,4] <- -c;  kL.[1,5] <-  d
            kL.[2,1] <-  d;  kL.[2,2] <- e4;  kL.[2,4] <- -d;  kL.[2,5] <- e2
            kL.[4,1] <- -c;  kL.[4,2] <- -d;  kL.[4,4] <-  c;  kL.[4,5] <- -d
            kL.[5,1] <-  d;  kL.[5,2] <- e2;  kL.[5,4] <- -d;  kL.[5,5] <- e4
            // Transformation T: d_local = T * d_global
            let t   = Array2D.zeroCreate<float> 6 6
            t.[0,0] <-  l;  t.[0,1] <-  m
            t.[1,0] <- -m;  t.[1,1] <-  l
            t.[2,2] <- 1.0
            t.[3,3] <-  l;  t.[3,4] <-  m
            t.[4,3] <- -m;  t.[4,4] <-  l
            t.[5,5] <- 1.0
            // ke_global = T^T * kL * T
            let n   = 6
            let tmp = Array2D.zeroCreate<float> n n
            let ke  = Array2D.zeroCreate<float> n n
            for i in 0..n-1 do
                for j in 0..n-1 do
                    for k in 0..n-1 do
                        tmp.[i,j] <- tmp.[i,j] + kL.[i,k] * t.[k,j]
            for i in 0..n-1 do
                for j in 0..n-1 do
                    for k in 0..n-1 do
                        ke.[i,j] <- ke.[i,j] + t.[k,i] * tmp.[k,j]
            Ok ke
        | _ ->
            Validation.fail (InvalidInput (sprintf "Beam2D element %d: Properties must be Beam2DSection { Area; Iz }." (ElementId.value e.Id)))

    /// Euler-Bernoulli 3-D beam element stiffness (12×12) in global coordinates.
    ///   Requires Beam3DSection element properties.
    ///   Local y-axis lies in the plane of the element and global Z (falls back to
    ///   global X when the element axis is nearly parallel to Z).
    ///   DOF order per node: UX, UY, UZ, ROTX, ROTY, ROTZ.
    let beam3D (e: Element) (nodeI: Node) (nodeJ: Node) (mat: Material) : Validation<float[,]> =
        let v = Point3D.vectorTo nodeI.Position nodeJ.Position
        let L = Vector3D.magnitude v
        if L < 1e-15 then
            Validation.fail (InvalidInput (sprintf "Beam3D element %d: zero length." (ElementId.value e.Id)))
        else
        match e.Properties with
        | Beam3DSection props ->
            let E   = mat.YoungModulus
            let G   = E / (2.0 * (1.0 + mat.PoissonRatio))
            let ea  = E * props.Area / L
            let gj  = G * props.J    / L
            let cz  = 12.0 * E * props.Iz / (L*L*L)
            let dz  =  6.0 * E * props.Iz / (L*L)
            let ez4 =  4.0 * E * props.Iz / L
            let ez2 =  2.0 * E * props.Iz / L
            let cy  = 12.0 * E * props.Iy / (L*L*L)
            let dy  =  6.0 * E * props.Iy / (L*L)
            let ey4 =  4.0 * E * props.Iy / L
            let ey2 =  2.0 * E * props.Iy / L
            // Local 12×12 stiffness — DOF order: u1 v1 w1 θx1 θy1 θz1 u2 v2 w2 θx2 θy2 θz2
            let kL  = Array2D.zeroCreate<float> 12 12
            // Axial
            kL.[0, 0] <-  ea;  kL.[0, 6]  <- -ea
            kL.[6, 0] <- -ea;  kL.[6, 6]  <-  ea
            // Torsion
            kL.[3, 3] <-  gj;  kL.[3, 9]  <- -gj
            kL.[9, 3] <- -gj;  kL.[9, 9]  <-  gj
            // Bending in local XY (Iz) — DOFs: v(1,7), θz(5,11)
            kL.[1,  1] <-  cz;  kL.[1,  5] <-  dz;  kL.[1,  7] <- -cz;  kL.[1, 11] <-  dz
            kL.[5,  1] <-  dz;  kL.[5,  5] <- ez4;  kL.[5,  7] <- -dz;  kL.[5, 11] <- ez2
            kL.[7,  1] <- -cz;  kL.[7,  5] <- -dz;  kL.[7,  7] <-  cz;  kL.[7, 11] <- -dz
            kL.[11, 1] <-  dz;  kL.[11, 5] <- ez2;  kL.[11, 7] <- -dz;  kL.[11,11] <- ez4
            // Bending in local XZ (Iy) — DOFs: w(2,8), θy(4,10)  [θy = +dw/dx]
            kL.[2,  2] <-  cy;  kL.[2,  4] <-  dy;  kL.[2,  8] <- -cy;  kL.[2, 10] <-  dy
            kL.[4,  2] <-  dy;  kL.[4,  4] <- ey4;  kL.[4,  8] <- -dy;  kL.[4, 10] <- ey2
            kL.[8,  2] <- -cy;  kL.[8,  4] <- -dy;  kL.[8,  8] <-  cy;  kL.[8, 10] <- -dy
            kL.[10, 2] <-  dy;  kL.[10, 4] <- ey2;  kL.[10, 8] <- -dy;  kL.[10,10] <- ey4
            // Local axes: ex along element; ez = normalize(ex × ref); ey = ez × ex
            let cross (a: Vector3D) (b: Vector3D) : Vector3D =
                { Dx = a.Dy*b.Dz - a.Dz*b.Dy
                  Dy = a.Dz*b.Dx - a.Dx*b.Dz
                  Dz = a.Dx*b.Dy - a.Dy*b.Dx }
            let ex  = { Dx = v.Dx/L; Dy = v.Dy/L; Dz = v.Dz/L }
            let ref =
                if abs ex.Dz > 0.9 then { Dx = 1.0; Dy = 0.0; Dz = 0.0 }
                else                     { Dx = 0.0; Dy = 0.0; Dz = 1.0 }
            let ez  = Vector3D.normalize (cross ex ref)
            let ey  = cross ez ex
            // Block-diagonal T (12×12): each 3×3 block = [ex; ey; ez] in global coords
            let n   = 12
            let t   = Array2D.zeroCreate<float> n n
            for b in 0..3 do
                let o = b * 3
                t.[o,   o  ] <- ex.Dx;  t.[o,   o+1] <- ex.Dy;  t.[o,   o+2] <- ex.Dz
                t.[o+1, o  ] <- ey.Dx;  t.[o+1, o+1] <- ey.Dy;  t.[o+1, o+2] <- ey.Dz
                t.[o+2, o  ] <- ez.Dx;  t.[o+2, o+1] <- ez.Dy;  t.[o+2, o+2] <- ez.Dz
            // ke_global = T^T * kL * T
            let tmp = Array2D.zeroCreate<float> n n
            let ke  = Array2D.zeroCreate<float> n n
            for i in 0..n-1 do
                for j in 0..n-1 do
                    for k in 0..n-1 do
                        tmp.[i,j] <- tmp.[i,j] + kL.[i,k] * t.[k,j]
            for i in 0..n-1 do
                for j in 0..n-1 do
                    for k in 0..n-1 do
                        ke.[i,j] <- ke.[i,j] + t.[k,i] * tmp.[k,j]
            Ok ke
        | _ ->
            Validation.fail (InvalidInput (sprintf "Beam3D element %d: Properties must be Beam3DSection { Area; Iz; Iy; J }." (ElementId.value e.Id)))

    /// Dispatch to the correct stiffness routine for the given element type.
    let computeKe (e: Element) (nodes: Map<NodeId, Node>) (mat: Material)
                  : Validation<float[,] * int[]> =
        let twoNodeDispatch eid =
            match e.NodeIds with
            | [ nidI; nidJ ] ->
                match nodes.TryFind nidI, nodes.TryFind nidJ with
                | Some nI, Some nJ -> Ok (nI, nJ)
                | None, _          -> Validation.fail (NodeNotFound (NodeId.value nidI))
                | _,    None       -> Validation.fail (NodeNotFound (NodeId.value nidJ))
            | _ -> Validation.fail (InvalidInput (sprintf "Element %d must have exactly 2 nodes." eid))
        match e.Type with
        | Beam Bar1D ->
            twoNodeDispatch (ElementId.value e.Id)
            |> Result.map (fun (nI, nJ) -> bar1D e nI nJ mat, [| 0; 1 |])

        | Beam Truss3D ->
            twoNodeDispatch (ElementId.value e.Id)
            |> Result.map (fun (nI, nJ) -> truss3D e nI nJ mat, [| 0; 1; 2; 3; 4; 5 |])

        | Beam Beam2D ->
            twoNodeDispatch (ElementId.value e.Id)
            |> Result.bind (fun (nI, nJ) ->
                beam2D e nI nJ mat
                |> Result.map (fun ke -> ke, [| 0; 1; 2; 3; 4; 5 |]))

        | Beam Beam3D ->
            twoNodeDispatch (ElementId.value e.Id)
            |> Result.bind (fun (nI, nJ) ->
                beam3D e nI nJ mat
                |> Result.map (fun ke -> ke, [| 0..11 |]))

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
