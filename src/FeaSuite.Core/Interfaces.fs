namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Core interfaces for the FEA pipeline
// ---------------------------------------------------------------------------

/// Interface for an assembled global stiffness matrix and load vector.
type IAssembledSystem =
    /// Number of global degrees of freedom.
    abstract TotalDofs : int
    /// Global stiffness matrix K (dense representation, TotalDofs × TotalDofs).
    abstract K : float[,]
    /// Global load vector f (length TotalDofs).
    abstract F : float[]

/// Assembles the global stiffness matrix K and load vector f from the model.
type IAssembler =
    abstract Assemble : model: FEAModel * loadCase: LoadCase -> Validation<IAssembledSystem>

/// Solves the linear system K·u = f after applying boundary conditions.
type ILinearSolver =
    abstract Solve : system: IAssembledSystem * bcs: BoundaryCondition list * dofMap: DofMap -> Validation<float[]>

/// Configuration for the Newton-Raphson non-linear solver.
type NonlinearConfig = {
    MaxIterations     : int
    ResidualTolerance : float   // ||r|| < tol → convergence
    IncrementCount    : int     // number of load increments
}

module NonlinearConfig =
    let defaults = {
        MaxIterations     = 50
        ResidualTolerance = 1e-6
        IncrementCount    = 10
    }

/// Callback that computes the internal force vector and tangent stiffness
/// at a given displacement state (for non-linear elements).
type TangentProvider = float[] -> float[,] * float[]

/// Solves a non-linear system using incremental Newton-Raphson.
type INonlinearSolver =
    abstract Solve : assembler: IAssembler * model: FEAModel * loadCase: LoadCase * config: NonlinearConfig -> Validation<float[]>

/// Recovers engineering results (displacements, reactions, element stresses)
/// from a solved displacement vector.
type IResultRecovery =
    abstract RecoverDisplacements : displacements: float[] * dofMap: DofMap * model: FEAModel -> Map<NodeId, float[]>
    abstract RecoverReactions : displacements: float[] * system: IAssembledSystem * bcs: BoundaryCondition list * dofMap: DofMap -> Map<NodeId * int, float>

// ---------------------------------------------------------------------------
// Paged storage interfaces
// ---------------------------------------------------------------------------

/// A vector whose data may be stored on disk in pages.
/// Exposes get/set semantics and explicit flush/dispose.
type IPagedVector<'T> =
    inherit System.IDisposable
    abstract Length   : int
    abstract GetValue : index: int -> 'T
    abstract SetValue : index: int * value: 'T -> unit
    abstract Flush    : unit -> unit

/// Sparse matrix store that can page data to disk.
/// Stores non-zero (row, col, value) entries.
type IPagedMatrixStore =
    inherit System.IDisposable
    abstract Rows    : int
    abstract Cols    : int
    abstract Add     : row: int * col: int * value: float -> unit
    abstract Get     : row: int * col: int -> float
    abstract ToDense : unit -> float[,]
    abstract Flush   : unit -> unit
