namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Railway-Oriented Programming (ROP) types
// Compatible with GianFossi/ROP (https://github.com/GianFossi/ROP).
// When the ROP package is published to NuGet, replace these definitions with
// a package reference and open the ROP namespace.
// ---------------------------------------------------------------------------

/// All typed errors that can occur in the FEA pipeline.
type FEAError =
    | NodeNotFound          of nodeId: int
    | ElementNotFound       of elementId: int
    | MaterialNotFound      of materialId: int
    | DuplicateNodeId       of int
    | DuplicateElementId    of int
    | EmptyModel
    | SingularMatrix
    | NonConvergence        of iteration: int * residualNorm: float
    | InvalidInput          of message: string
    | IncompatibleDimensions of expected: int * actual: int
    | StorageError          of message: string
    | SolverError           of message: string
    | NotImplemented        of feature: string

/// A validation result: either a valid value or a list of errors.
/// Mirrors the Result-based Railway-Oriented design of GianFossi/ROP.
type Validation<'T> = Result<'T, FEAError list>

/// Operators and helpers for composing Validation pipelines.
module Validation =

    /// Lift a value into a successful Validation.
    let ok (value: 'T) : Validation<'T> = Ok value

    /// Lift a single error into a failed Validation.
    let fail (error: FEAError) : Validation<'T> = Error [ error ]

    /// Lift multiple errors into a failed Validation.
    let failMany (errors: FEAError list) : Validation<'T> = Error errors

    /// Map the success value.
    let map (f: 'T -> 'U) (v: Validation<'T>) : Validation<'U> = Result.map f v

    /// Bind / chain validations.
    let bind (f: 'T -> Validation<'U>) (v: Validation<'T>) : Validation<'U> = Result.bind f v

    /// Combine a list of validations, collecting all errors.
    let combine (results: Validation<'T> list) : Validation<'T list> =
        let mutable errors : FEAError list = []
        let mutable oks    : 'T list       = []
        for r in results do
            match r with
            | Ok v    -> oks   <- oks @ [ v ]
            | Error e -> errors <- errors @ e
        if List.isEmpty errors then Ok oks
        else Error errors

    /// Lift an Option into a Validation, using the supplied error when None.
    let ofOption (error: FEAError) (opt: 'T option) : Validation<'T> =
        match opt with
        | Some v -> Ok v
        | None   -> Error [ error ]

    /// Apply a predicate; return the value on success or a fail with the given error.
    let validate (predicate: 'T -> bool) (error: FEAError) (value: 'T) : Validation<'T> =
        if predicate value then Ok value else Error [ error ]

    /// Ignore the success value (map to unit).
    let ignore (v: Validation<'T>) : Validation<unit> = map (fun _ -> ()) v

    /// Run a side-effect on success; return the original Validation unchanged.
    let tap (f: 'T -> unit) (v: Validation<'T>) : Validation<'T> =
        match v with
        | Ok x -> f x; Ok x
        | err  -> err

    /// Computation expression support.
    type ValidationBuilder() =
        member _.Return(x)         = ok x
        member _.ReturnFrom(v)     = v
        member _.Bind(v, f)        = bind f v
        member _.Zero()            = ok ()
        member _.Delay(f)          = f
        member _.Run(f)            = f ()
        member _.Combine(v, f)     = bind (fun _ -> f ()) v

    let validation = ValidationBuilder()
