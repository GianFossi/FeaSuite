namespace FeaSuite.Post

open FeaSuite.Core

// ---------------------------------------------------------------------------
// FEA result types
// ---------------------------------------------------------------------------

/// Nodal displacement results.
type NodalDisplacements = {
    /// Map from NodeId to displacement vector (one entry per active DOF).
    Values : Map<NodeId, float[]>
}

/// Nodal reaction force results (at constrained DOFs).
type NodalReactions = {
    /// Map from (NodeId, localDofIndex) to reaction force/moment.
    Values : Map<NodeId * int, float>
}

/// Per-element stress / strain result for 1-D elements.
type ElementResult1D = {
    ElementId  : ElementId
    AxialForce : float   // N
    AxialStress : float  // Pa = N/A
    AxialStrain : float  // ε = σ/E
}

/// Container for all post-processing results.
type FEAResults = {
    Displacements   : NodalDisplacements
    Reactions       : NodalReactions
    ElementResults  : ElementResult1D list
}
