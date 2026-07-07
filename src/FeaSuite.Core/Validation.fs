namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Model validation using ROP / Validation<'T>
// ---------------------------------------------------------------------------

module ModelValidation =

    /// Validate that all element node references exist in the model.
    let private checkElementNodeRefs (model: FEAModel) : Validation<unit> =
        let errors =
            [ for KeyValue(_, elem) in model.Elements do
                for nid in elem.NodeIds do
                    if not (model.Nodes.ContainsKey nid) then
                        yield NodeNotFound (NodeId.value nid) ]
        if List.isEmpty errors then Ok ()
        else Error errors

    /// Validate that all element material references exist.
    let private checkElementMaterialRefs (model: FEAModel) : Validation<unit> =
        let errors =
            [ for KeyValue(_, elem) in model.Elements do
                if not (model.Materials.ContainsKey elem.MaterialId) then
                    yield MaterialNotFound (MaterialId.value elem.MaterialId) ]
        if List.isEmpty errors then Ok ()
        else Error errors

    /// Validate that load case node references exist.
    let private checkLoadCaseRefs (model: FEAModel) : Validation<unit> =
        let errors =
            [ for lc in model.LoadCases do
                for load in lc.Loads do
                    if not (model.Nodes.ContainsKey load.NodeId) then
                        yield NodeNotFound (NodeId.value load.NodeId)
                for bc in lc.BoundaryConditions do
                    if not (model.Nodes.ContainsKey bc.NodeId) then
                        yield NodeNotFound (NodeId.value bc.NodeId) ]
        if List.isEmpty errors then Ok ()
        else Error errors

    /// Validate that the model has at least one node and one element.
    let private checkNotEmpty (model: FEAModel) : Validation<unit> =
        if model.Nodes.IsEmpty || model.Elements.IsEmpty then
            Validation.fail EmptyModel
        else Ok ()

    /// Validate material properties (E > 0, -1 < ν < 0.5).
    let private checkMaterialProps (model: FEAModel) : Validation<unit> =
        let errors =
            [ for KeyValue(_, mat) in model.Materials do
                if mat.YoungModulus <= 0.0 then
                    yield InvalidInput (sprintf "Material '%s': YoungModulus must be > 0." mat.Name)
                if mat.PoissonRatio <= -1.0 || mat.PoissonRatio >= 0.5 then
                    yield InvalidInput (sprintf "Material '%s': PoissonRatio must be in (-1, 0.5)." mat.Name) ]
        if List.isEmpty errors then Ok ()
        else Error errors

    /// Run all validations; collect every error found.
    let validate (model: FEAModel) : Validation<FEAModel> =
        let checks : Validation<unit> list =
            [ checkNotEmpty          model
              checkElementNodeRefs   model
              checkElementMaterialRefs model
              checkLoadCaseRefs      model
              checkMaterialProps     model ]
        let errors =
            checks
            |> List.collect (function Error e -> e | Ok _ -> [])
        if List.isEmpty errors then Ok model
        else Error errors
