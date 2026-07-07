module FeaSuite.Tests.ModelValidationTests

open Xunit
open FeaSuite.Core

// ---------------------------------------------------------------------------
// Model validation tests (ROP / Validation<_>)
// ---------------------------------------------------------------------------

[<Fact>]
let ``validate accepts a well-formed model`` () =
    let model, _ = Helpers.buildBar1DModel 2e11 1e-4 1.0 1000.0
    match ModelValidation.validate model with
    | Ok _    -> ()  // expected
    | Error e -> failwith (sprintf "Unexpected errors: %A" e)

[<Fact>]
let ``validate rejects an empty model`` () =
    let model = FEAModel.empty
    match ModelValidation.validate model with
    | Error errs -> Assert.Contains(EmptyModel, errs)
    | Ok _       -> failwith "Expected EmptyModel error"

[<Fact>]
let ``validate detects missing material reference`` () =
    let n1 = { Id = NodeId 1; Position = Point3D.ofXY 0.0 0.0; DegreesOfFreedom = 1 }
    let n2 = { Id = NodeId 2; Position = Point3D.ofXY 1.0 0.0; DegreesOfFreedom = 1 }
    let el = {
        Id         = ElementId 1
        Type       = Beam Bar1D
        NodeIds    = [ NodeId 1; NodeId 2 ]
        MaterialId = MaterialId 99  // does not exist
        Properties = Map.empty
    }
    let model =
        FEAModel.empty
        |> FEAModel.addNode n1
        |> FEAModel.addNode n2
        |> FEAModel.addElement el
    match ModelValidation.validate model with
    | Error errs ->
        let found = errs |> List.exists (function MaterialNotFound 99 -> true | _ -> false)
        Assert.True(found, sprintf "Expected MaterialNotFound 99, got: %A" errs)
    | Ok _ -> failwith "Expected MaterialNotFound error"

[<Fact>]
let ``validate detects missing node reference in element`` () =
    let mat = {
        Id = MaterialId 1; Name = "M"; YoungModulus = 1e9; PoissonRatio = 0.3
        Density = 1.0; CrossSectionArea = Some 1e-4
    }
    let n1 = { Id = NodeId 1; Position = Point3D.ofXY 0.0 0.0; DegreesOfFreedom = 1 }
    let el = {
        Id         = ElementId 1
        Type       = Beam Bar1D
        NodeIds    = [ NodeId 1; NodeId 99 ]  // NodeId 99 missing
        MaterialId = MaterialId 1
        Properties = Map.empty
    }
    let model =
        FEAModel.empty
        |> FEAModel.addMaterial mat
        |> FEAModel.addNode n1
        |> FEAModel.addElement el
    match ModelValidation.validate model with
    | Error errs ->
        let found = errs |> List.exists (function NodeNotFound 99 -> true | _ -> false)
        Assert.True(found, sprintf "Expected NodeNotFound 99, got: %A" errs)
    | Ok _ -> failwith "Expected NodeNotFound error"

[<Fact>]
let ``validate detects invalid Young's modulus`` () =
    let mat = {
        Id = MaterialId 1; Name = "Bad"; YoungModulus = -1.0; PoissonRatio = 0.3
        Density = 1.0; CrossSectionArea = Some 1e-4
    }
    let n1 = { Id = NodeId 1; Position = Point3D.ofXY 0.0 0.0; DegreesOfFreedom = 1 }
    let n2 = { Id = NodeId 2; Position = Point3D.ofXY 1.0 0.0; DegreesOfFreedom = 1 }
    let el = {
        Id = ElementId 1; Type = Beam Bar1D; NodeIds = [NodeId 1; NodeId 2]
        MaterialId = MaterialId 1; Properties = Map.empty
    }
    let model =
        FEAModel.empty
        |> FEAModel.addMaterial mat
        |> FEAModel.addNode n1
        |> FEAModel.addNode n2
        |> FEAModel.addElement el
    match ModelValidation.validate model with
    | Error errs ->
        let found = errs |> List.exists (function InvalidInput _ -> true | _ -> false)
        Assert.True(found, sprintf "Expected InvalidInput for bad E, got: %A" errs)
    | Ok _ -> failwith "Expected InvalidInput error"
