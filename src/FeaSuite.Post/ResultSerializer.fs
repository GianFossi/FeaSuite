namespace FeaSuite.Post

open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open FeaSuite.Core
open FeaSuite.Storage

// ---------------------------------------------------------------------------
// ResultSerializer – save / load FEAResults to / from a JSON file.
//
// Uses System.Text.Json.Nodes (JsonObject / JsonArray) directly to avoid
// F# record serialisation limitations with System.Text.Json.
// ---------------------------------------------------------------------------

module ResultSerializer =

    let private jsonOpts = JsonSerializerOptions(WriteIndented = true)

    // -----------------------------------------------------------------------
    // Domain → JsonObject
    // -----------------------------------------------------------------------

    let private resultsToJson (r: FEAResults) =
        let root = JsonObject()

        let disps = JsonArray()
        for KeyValue(nid, vals) in r.Displacements.Values do
            let o = JsonObject()
            o["NodeId"] <- JsonValue.Create(NodeId.value nid)
            let arr = JsonArray()
            for v in vals do arr.Add(JsonValue.Create(v))
            o["Values"] <- arr
            disps.Add(o)
        root["NodalDisplacements"] <- disps

        let reacts = JsonArray()
        for KeyValue((nid, dof), v) in r.Reactions.Values do
            let o = JsonObject()
            o["NodeId"]        <- JsonValue.Create(NodeId.value nid)
            o["LocalDofIndex"] <- JsonValue.Create(dof)
            o["Value"]         <- JsonValue.Create(v)
            reacts.Add(o)
        root["NodalReactions"] <- reacts

        let elems = JsonArray()
        for e in r.ElementResults do
            let o = JsonObject()
            o["ElementId"]   <- JsonValue.Create(ElementId.value e.ElementId)
            o["AxialForce"]  <- JsonValue.Create(e.AxialForce)
            o["AxialStress"] <- JsonValue.Create(e.AxialStress)
            o["AxialStrain"] <- JsonValue.Create(e.AxialStrain)
            elems.Add(o)
        root["ElementResults"] <- elems
        root

    // -----------------------------------------------------------------------
    // JsonObject → Domain
    // -----------------------------------------------------------------------

    let private resultsOfJson (root: JsonObject) : FEAResults =
        let displacements : NodalDisplacements = {
            Values =
                (root["NodalDisplacements"] :?> JsonArray)
                |> Seq.map (fun n ->
                    let o   = n :?> JsonObject
                    let nid = NodeId (o["NodeId"].GetValue<int>())
                    let vals =
                        (o["Values"] :?> JsonArray)
                        |> Seq.map (fun v -> v.GetValue<float>())
                        |> Array.ofSeq
                    nid, vals)
                |> Map.ofSeq
        }
        let reactions : NodalReactions = {
            Values =
                (root["NodalReactions"] :?> JsonArray)
                |> Seq.map (fun n ->
                    let o   = n :?> JsonObject
                    let key = NodeId (o["NodeId"].GetValue<int>()), o["LocalDofIndex"].GetValue<int>()
                    key, o["Value"].GetValue<float>())
                |> Map.ofSeq
        }
        let elemResults =
            (root["ElementResults"] :?> JsonArray)
            |> Seq.map (fun n ->
                let o = n :?> JsonObject
                { ElementId   = ElementId (o["ElementId"].GetValue<int>())
                  AxialForce  = o["AxialForce"].GetValue<float>()
                  AxialStress = o["AxialStress"].GetValue<float>()
                  AxialStrain = o["AxialStrain"].GetValue<float>() })
            |> Seq.toList
        { Displacements   = displacements
          Reactions        = reactions
          ElementResults   = elemResults }

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /// Serialise an FEAResults value to a JSON file at <paramref name="path"/>.
    let saveResults (path: string) (results: FEAResults) : Validation<unit> =
        try
            let json = (resultsToJson results).ToJsonString(jsonOpts)
            File.WriteAllText(path, json)
            Ok ()
        with ex ->
            Error [ StorageError ex.Message ]

    /// Deserialise an FEAResults value from a JSON file at <paramref name="path"/>.
    let loadResults (path: string) : Validation<FEAResults> =
        try
            if not (File.Exists path) then
                Error [ StorageError (sprintf "Results file not found: %s" path) ]
            else
                let json = File.ReadAllText path
                let root = JsonNode.Parse(json) :?> JsonObject
                Ok (resultsOfJson root)
        with ex ->
            Error [ StorageError ex.Message ]
