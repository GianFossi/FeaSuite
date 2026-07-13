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

        // --- NodalDisplacements ---
        let disps = JsonArray()
        for nid, vals in Map.toSeq r.Displacements.Values do
            let o = JsonObject()
            o["NodeId"] <- JsonValue.Create(NodeId.value nid)
            let arr = JsonArray()
            for v in vals do arr.Add(JsonValue.Create(v))
            o["Values"] <- arr
            disps.Add(o)
        root["NodalDisplacements"] <- disps

        // --- NodalTemperatures ---
        let temps = JsonArray()
        for nid, temp in Map.toSeq r.Temperatures.Values do
            let o = JsonObject()
            o["NodeId"]      <- JsonValue.Create(NodeId.value nid)
            o["Temperature"] <- JsonValue.Create(temp)
            temps.Add(o)
        root["NodalTemperatures"] <- temps

        // --- NodalForces ---
        let forces = JsonArray()
        for nid, vals in Map.toSeq r.NodalForces.Values do
            let o = JsonObject()
            o["NodeId"] <- JsonValue.Create(NodeId.value nid)
            let arr = JsonArray()
            for v in vals do arr.Add(JsonValue.Create(v))
            o["Values"] <- arr
            forces.Add(o)
        root["NodalForces"] <- forces

        // --- NodalReactions ---
        let reacts = JsonArray()
        for (nid, dof), v in Map.toSeq r.Reactions.Values do
            let o = JsonObject()
            o["NodeId"]        <- JsonValue.Create(NodeId.value nid)
            o["LocalDofIndex"] <- JsonValue.Create(dof)
            o["Value"]         <- JsonValue.Create(v)
            reacts.Add(o)
        root["NodalReactions"] <- reacts

        // --- ElementResults (legacy 1-D) ---
        let elems = JsonArray()
        for e in r.ElementResults do
            let o = JsonObject()
            o["ElementId"]   <- JsonValue.Create(ElementId.value e.ElementId)
            o["AxialForce"]  <- JsonValue.Create(e.AxialForce)
            o["AxialStress"] <- JsonValue.Create(e.AxialStress)
            o["AxialStrain"] <- JsonValue.Create(e.AxialStrain)
            elems.Add(o)
        root["ElementResults"] <- elems

        // --- ElementStressStrains ---
        let stresses = JsonArray()
        for e in r.ElementStressStrains do
            let o = JsonObject()
            o["ElementId"]        <- JsonValue.Create(ElementId.value e.ElementId)
            o["Sxx"]              <- JsonValue.Create(e.Stress.Sxx)
            o["Syy"]              <- JsonValue.Create(e.Stress.Syy)
            o["Szz"]              <- JsonValue.Create(e.Stress.Szz)
            o["Sxy"]              <- JsonValue.Create(e.Stress.Sxy)
            o["Syz"]              <- JsonValue.Create(e.Stress.Syz)
            o["Sxz"]              <- JsonValue.Create(e.Stress.Sxz)
            o["Exx"]              <- JsonValue.Create(e.Strain.Exx)
            o["Eyy"]              <- JsonValue.Create(e.Strain.Eyy)
            o["Ezz"]              <- JsonValue.Create(e.Strain.Ezz)
            o["Exy"]              <- JsonValue.Create(e.Strain.Exy)
            o["Eyz"]              <- JsonValue.Create(e.Strain.Eyz)
            o["Exz"]              <- JsonValue.Create(e.Strain.Exz)
            o["VonMisesStress"]   <- JsonValue.Create(e.VonMisesStress)
            o["TrescaStress"]     <- JsonValue.Create(e.TrescaStress)
            o["EquivalentStrain"] <- JsonValue.Create(e.EquivalentStrain)
            o["IsPlastic"]        <- JsonValue.Create(e.IsPlastic)
            o["CumulativeStrain"] <- JsonValue.Create(e.CumulativeStrain)
            stresses.Add(o)
        root["ElementStressStrains"] <- stresses

        root

    // -----------------------------------------------------------------------
    // JsonObject → Domain
    // -----------------------------------------------------------------------

    let private flt (o: JsonObject) (key: string) = o[key].GetValue<float>()
    let private bl  (o: JsonObject) (key: string) = o[key].GetValue<bool>()

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

        let temperatures : NodalTemperatures = {
            Values =
                match root["NodalTemperatures"] with
                | null -> Map.empty
                | node ->
                    (node :?> JsonArray)
                    |> Seq.map (fun n ->
                        let o = n :?> JsonObject
                        NodeId (o["NodeId"].GetValue<int>()), flt o "Temperature")
                    |> Map.ofSeq
        }

        let nodalForces : NodalForces = {
            Values =
                match root["NodalForces"] with
                | null -> Map.empty
                | node ->
                    (node :?> JsonArray)
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
                    key, flt o "Value")
                |> Map.ofSeq
        }

        let elemResults =
            (root["ElementResults"] :?> JsonArray)
            |> Seq.map (fun n ->
                let o = n :?> JsonObject
                { ElementId   = ElementId (o["ElementId"].GetValue<int>())
                  AxialForce  = flt o "AxialForce"
                  AxialStress = flt o "AxialStress"
                  AxialStrain = flt o "AxialStrain" })
            |> Seq.toList

        let elemStressStrains =
            match root["ElementStressStrains"] with
            | null -> []
            | node ->
                (node :?> JsonArray)
                |> Seq.map (fun n ->
                    let o = n :?> JsonObject
                    { ElementId       = ElementId (o["ElementId"].GetValue<int>())
                      Stress          = { Sxx = flt o "Sxx"; Syy = flt o "Syy"; Szz = flt o "Szz"
                                          Sxy = flt o "Sxy"; Syz = flt o "Syz"; Sxz = flt o "Sxz" }
                      Strain          = { Exx = flt o "Exx"; Eyy = flt o "Eyy"; Ezz = flt o "Ezz"
                                          Exy = flt o "Exy"; Eyz = flt o "Eyz"; Exz = flt o "Exz" }
                      VonMisesStress   = flt o "VonMisesStress"
                      TrescaStress     = flt o "TrescaStress"
                      EquivalentStrain = flt o "EquivalentStrain"
                      IsPlastic        = bl  o "IsPlastic"
                      CumulativeStrain = flt o "CumulativeStrain" })
                |> Seq.toList

        { Displacements        = displacements
          Temperatures         = temperatures
          NodalForces          = nodalForces
          Reactions            = reactions
          ElementResults       = elemResults
          ElementStressStrains = elemStressStrains }

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
