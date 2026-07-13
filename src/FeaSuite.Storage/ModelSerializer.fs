namespace FeaSuite.Storage

open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open FeaSuite.Core

// ---------------------------------------------------------------------------
// ModelSerializer – save / load FEAModel to / from a JSON file.
//
// Uses System.Text.Json.Nodes (JsonObject / JsonArray) to build and parse the
// JSON document directly, without DTO types, to avoid F# record serialisation
// limitations with System.Text.Json.
// ---------------------------------------------------------------------------

module ModelSerializer =

    let private jsonOpts = JsonSerializerOptions(WriteIndented = true)

    // -----------------------------------------------------------------------
    // Helper: inline element-type string encoding
    // -----------------------------------------------------------------------

    let private elementTypeToString = function
        // Beam group
        | Beam Bar1D    -> "Bar1D"    | Beam Truss3D  -> "Truss3D"
        | Beam Beam2D   -> "Beam2D"   | Beam Beam3D   -> "Beam3D"
        // Shell group
        | Shell Shell4   -> "Shell4"
        | Shell Shell61  -> "Shell61"
        // Axisymmetric group
        | Axisymmetric Plane75  -> "Plane75" | Axisymmetric Plane78 -> "Plane78" | Axisymmetric Plane83 -> "Plane83"
        | Axisymmetric Shell208 -> "Shell208" | Axisymmetric Shell209 -> "Shell209"
        // Link group
        | Link Link11   -> "Link11"   | Link Link31  -> "Link31"   | Link Link33  -> "Link33"
        | Link Link34   -> "Link34"   | Link Link68  -> "Link68"   | Link Link180 -> "Link180"  | Link Link228  -> "Link228"
        // Pipe group
        | Pipe Pipe288  -> "Pipe288"  | Pipe Pipe289 -> "Pipe289"  | Pipe Elbow290 -> "Elbow290"
        // Special group
        | Special Solid8   -> "Solid8"
        | Special Cpt212   -> "Cpt212"   | Special Cpt213   -> "Cpt213"
        | Special Cpt215   -> "Cpt215"   | Special Cpt216   -> "Cpt216"   | Special Cpt217   -> "Cpt217"
        | Special Fluid29  -> "Fluid29"  | Special Fluid30  -> "Fluid30"  | Special Fluid38  -> "Fluid38"
        | Special Fluid116 -> "Fluid116" | Special Fluid129 -> "Fluid129" | Special Fluid130 -> "Fluid130"
        | Special Fluid136 -> "Fluid136" | Special Fluid138 -> "Fluid138" | Special Fluid139 -> "Fluid139"
        | Special Fluid218 -> "Fluid218" | Special Fluid220 -> "Fluid220" | Special Fluid221 -> "Fluid221"
        | Special Fluid243 -> "Fluid243" | Special Fluid244 -> "Fluid244"
        | Special Follw201 -> "Follw201"
        | Special Hsfld241 -> "Hsfld241" | Special Hsfld242 -> "Hsfld242"
        | Special Infin47  -> "Infin47"  | Special Infin110 -> "Infin110" | Special Infin111 -> "Infin111" | Special Infin257 -> "Infin257"
        | Special Inter192 -> "Inter192" | Special Inter193 -> "Inter193" | Special Inter194 -> "Inter194" | Special Inter195 -> "Inter195"
        | Special Inter202 -> "Inter202" | Special Inter203 -> "Inter203" | Special Inter204 -> "Inter204" | Special Inter205 -> "Inter205"
        | Special Mass21   -> "Mass21"   | Special Mass71   -> "Mass71"
        | Special Mpc184   -> "Mpc184"

    let private elementTypeOfString = function
        // Beam group
        | "Bar1D"    -> Beam Bar1D    | "Truss3D"  -> Beam Truss3D
        | "Beam2D"   -> Beam Beam2D   | "Beam3D"   -> Beam Beam3D
        // Shell group
        | "Shell4"   -> Shell Shell4
        | "Shell61"  -> Shell Shell61
        // Axisymmetric group
        | "Plane75"  -> Axisymmetric Plane75 | "Plane78" -> Axisymmetric Plane78 | "Plane83" -> Axisymmetric Plane83
        | "Shell208" -> Axisymmetric Shell208 | "Shell209" -> Axisymmetric Shell209
        // Link group
        | "Link11"   -> Link Link11   | "Link31"   -> Link Link31   | "Link33"  -> Link Link33
        | "Link34"   -> Link Link34   | "Link68"   -> Link Link68   | "Link180" -> Link Link180  | "Link228" -> Link Link228
        // Pipe group
        | "Pipe288"  -> Pipe Pipe288  | "Pipe289"  -> Pipe Pipe289  | "Elbow290" -> Pipe Elbow290
        // Special group
        | "Solid8"   -> Special Solid8
        | "Cpt212"   -> Special Cpt212   | "Cpt213"   -> Special Cpt213
        | "Cpt215"   -> Special Cpt215   | "Cpt216"   -> Special Cpt216   | "Cpt217"   -> Special Cpt217
        | "Fluid29"  -> Special Fluid29  | "Fluid30"  -> Special Fluid30  | "Fluid38"  -> Special Fluid38
        | "Fluid116" -> Special Fluid116 | "Fluid129" -> Special Fluid129 | "Fluid130" -> Special Fluid130
        | "Fluid136" -> Special Fluid136 | "Fluid138" -> Special Fluid138 | "Fluid139" -> Special Fluid139
        | "Fluid218" -> Special Fluid218 | "Fluid220" -> Special Fluid220 | "Fluid221" -> Special Fluid221
        | "Fluid243" -> Special Fluid243 | "Fluid244" -> Special Fluid244
        | "Follw201" -> Special Follw201
        | "Hsfld241" -> Special Hsfld241 | "Hsfld242" -> Special Hsfld242
        | "Infin47"  -> Special Infin47  | "Infin110" -> Special Infin110 | "Infin111" -> Special Infin111 | "Infin257" -> Special Infin257
        | "Inter192" -> Special Inter192 | "Inter193" -> Special Inter193 | "Inter194" -> Special Inter194 | "Inter195" -> Special Inter195
        | "Inter202" -> Special Inter202 | "Inter203" -> Special Inter203 | "Inter204" -> Special Inter204 | "Inter205" -> Special Inter205
        | "Mass21"   -> Special Mass21   | "Mass71"   -> Special Mass71
        | "Mpc184"   -> Special Mpc184
        | _          -> Beam Bar1D

    // -----------------------------------------------------------------------
    // Domain → JsonObject
    // -----------------------------------------------------------------------

    let private nodeToJson (n: Node) =
        let o = JsonObject()
        o["Id"]               <- JsonValue.Create(NodeId.value n.Id)
        o["X"]                <- JsonValue.Create(n.Position.X)
        o["Y"]                <- JsonValue.Create(n.Position.Y)
        o["Z"]                <- JsonValue.Create(n.Position.Z)
        o["DegreesOfFreedom"] <- JsonValue.Create(n.DegreesOfFreedom)
        o

    let private materialToJson (m: Material) =
        let o = JsonObject()
        o["Id"]               <- JsonValue.Create(MaterialId.value m.Id)
        o["Name"]             <- JsonValue.Create(m.Name)
        o["YoungModulus"]     <- JsonValue.Create(m.YoungModulus)
        o["PoissonRatio"]     <- JsonValue.Create(m.PoissonRatio)
        o["Density"]          <- JsonValue.Create(m.Density)
        o["HasCrossSection"]  <- JsonValue.Create(m.CrossSectionArea.IsSome)
        o["CrossSectionArea"] <- JsonValue.Create(m.CrossSectionArea |> Option.defaultValue 0.0)
        o

    let private elementPropsToJson (p: ElementProperties) =
        let o = JsonObject()
        match p with
        | NoProperties ->
            o["case"] <- JsonValue.Create("NoProperties")
        | BarSection s ->
            o["case"] <- JsonValue.Create("BarSection")
            o["Area"] <- JsonValue.Create(s.Area)
        | Beam2DSection s ->
            o["case"] <- JsonValue.Create("Beam2DSection")
            o["Area"] <- JsonValue.Create(s.Area)
            o["Iz"]   <- JsonValue.Create(s.Iz)
        | Beam3DSection s ->
            o["case"] <- JsonValue.Create("Beam3DSection")
            o["Area"] <- JsonValue.Create(s.Area)
            o["Iz"]   <- JsonValue.Create(s.Iz)
            o["Iy"]   <- JsonValue.Create(s.Iy)
            o["J"]    <- JsonValue.Create(s.J)
        | ShellSection s ->
            o["case"]      <- JsonValue.Create("ShellSection")
            o["Thickness"] <- JsonValue.Create(s.Thickness)
        o

    let private elementPropsOfJson (o: JsonObject) : ElementProperties =
        let f key = o.Item(key : string).GetValue<float>()
        match o.Item("case" : string).GetValue<string>() with
        | "BarSection"     -> BarSection    { Area = f "Area" }
        | "Beam2DSection"  -> Beam2DSection { Area = f "Area"; Iz = f "Iz" }
        | "Beam3DSection"  -> Beam3DSection { Area = f "Area"; Iz = f "Iz"
                                              Iy   = f "Iy";   J  = f "J" }
        | "ShellSection"   -> ShellSection  { Thickness = f "Thickness" }
        | _                -> NoProperties

    let private elementToJson (e: Element) =
        let o = JsonObject()
        o["Id"]         <- JsonValue.Create(ElementId.value e.Id)
        o["Type"]       <- JsonValue.Create(elementTypeToString e.Type)
        o["MaterialId"] <- JsonValue.Create(MaterialId.value e.MaterialId)
        let nids = JsonArray()
        for nid in e.NodeIds do nids.Add(JsonValue.Create(NodeId.value nid))
        o["NodeIds"]    <- nids
        o["Properties"] <- elementPropsToJson e.Properties
        o

    let private loadToJson (l: Load) =
        let o = JsonObject()
        o["NodeId"]        <- JsonValue.Create(NodeId.value l.NodeId)
        o["LocalDofIndex"] <- JsonValue.Create(l.LocalDofIndex)
        o["Value"]         <- JsonValue.Create(l.Value)
        o

    let private bcToJson (bc: BoundaryCondition) =
        let o = JsonObject()
        o["NodeId"]        <- JsonValue.Create(NodeId.value bc.NodeId)
        o["LocalDofIndex"] <- JsonValue.Create(bc.LocalDofIndex)
        let ctype, pval =
            match bc.Constraint with Fixed -> "Fixed", 0.0 | Prescribed v -> "Prescribed", v
        o["ConstraintType"]  <- JsonValue.Create(ctype)
        o["PrescribedValue"] <- JsonValue.Create(pval)
        o

    let private loadCaseToJson (lc: LoadCase) =
        let o = JsonObject()
        o["Id"]   <- JsonValue.Create(LoadCaseId.value lc.Id)
        o["Name"] <- JsonValue.Create(lc.Name)
        let loads = JsonArray()
        for l in lc.Loads do loads.Add(loadToJson l)
        o["Loads"] <- loads
        let bcs = JsonArray()
        for bc in lc.BoundaryConditions do bcs.Add(bcToJson bc)
        o["BoundaryConditions"] <- bcs
        o

    let private modelToJson (m: FEAModel) =
        let root = JsonObject()
        let nodes = JsonArray()
        for KeyValue(_, n) in m.Nodes do nodes.Add(nodeToJson n)
        root["Nodes"] <- nodes
        let mats = JsonArray()
        for KeyValue(_, mat) in m.Materials do mats.Add(materialToJson mat)
        root["Materials"] <- mats
        let elems = JsonArray()
        for KeyValue(_, e) in m.Elements do elems.Add(elementToJson e)
        root["Elements"] <- elems
        let lcs = JsonArray()
        for lc in m.LoadCases do lcs.Add(loadCaseToJson lc)
        root["LoadCases"] <- lcs
        root

    // -----------------------------------------------------------------------
    // JsonObject → Domain
    // -----------------------------------------------------------------------

    let private str   (o: JsonObject) (key: string) = o.[key].GetValue<string>()
    let private int_  (o: JsonObject) (key: string) = o.[key].GetValue<int>()
    let private flt   (o: JsonObject) (key: string) = o.[key].GetValue<float>()
    let private bool_ (o: JsonObject) (key: string) = o.[key].GetValue<bool>()

    let private nodeOfJson (o: JsonObject) : Node = {
        Id               = NodeId (int_ o "Id")
        Position         = { X = flt o "X"; Y = flt o "Y"; Z = flt o "Z" }
        DegreesOfFreedom = int_ o "DegreesOfFreedom"
    }

    let private materialOfJson (o: JsonObject) : Material = {
        Id               = MaterialId (int_ o "Id")
        Name             = str o "Name"
        YoungModulus     = flt o "YoungModulus"
        PoissonRatio     = flt o "PoissonRatio"
        Density          = flt o "Density"
        CrossSectionArea =
            if bool_ o "HasCrossSection" then Some (flt o "CrossSectionArea") else None
    }

    let private elementOfJson (o: JsonObject) : Element =
        let nodeIds =
            (o["NodeIds"] :?> JsonArray)
            |> Seq.map (fun n -> NodeId (n.GetValue<int>()))
            |> Seq.toList
        { Id         = ElementId (int_ o "Id")
          Type       = elementTypeOfString (str o "Type")
          NodeIds    = nodeIds
          MaterialId = MaterialId (int_ o "MaterialId")
          Properties = elementPropsOfJson (o["Properties"] :?> JsonObject) }

    let private loadOfJson (o: JsonObject) : Load = {
        NodeId        = NodeId (int_ o "NodeId")
        LocalDofIndex = int_ o "LocalDofIndex"
        Value         = flt o "Value"
    }

    let private bcOfJson (o: JsonObject) : BoundaryCondition = {
        NodeId        = NodeId (int_ o "NodeId")
        LocalDofIndex = int_ o "LocalDofIndex"
        Constraint    =
            if str o "ConstraintType" = "Prescribed" then Prescribed (flt o "PrescribedValue")
            else Fixed
    }

    let private loadCaseOfJson (o: JsonObject) : LoadCase = {
        Id   = LoadCaseId (int_ o "Id")
        Name = str o "Name"
        Loads =
            (o["Loads"] :?> JsonArray)
            |> Seq.map (fun n -> loadOfJson (n :?> JsonObject))
            |> Seq.toList
        BoundaryConditions =
            (o["BoundaryConditions"] :?> JsonArray)
            |> Seq.map (fun n -> bcOfJson (n :?> JsonObject))
            |> Seq.toList
    }

    let private modelOfJson (root: JsonObject) : FEAModel =
        let addNodes m =
            (root["Nodes"] :?> JsonArray)
            |> Seq.fold (fun acc n -> FEAModel.addNode (nodeOfJson (n :?> JsonObject)) acc) m
        let addMaterials m =
            (root["Materials"] :?> JsonArray)
            |> Seq.fold (fun acc n -> FEAModel.addMaterial (materialOfJson (n :?> JsonObject)) acc) m
        let addElements m =
            (root["Elements"] :?> JsonArray)
            |> Seq.fold (fun acc n -> FEAModel.addElement (elementOfJson (n :?> JsonObject)) acc) m
        let addLoadCases m =
            (root["LoadCases"] :?> JsonArray)
            |> Seq.fold (fun acc n -> FEAModel.addLoadCase (loadCaseOfJson (n :?> JsonObject)) acc) m
        FEAModel.empty
        |> addNodes
        |> addMaterials
        |> addElements
        |> addLoadCases

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /// Serialise an FEAModel to a JSON file at <paramref name="path"/>.
    let saveModel (path: string) (model: FEAModel) : Validation<unit> =
        try
            let json = (modelToJson model).ToJsonString(jsonOpts)
            File.WriteAllText(path, json)
            Ok ()
        with ex ->
            Error [ StorageError ex.Message ]

    /// Deserialise an FEAModel from a JSON file at <paramref name="path"/>.
    let loadModel (path: string) : Validation<FEAModel> =
        try
            if not (File.Exists path) then
                Error [ StorageError (sprintf "Model file not found: %s" path) ]
            else
                let json = File.ReadAllText path
                let root = JsonNode.Parse(json) :?> JsonObject
                Ok (modelOfJson root)
        with ex ->
            Error [ StorageError ex.Message ]
