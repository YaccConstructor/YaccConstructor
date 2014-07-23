module XMLParser

open System.Xml

type Hotspot = 
    val Namespace : string
    val Class : string
    val Method : string
    val ArgumentsType : string list 
    val ReturnType : string

    new (n, c, m, a, r) = {Namespace = n; Class = c; Method = m; ArgumentsType = a; ReturnType = r}
    new (full : string array, a, r) = new Hotspot(full.[0], full.[1], full.[2], a, r)

let parseHotspots (path : string) = 
    let xmlParser = new XmlTextReader(path)
    
    let inline readAllSpaces() = while xmlParser.NodeType = XmlNodeType.Whitespace do xmlParser.Read() |> ignore
    
    let parseHotspot() = 
        let mutable hotName = null
        let mutable args = []
        let mutable returnType = null

        xmlParser.Read() |> ignore
        readAllSpaces()
        match xmlParser.Name.ToLowerInvariant() with
        | "fullname" -> 
            xmlParser.Read() |> ignore
            hotName <- xmlParser.ReadContentAsString().ToLowerInvariant().Split('.')
            xmlParser.Read() |> ignore
            readAllSpaces()
        | _ -> failwithf "Unexpected %s. Expected <FullName>" xmlParser.Name

        match xmlParser.Name.ToLowerInvariant() with
        | "argumentstypelist" -> 
            readAllSpaces()
            xmlParser.Read() |> ignore
            readAllSpaces()
            
            while xmlParser.NodeType = XmlNodeType.Element do
                match xmlParser.Name.ToLower() with
                | "argumenttype" -> 
                    xmlParser.Read() |> ignore
                    readAllSpaces()
                    args <- xmlParser.ReadContentAsString().ToLowerInvariant().Trim() :: args
                    xmlParser.Read() |> ignore
                    readAllSpaces()
                | _ -> failwithf "Unexpected %s. Expected <ArgumentType>" xmlParser.Name
            xmlParser.Read() |> ignore
            readAllSpaces()
        | _ -> failwithf "Unexpected %s. Expected <ArgumentTypeList>" xmlParser.Name


        match xmlParser.Name.ToLowerInvariant() with
        | "returntype" -> 
            xmlParser.Read() |> ignore
            readAllSpaces()
            returnType <- xmlParser.ReadContentAsString().ToLowerInvariant().Trim()
            xmlParser.Read() |> ignore
            readAllSpaces()
        | _ -> failwithf "Unexpected %s. Expected <ReturnType>" xmlParser.Name

        xmlParser.Read() |> ignore
        readAllSpaces()
        
        args <- args |> List.rev
        
        new Hotspot (hotName, args, returnType)

    let mutable resultDict = []
    xmlParser.MoveToContent() |> ignore
    xmlParser.Read() |> ignore
    readAllSpaces()
    while xmlParser.NodeType <> XmlNodeType.EndElement do 
        match xmlParser.Name.ToLower() with
        | "language" -> 
            let lang = xmlParser.GetAttribute("name").ToLowerInvariant().Trim()
            xmlParser.Read() |> ignore
            readAllSpaces()

            while xmlParser.NodeType = XmlNodeType.Element do
                match xmlParser.Name.ToLowerInvariant() with
                | "hotspot" -> 
                    let hotspot = parseHotspot()
                    resultDict <- (lang, hotspot) :: resultDict
        
                | _ -> failwithf "unexpected %s. Expected <Hotspot>" <| xmlParser.Name
        | x -> failwith "Unexpected %s. Expected <Language>" x
        xmlParser.Read() |> ignore
        readAllSpaces()
    resultDict
    
//let res = parseHotspots "Hotspots.xml"
//res |> ignore
//()