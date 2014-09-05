module XMLParser

open System.Xml

type Hotspot = 
    val Class : string
    val Method : string
    val QueryPosition : int 
    val ReturnType : string

    new (_class, _method, position, returnType) = {Class = _class; Method = _method; QueryPosition = position; ReturnType = returnType}
    new (full : string array, position, returnType) = new Hotspot(full.[0], full.[1], position, returnType)

let parseXml (path : string) = 
    let parseHotspot (hotspot : XmlNode) = 
        let mutable child = hotspot
        if child.NodeType = XmlNodeType.Comment 
        then child <- child.NextSibling

        let methodName = 
            match child.Name.ToLowerInvariant() with
            | "fullname" -> child.InnerText.Trim().ToLowerInvariant().Split('.')
            | x -> failwithf "Unexpected tag %A. Expected <Fullname>" x

        child <- child.NextSibling
        if child.NodeType = XmlNodeType.Comment 
        then child <- child.NextSibling

        let pos = 
            match child.Name.ToLowerInvariant() with
            | "argumentposition" -> System.Int32.Parse <| child.InnerText.Trim()
            | x -> failwithf "Unexpected tag %A. Expected <ArgumentListType>" x
        
        child <- child.NextSibling
        if child.NodeType = XmlNodeType.Comment 
        then child <- child.NextSibling

        let returnType = 
            match child.Name.ToLowerInvariant() with
            | "returntype" -> child.InnerText.Trim().ToLowerInvariant()
            | x -> failwithf "Unexpected tag %A. Expected <ReturnType>" x

        new Hotspot(methodName, pos, returnType)
    
    let xmlDocument = new XmlDocument()
    xmlDocument.Load (path)

    let mutable element = xmlDocument.DocumentElement.ChildNodes
    let mutable result = []
    
    for langNode in element do
        if langNode.NodeType <> XmlNodeType.Comment 
        then
            match langNode.Name.ToLowerInvariant() with
            | "language" -> 
                let lang = langNode.Attributes.GetNamedItem("name").Value.ToLowerInvariant()
                for item in langNode.ChildNodes do
                    if item.NodeType <> XmlNodeType.Comment
                    then
                        let hotspot = parseHotspot item.FirstChild
                        result <- (lang, hotspot) :: result
            | x -> failwithf "Unexpected tag %A. Expected <Language>" x

    result