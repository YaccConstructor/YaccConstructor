module XMLParser

open System.Xml

type Hotspot = 
    val Class : string
    val Method : string
    val ArgumentsType : string list 
    val ReturnType : string

    new (_class, _method, argsTypes, returnType) = {Class = _class; Method = _method; ArgumentsType = argsTypes; ReturnType = returnType}
    new (full : string array, argsTypes, returnType) = new Hotspot(full.[0], full.[1], argsTypes, returnType)

let parseXml (path : string) = 
    let parseHotspot (hotspot : XmlNode) = 
        let mutable child = hotspot
        let methodName = 
            match child.Name.ToLowerInvariant() with
            | "fullname" -> child.InnerText.Trim().ToLowerInvariant().Split('.')
            | x -> failwithf "Unexpected tag %A. Expected <Fullname>" x

        child <- child.NextSibling
        let args = 
            match child.Name.ToLowerInvariant() with
            | "argumentstypelist" -> 
                let types = child.ChildNodes
                let mutable arguments = []
                for item in types do
                    match item.Name.ToLowerInvariant() with
                    | "argumenttype" -> 
                        arguments <- item.InnerText.Trim().ToLowerInvariant() :: arguments
                    | x -> failwithf "Unexpected tag %A. Expected <ArgumentType>" x
                List.rev arguments
            | x -> failwithf "Unexpected tag %A. Expected <ArgumentListType>" x
        
        child <- child.NextSibling
        let returnType = 
            match child.Name.ToLowerInvariant() with
            | "returntype" -> child.InnerText.Trim().ToLowerInvariant()
            | x -> failwithf "Unexpected tag %A. Expected <ReturnType>" x

        new Hotspot(methodName, args, returnType)
    
    let xmlDocument = new XmlDocument()
    xmlDocument.Load (path)

    let mutable element = xmlDocument.DocumentElement.ChildNodes
    let mutable result = []
    
    for langNode in element do
        match langNode.Name.ToLowerInvariant() with
        | "language" -> 
            let lang = langNode.Attributes.GetNamedItem("name").Value.ToLowerInvariant()
            for item in langNode.ChildNodes do
                let hotspot = parseHotspot item.FirstChild
                result <- (lang, hotspot) :: result
        | x -> failwithf "Unexpected tag %A. Expected <Language>" x

    result