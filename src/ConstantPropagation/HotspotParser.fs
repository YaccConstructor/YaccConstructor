module HotspotParser
/// For parsing Hotspots.xml file and producing it's object
/// representation

open System
open System.IO
open System.Xml


type Hotspot = 
    val Class : string
    val Method : string
    val QueryPosition : int 
    val ReturnType : string

    new (_class, _method, position, returnType) = {
        Class = _class
        Method = _method
        QueryPosition = position
        ReturnType = returnType
    }
    new (full : string array, position, returnType) = 
        new Hotspot(full.[0], full.[1], position, returnType)

let parseHotspots (fileName : string) = 
    let parseHotspot (hotspot : XmlNode) = 
        let mutable child = hotspot
        if child.NodeType = XmlNodeType.Comment 
        then child <- child.NextSibling

        let language = 
            match child.Name.ToLowerInvariant() with
            | "language" -> child.InnerText.Trim().ToLowerInvariant()
            | x -> failwithf "Unexpected tag %A. Expected <Language>" x

        child <- child.NextSibling
        if child.NodeType = XmlNodeType.Comment 
        then child <- child.NextSibling

        let methodName = 
            match child.Name.ToLowerInvariant() with
            | "method" -> child.InnerText.Trim().ToLowerInvariant().Split('.')
            | x -> failwithf "Unexpected tag %A. Expected <Method>" x

        child <- child.NextSibling
        if child.NodeType = XmlNodeType.Comment 
        then child <- child.NextSibling

        let pos = 
            match child.Name.ToLowerInvariant() with
            | "argumentposition" -> Int32.Parse <| child.InnerText.Trim()
            | x -> failwithf "Unexpected tag %A. Expected <ArgumentListType>" x
        
        child <- child.NextSibling
        if child.NodeType = XmlNodeType.Comment 
        then child <- child.NextSibling

        let returnType = 
            match child.Name.ToLowerInvariant() with
            | "returntype" -> child.InnerText.Trim().ToLowerInvariant()
            | x -> failwithf "Unexpected tag %s. Expected <ReturnType>" x

        language, new Hotspot(methodName, pos, returnType)
    
    if not <| File.Exists fileName
    then 
        failwithf "File %s isn't found" fileName
    else 
        let xmlDocument = new XmlDocument()
        xmlDocument.Load(fileName)

        let mutable element = xmlDocument.DocumentElement.ChildNodes
        let mutable result = []
    
        for hotNode in element do
            if hotNode.NodeType <> XmlNodeType.Comment 
            then
                match hotNode.Name.ToLowerInvariant() with
                | "hotspot" -> 
                    result <- parseHotspot hotNode.FirstChild :: result
                | x -> failwithf "Unexpected tag %A. Expected <Hotspot>" x
        result