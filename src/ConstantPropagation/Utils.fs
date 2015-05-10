module Utils

open System.IO

module DotUtils =
    open JetBrains.ReSharper.Psi.ControlFlow.CSharp
    open JetBrains.ReSharper.Psi.ControlFlow
    open JetBrains.ReSharper.Psi.CSharp.Tree
    open JetBrains.ReSharper.Psi.Tree
    open JetBrains.ReSharper.Psi

    open System.Collections.Generic

    let private toDot (cfg: IControlFlowGraf) (outStream: StreamWriter) =
        let getNodeInfo (node: IControlFlowElement) =
            if node <> null
            then 
                let psiType = 
                    if node.SourceElement <> null 
                    then
                        let extraInfo = 
                            match node.SourceElement with
                            | :? IForStatement as forStmt ->
                                let condHash = string <| hash forStmt.Condition
                                sprintf "_%s" condHash
                            | _ -> ""
                        let nodeType = string node.SourceElement.NodeType
                        let srcHash = string <| hash node.SourceElement
                        sprintf "%s_%s%s" nodeType srcHash extraInfo
                    else "null"
                node.Id.ToString(), psiType
            else
                "nullnode", "nullnode"

        let printLabel (nodeNum, text) =
            outStream.WriteLine(nodeNum + " [label=\"" + nodeNum + "(" + text + ")" + "\"]")

        let printGraphNode (node: IControlFlowElement) =
            let src = getNodeInfo(node)
            printLabel src
            node.Exits
            |> List.ofSeq
            |> List.map (fun e -> e.Target)
            |> List.map 
                (
                    fun t ->
                        let target = getNodeInfo t
                        printLabel target
                        outStream.WriteLine((fst src) + " -> " + (fst target))
                )
            |> List.iter (fun edge -> outStream.WriteLine(edge))

        let rec bfs (elems: list<IControlFlowElement>) (visited: HashSet<IControlFlowElement>)=
            match elems with
            | null :: tl ->
                outStream.WriteLine ("null_node")
                bfs tl visited
            | hd :: tl when visited.Contains hd ->
                bfs tl visited
            | hd :: tl -> 
                printGraphNode hd
                let updatedElems =
                    tl @ (
                        hd.Exits 
                        |> List.ofSeq 
                        |> List.map (fun rib -> if rib <> null then rib.Target else null)
                    )
                do visited.Add hd |> ignore
                bfs updatedElems visited
            | [] -> ()
        bfs [cfg.EntryElement] (new HashSet<IControlFlowElement>())

    /// Converts passed cfg "cfg" to DOT's digraph with name "name" 
    /// and stores it in the file specified by "outPath"
    let cfgToDot (cfg: IControlFlowGraf) outPath name =
        use outStream = FileInfo(outPath).CreateText()
        outStream.WriteLine("digraph " + name + " {")
        toDot cfg outStream
        outStream.WriteLine("}")

    /// Extracts C# CFG from method declaration and converts it
    /// to DOT's digraph. The output file's name and the digraph's name
    /// are the same as passed method's name
    let methodCFGToDot (methodDecl: IMethodDeclaration) (outDirPath: string) =
        let methodName = methodDecl.NameIdentifier.GetText()
        let outPath = Path.Combine(outDirPath, methodName + ".dot")
        let cfg = CSharpControlFlowBuilder.Build methodDecl
        cfgToDot cfg outPath methodName

    /// Applies "CFGUtils.methodCFGToDot" to all the methods in the file
    let allMethodsCFGToDot (file: ICSharpFile) (outDirPath: string)=
        let processorAction (node: ITreeNode) = 
            match node with
            | :? IMethodDeclaration as methodDecl -> methodCFGToDot methodDecl outDirPath
            | _ -> ()
        let processor = RecursiveElementProcessor (fun node -> processorAction node)
        processor.Process file

let myDebugFolderPath = "E:\\Diploma\\Debug"
let myDebugFilePath fileName = Path.Combine (myDebugFolderPath, fileName)

let applyToMappedTypedArgs f mapper (arg1: 'a) (arg2: obj) typingFailedFunc =
    match arg2 with
    | :? 'a as arg2Typed-> f (mapper arg1) (mapper arg2Typed)
    | _ -> typingFailedFunc ()

let (===) = LanguagePrimitives.PhysicalEquality

module Option =
    let getOrElse opt defaultVal =
        match opt with
        | Some(value) -> value
        | None -> defaultVal

module DictionaryFuns =
    open System.Collections.Generic

    let dictFromSeq (vals: seq<'a * 'b>) =
        let dict = Dictionary()
        vals |> Seq.iter (fun (a, b) -> dict.[a] <- b)
        dict
    // todo: reassign may be redundant in case of set already exists
    let addToSetInDict key elemForSet (dict: Dictionary<_, HashSet<_>>) =
        match dict.TryGetValue key with 
        | true, hs -> do hs.Add elemForSet |> ignore
        | false, _ -> 
            let hs = HashSet()
            do hs.Add elemForSet |> ignore
            do dict.[key] <- hs
    let mergeDicts (dict1: Dictionary<'a, 'b>) (dict2: Dictionary<'a, 'c>) =
        let resDict = Dictionary()
        do dict1
        |> Seq.iter 
            (
                fun (KeyValue(a, b)) -> 
                    match dict2.TryGetValue a with
                    | true, c -> resDict.[b] <- c
                    | false, _ -> failwith "dict1 contains elements that dict2 doesn't"
            )
        resDict
    let getMappingToOne key (dict: Dictionary<'k, HashSet<'v>>) =
        let hSet = dict.[key]
        if hSet.Count <> 1
        then failwith "one to one mapping expected"
        else hSet |> Seq.head

let flip f = fun x y -> f y x

module List =
    let cons x xs = x :: xs
        
module Seq =
    let unfoldS generator state =
        Seq.unfold (generator >> Option.map (fun (u, s) -> (u, s), s)) state