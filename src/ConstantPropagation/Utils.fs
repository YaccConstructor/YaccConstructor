module Utils

module DotUtils =
    open JetBrains.ReSharper.Psi.ControlFlow.CSharp
    open JetBrains.ReSharper.Psi.ControlFlow
    open JetBrains.ReSharper.Psi.CSharp.Tree
    open JetBrains.ReSharper.Psi.Tree
    open JetBrains.ReSharper.Psi

    open System.IO
    open System.Collections.Generic

    let private toDot (cfg: ICSharpControlFlowGraf) (outStream: StreamWriter) =
        let getNodeInfo (node: IControlFlowElement) =
            if node <> null
            then 
                let psiType = 
                    if node.SourceElement <> null 
                    then node.SourceElement.NodeType.ToString()
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
    let cfgToDot (cfg: ICSharpControlFlowGraf) outPath name =
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

module MapUtils =
    let union (map1: Map<_,_>) (map2: Map<_,_>) =
        Map.fold (fun acc key value -> Map.add key value acc) map2 map1

module StateMonad = 
    type State<'s,'a> = State of ('s -> 'a * 's)

    type StateBuilder<'s>() =
      member x.Return v : State<'s,_> = State(fun s -> v,s)
      member x.Bind(State v, f) : State<'s,_> =
        State(fun s ->
          let (a, s) = v s
          let (State v') = f a
          v' s)

    let withState<'s> = StateBuilder<'s>()

    let getState = State(fun s -> s, s)
    let putState v = State(fun _ -> (), v)

    let runState (State f) init = f init

let myDebugFolderPath = "E:\\Diploma\\Debug"

let applyToMappedTypedArgs f mapper (arg1: 'a) (arg2: obj) typingFaildFunc =
    match arg2 with
    | :? 'a as arg2Typed-> f (mapper arg1) (mapper arg2Typed)
    | _ -> typingFaildFunc ()