/// Utilities for JetBrains.ReSharper.Psi.ControlFlow.IControlFlowGraph
module IControlFlowGraphUtils

open JetBrains.ReSharper.Psi.ControlFlow

open System.IO
open System.Collections.Generic

open GraphUtils

let private toDot (cfg: IControlFlowGraph) (outStream: StreamWriter) =
    let getNodeInfo (node: IControlFlowElement) =
        if node <> null
        then 
            let psiType = 
                if node.SourceElement <> null 
                then
                    let nodeType = string node.SourceElement.NodeType
                    let srcHash = string <| hash node.SourceElement
                    sprintf "%s_%s" nodeType srcHash
                else "null"
            node.Id.ToString(), psiType
        else "nullnode", "nullnode"
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
let cfgToDot (cfg: IControlFlowGraph) outPath name =
    use outStream = FileInfo(outPath).CreateText()
    outStream.WriteLine("digraph " + name + " {")
    toDot cfg outStream
    outStream.WriteLine("}")

/// IsVisited part of the DFS algo specific for IControlFlowGraph
let cfgIsVisited (e: IControlFlowElement) v = Set.contains e.Id v

/// MakeVisited part of the DFS algo specific for IControlFlowGraph
let cfgMakeVisited (e: IControlFlowElement) v = Set.add e.Id v

/// Returns DfsParts instance with IsVisited and MakeVisited set to 
/// "cfgIsVisited" and "cfgMakeVisited" respectively
let basicCfgDfsParts preProcess processNode getNextNodes postProcess = {
    IsVisited = cfgIsVisited
    MakeVisited = cfgMakeVisited
    PreProcess = preProcess
    ProcessNode = processNode
    GetNextNodes = getNextNodes
    PostProcess = postProcess }

/// PreProcess, ProcessNode and PostProcess DFS part implementation
/// that does nothing with passed node
let notProcessNode e s = s

/// Returns DfsParts instance with IsVisited and MakeVisited set to 
/// "cfgIsVisited" and "cfgMakeVisited" respectively, PreProcess and PostProcess
/// are implemented to do nothing
let cfgDfsParts processNode getNextNodes = 
    basicCfgDfsParts 
        notProcessNode
        processNode
        getNextNodes
        notProcessNode

/// Returns passed node's successors
let getCfeExits (e: IControlFlowElement) s =
    let exits =
        e.Exits
        |> Seq.choose (fun rib -> if rib.Target <> null then Some(rib.Target) else None)
        |> List.ofSeq
    exits, s

/// Returns DfsParts instance that is implemented to make DFS traversing
/// graph from predecessors to successors and doing nothing in preprocessing and
/// postprocessing stages
let cfgExitsDfsParts processNode =
    cfgDfsParts processNode getCfeExits

/// Returns passed node's predecessors
let getCfeEntries (e: IControlFlowElement) s =
    let entries =
        e.Entries
        |> Seq.choose (fun rib -> if rib.Source <> null then Some(rib.Source) else None)
        |> List.ofSeq
    entries, s

/// Returns DfsParts instance that is implemented to make DFS traversing
/// graph from successors to predecessors and doing nothing in preprocessing and
/// postprocessing stages
let cfgEntriesDfsParts processNode =
    cfgDfsParts processNode getCfeEntries

/// Cycles searching algorithm implementation for IControlFlowGraph.
/// Searches for the loop enter nodes. These nodes
/// have successors looped to the nodes under discussion.
/// The set of loop enter node ID's is returned.
let findLoopNodes (cfg: IControlFlowGraph): Set<int> =
    let calculateEnterExit (cfg: IControlFlowGraph) =
        let initState = (0, Map.empty, Map.empty)
        let preProcess (e: IControlFlowElement) (step, enter, x) = 
            (step + 1, Map.add e.Id (step + 1) enter, x)
        let processNode e s = s
        let getNextNodes (e: IControlFlowElement) s =
            let exits =
                e.Exits
                |> Seq.choose (fun rib -> if rib.Target <> null then Some(rib.Target) else None)
                |> List.ofSeq
            exits, s
        let postProcess (e: IControlFlowElement) (step, x, exit) =
            (step + 1, x, Map.add e.Id (step + 1) exit)
        let algoParts = 
            basicCfgDfsParts 
                preProcess
                processNode
                getNextNodes
                postProcess
        let visited, (step, enter, exit) = dfs algoParts cfg.EntryElement Set.empty initState
        enter, exit

    let findCycleFirstNodes enter exit =
        cfg.AllElements
        |> Seq.filter (fun e -> Map.containsKey e.Id enter)
        |> Seq.map
            (
                fun s -> 
                    if s <> null
                    then 
                        let srcEnter = Map.find s.Id enter
                        let srcExit = Map.find s.Id exit
                        s.Exits
                        |> Seq.choose 
                            (fun rib -> if rib.Target <> null then Some(rib.Target) else None)
                        |> Seq.choose 
                            (
                                fun t ->
                                    let tarEnter = Map.find t.Id enter
                                    let tarExit = Map.find t.Id exit
                                    if tarEnter < srcEnter && srcExit < tarExit
                                    then Some(t.Id)
                                    else None
                            )
                    else Seq.empty
            )
        |> Seq.concat

    let enter, exit = calculateEnterExit cfg
    findCycleFirstNodes enter exit |> Set.ofSeq