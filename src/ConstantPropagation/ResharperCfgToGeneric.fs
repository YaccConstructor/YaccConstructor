/// Functions for converting JetBrains.ReSharper.Psi.ControlFlow.IControlFlowGraph
/// to generic CFG
module ResharperCfgToGeneric

open QuickGraph

open System.Collections.Generic

open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.Tree

open ResharperCfgAdditionalInfo
open GenericGraphs
open GenericCFG
open Utils
open GraphUtils
open IControlFlowGraphUtils

/// Additional info about converted IControlFlowGraph
type ConvertInfo<'Lit, 'OpInfo> = {
    AstToCfgMapping: AstToCfgDict
    LoopNodes: Dictionary<IControlFlowElement, LoopNodeInfo>
    AstToGenericNodes: Dictionary<ITreeNode, HashSet<GraphNode<'Lit, 'OpInfo>>>
    CfeToGenericNodes: Dictionary<IControlFlowElement, GraphNode<'Lit, 'OpInfo>> }

/// Generic function for converting IControlFlowGraph to generic CFG. Expects a function
/// that can convert a single node of IControlFlowGraph to generic node and a function
/// that generates basic loops info.
let rec toGenericCfg (cfg: IControlFlowGraph) toGenericNode findLoopToCondExits tryAsLoopCfe functionName =
    let connectToTraversedSuccessors 
            (curCfe: IControlFlowElement) 
            curNode 
            (graph: BidirectGraph<_,_>) 
            (info: ConvertInfo<_,_>) =
        let tryGetGenericOfTarget (rib: IControlFlowEdge) =
            if rib.Target <> null 
            then 
                match info.CfeToGenericNodes.TryGetValue rib.Target with
                | true, s -> Some(s)
                | false, _ -> None
            else None
        curCfe.Exits 
        |> Seq.choose tryGetGenericOfTarget
        |> Seq.iter (fun succ -> graph.AddEdge (Edge(curNode, succ)) |> ignore)

    let processNode 
            (cfe: IControlFlowElement) 
            (graph: BidirectGraph<_,_>, parentsStack, lastId, info: ConvertInfo<_,_>, alternativeExits) =
        // convert cfe to generic node
        let newId = lastId + 1
        let genericNode = toGenericNode cfe newId info
        // update info
        if cfe.SourceElement <> null
        then do Dictionary.addToSetInDict cfe.SourceElement genericNode info.AstToGenericNodes
        do info.CfeToGenericNodes.[cfe] <- genericNode
        // add to graph
        if List.isEmpty parentsStack
        then do graph.AddVertex genericNode |> ignore
        else do graph.AddVerticesAndEdge (Edge(List.head parentsStack, genericNode)) |> ignore
        // if cur node is loop, we must continue processing from it's exits
        // so alternative exits are added and will be used in getNextNodes during dfs
        let alternativeExits =
            match tryAsLoopCfe cfe info with
            | Some(loopInfo) -> 
                loopInfo.BodyEnter :: loopInfo.LoopExit :: alternativeExits
            | _ -> alternativeExits
        graph, genericNode :: parentsStack, newId, info, alternativeExits

    let getNextNodes cfe ((g, ps, li, i, alternativeExits) as state) =
        if List.isEmpty alternativeExits
        then getCfeExits cfe state
        else alternativeExits, (g, ps, li, i, [])

    let postProcess cfe (graph, parentsStack, li, info, ae) =
        // check if there are already traversed successors
        // we must connect current node to
        let genericNode = info.CfeToGenericNodes.[cfe]
        do connectToTraversedSuccessors cfe genericNode graph info
        // pop node from parentsStack
        (graph, List.tail parentsStack, li, info, ae)

    let surroundLoopsWithMarkers (graph: BidirectGraph<_,_>) (info: ConvertInfo<_,_>) lastId =
        let addOutMarkNode (edges: seq<Edge<GraphNode<_,_>>>) markNode =
            do edges |> Seq.iter (graph.RemoveEdge >> ignore)
            let src = edges |> Seq.head |> (fun e -> e.Source)
            do Edge(src, markNode) |> graph.AddVerticesAndEdge |> ignore
            let markToTargets = edges |> Seq.map (fun e -> Edge(markNode, e.Target))
            do graph.AddVerticesAndEdgeRange markToTargets |> ignore
        let addInMarkNode (edges: seq<Edge<GraphNode<_,_>>>) markNode =
            do edges |> Seq.iter (graph.RemoveEdge >> ignore)
            let dst = edges |> Seq.head |> (fun e -> e.Target)
            do Edge(markNode, dst) |> graph.AddVerticesAndEdge |> ignore
            let sourcesToMark = edges |> Seq.map (fun e -> Edge(e.Source, markNode))
            do graph.AddVerticesAndEdgeRange sourcesToMark |> ignore
        let getLoopGenericInEdges (loopNode: GraphNode<_,_>) (bodyExits: list<IControlFlowEdge>) =
            let nodes = 
                bodyExits 
                |> List.map (fun r -> info.CfeToGenericNodes.[r.Source])
                |> Set.ofList
            graph.InEdges(loopNode) 
            |> List.ofSeq
            |> List.partition (fun r -> Set.contains r.Source nodes)
        let getLoopGenericOutEdges (loopNode: GraphNode<_,_>) bodyEnter =
            graph.OutEdges(loopNode)
            |> List.ofSeq
            |> List.partition (fun r -> info.CfeToGenericNodes.[bodyEnter] = r.Target)
        let surroundLoopWithMarkers (loopNode: GraphNode<_,_>) (loopInfo: LoopNodeInfo) lastId =
            let genericBodyExits, genericLoopEntries = getLoopGenericInEdges loopNode loopInfo.BodyExits
            let newId = lastId + 1
            let bodyEnd = { Id = newId; Type = LoopBodyEnd }
            do addInMarkNode genericBodyExits bodyEnd
            let newId = newId + 1
            let loopEnter = { Id = newId; Type = LoopEnter }
            do addInMarkNode genericLoopEntries loopEnter
            let genericBodyEnter, genericLoopExits = getLoopGenericOutEdges loopNode loopInfo.BodyEnter
            let newId = newId + 1
            let bodyBeg = { Id = newId; Type = LoopBodyBeg }
            do addOutMarkNode genericBodyEnter bodyBeg
            let newId = newId + 1
            let loopExit = { Id = newId; Type = LoopExit }
            do addOutMarkNode genericLoopExits loopExit
            newId

        (lastId, info.LoopNodes)
        ||> Seq.fold 
            (fun prevId (KeyValue(loopCfe, loopInfo)) -> 
                    let loopNode = info.CfeToGenericNodes.[loopCfe]
                    surroundLoopWithMarkers loopNode loopInfo prevId)
        |> ignore

    let traverseConverting (cfg: IControlFlowGraph) (info: ConvertInfo<_,_>) =
        let initState = (BidirectGraphFuns.create (), [], 0, info, [])
        let _, (graph, _, lastId, _, _) =
            let algoParts = basicCfgDfsParts (fun _ s -> s) processNode getNextNodes postProcess
            dfs algoParts cfg.EntryElement Set.empty initState
        do surroundLoopsWithMarkers graph info lastId
        { FunctionName = functionName; Graph = graph }

    let astNodeToCfeDict = GeneralCfgInfoFuns.astNodeToCfeDict cfg
    let loopNodesToExits = findLoopToCondExits cfg astNodeToCfeDict
    let loopNodesInfo = LoopNodeInfoFuns.collect cfg loopNodesToExits
    let convInfo = {   
        AstToCfgMapping = astNodeToCfeDict; 
        LoopNodes = loopNodesInfo; 
        AstToGenericNodes = Dictionary();
        CfeToGenericNodes = Dictionary() }
    let genericCfg = traverseConverting cfg convInfo
    genericCfg, convInfo