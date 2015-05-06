module ReshrperCfgToGeneric

open QuickGraph

open System.Collections.Generic

open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.Tree

open ResharperCfgAdditionalInfo
open GenericGraphs
open Utils
open IControlFlowGraphUtils

type ConvertInfo = {
    AstToCfgMapping: AstToCfgDict
    LoopNodes: Dictionary<IControlFlowElement, LoopNodeInfo>
    AstToGenericNodes: Dictionary<ITreeNode, HashSet<GraphNode>>
    CfeToGenericNodes: Dictionary<IControlFlowElement, GraphNode> }

let private collectConvertInfo (cfg: IControlFlowGraf) tryAsLoopTreeNode = 
    let astNodeToCfeDict = GeneralCfgInfoFuns.astNodeToCfeDict cfg 
    let loopsDict = LoopNodeInfoFuns.collect cfg tryAsLoopTreeNode astNodeToCfeDict
    {   AstToCfgMapping = astNodeToCfeDict; 
        LoopNodes = loopsDict; 
        AstToGenericNodes = Dictionary();
        CfeToGenericNodes = Dictionary() }

let rec toGenericCfg (cfg: IControlFlowGraf) toGenericNode tryAsLoopTreeNode tryAsLoopCfe functionName =
    let connectToTraversedSuccessors (curCfe: IControlFlowElement) curNode (graph: BidirectGraph) (info: ConvertInfo) =
        let tryGetGenericOfTarget (rib: IControlFlowRib) =
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
            (graph: BidirectGraph, parentsStack, lastId, info: ConvertInfo, alternativeExits) =
        // convert cfe to generic node
        let newId = lastId + 1
        let genericNode = toGenericNode cfe newId info
        // update info
        if cfe.SourceElement <> null
        then do DictionaryFuns.addToSetInDict cfe.SourceElement genericNode info.AstToGenericNodes
        do info.CfeToGenericNodes.[cfe] <- genericNode
        // add to graph
        if List.isEmpty parentsStack
        then do graph.AddVertex genericNode |> ignore
        else do graph.AddVerticesAndEdge (Edge(List.head parentsStack, genericNode)) |> ignore
        // check if there are already traversed successors
        // we must connect current node to
        do connectToTraversedSuccessors cfe genericNode graph info
        // if cur node is loop, we must continue processing from it's exits
        // so alternative exits are added and will be used in getNextNodes during dfs
        let alternativeExits =
            match tryAsLoopCfe cfe info with
            | Some(loopInfo) -> 
                loopInfo.BodyEnter :: loopInfo.LoopExit :: alternativeExits
            | _ -> alternativeExits
        graph, genericNode :: parentsStack, newId, info, alternativeExits

    let getNextNodes (e: IControlFlowElement) ((g, ps, li, i, alternativeExits) as state) =
        if List.isEmpty alternativeExits
        then getCfeExits e state
        else alternativeExits, (g, ps, li, i, [])

    let postProcess e (g, parentsStack, li, i, ae) =
        (g, List.tail parentsStack, li, i, ae)

    let surroundLoopsWithMarkers (graph: BidirectGraph) (info: ConvertInfo) lastId =
        let addOutMarkNode (edges: seq<Edge<GraphNode>>) markNode =
            do edges |> Seq.iter (graph.RemoveEdge >> ignore)
            let src = edges |> Seq.head |> (fun e -> e.Source)
            do Edge(src, markNode) |> graph.AddVerticesAndEdge |> ignore
            let markToTargets = edges |> Seq.map (fun e -> Edge(markNode, e.Target))
            do graph.AddVerticesAndEdgeRange markToTargets |> ignore
        let addInMarkNode (edges: seq<Edge<GraphNode>>) markNode =
            do edges |> Seq.iter (graph.RemoveEdge >> ignore)
            let dst = edges |> Seq.head |> (fun e -> e.Target)
            do Edge(markNode, dst) |> graph.AddVerticesAndEdge |> ignore
            let sourcesToMark = edges |> Seq.map (fun e -> Edge(e.Source, markNode))
            do graph.AddVerticesAndEdgeRange sourcesToMark |> ignore
        let getLoopGenericInEdges (loopNode: GraphNode) (bodyExits: list<IControlFlowRib>) =
            let nodes = 
                bodyExits 
                |> List.map (fun r -> info.CfeToGenericNodes.[r.Source])
                |> Set.ofList
            graph.InEdges(loopNode) 
            |> List.ofSeq
            |> List.partition (fun r -> Set.contains r.Source nodes)
        let getLoopGenericOutEdges (loopNode: GraphNode) bodyEnter =
            graph.OutEdges(loopNode)
            |> List.ofSeq
            |> List.partition (fun r -> info.CfeToGenericNodes.[bodyEnter] = r.Target)
        let surroundLoopWithMarkers (loopNode: GraphNode) (loopInfo: LoopNodeInfo) lastId =
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

    let traverseConverting (cfg: IControlFlowGraf) (info: ConvertInfo) =
        let initState = (BidirectGraphFuns.create (), [], 0, info, [])
        let _, (graph, _, lastId, _, _) =
            dfsCfgBasic cfg.EntryElement (fun _ s -> s) processNode getNextNodes postProcess initState
        do surroundLoopsWithMarkers graph info lastId
        { FunctionName = functionName; Graph = graph }

    let convInfo = collectConvertInfo cfg tryAsLoopTreeNode
    let genericCfg = traverseConverting cfg convInfo
    genericCfg, convInfo