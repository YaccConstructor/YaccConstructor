/// Represents generic control flow graph (CFG). Generic CFG is built from language specific
/// CFG and represents function or method in source code. Source code instructions are
/// represented by nodes, edges show the order of these instructions
module GenericCFG

open GenericGraphs
open DDG

/// Generic CFG. Contains graph itself and 
/// the name of a function it represents
type GenericCFG<'Lit, 'OpInfo> = {
    FunctionName: string
    Graph: BidirectGraph<'Lit, 'OpInfo> }

module GenericCFGFuncs =
    open System.Collections.Generic
    open System.IO
    open GraphNodeFuncs
    open GraphUtils.TopoTraverser
    open BidirectTopoUpTraverser
    open QuickGraph
    open GenericGraphs.BidirectGraphFuns
    open GraphUtils

    /// Creates empty generic CFG
    let create name = {
        FunctionName = name
        Graph = BidirectGraphFuns.create () }

    type private ConvertState<'Lit, 'OpInfo> = {
        Vars: Set<string>
        VisitedForks: Map<int, Set<string>>
        NodesToVisit: Set<int>
        VisitedNodes: list<GraphNode<'Lit, 'OpInfo>> }

    let private cfgToDdg (exitNode: GraphNode<_,_>) (cfg: GenericCFG<_,_>) = 
        let wrongCfgNodeTypeMsg = 
            "Cfg node was expected to be of ExitNode type case"
        let getExitVars = function
        | ExitNode(vars) -> vars
        | _ -> failwith wrongCfgNodeTypeMsg 
        let removeUnreachable (cfg: BidirectGraph<_,_>) exitNode =
            let isVisited (n: GraphNode<_,_>) v = Set.contains n.Id v
            let makeVisited (n: GraphNode<_,_>) v = Set.add n.Id v
            let doNothing n s = s
            let getPrevNodes n s = preds n cfg |> List.ofSeq, s
            let dfsParts = {
                IsVisited = isVisited
                MakeVisited = makeVisited
                PreProcess = doNothing
                ProcessNode = doNothing
                GetNextNodes = getPrevNodes
                PostProcess = doNothing }
            let visited, _ = dfs dfsParts exitNode Set.empty ()
            cfg.Vertices
            |> List.ofSeq
            |> List.iter (fun n -> if not <| Set.contains n.Id visited then removeVertex cfg n)
            cfg
        let rec findDdgNodes (cfg: BidirectGraph<_,_>) traverser (state: ConvertState<_,_>) =
            let makeVisited (node: GraphNode<_,_>) (state: ConvertState<_,_>) =
                { state with VisitedNodes = node :: state.VisitedNodes }
            let node, traverser = nextNode cfg traverser
            let traverser, state =
                match node.Type with
                | ExitNode(nodes) ->
                    let state = makeVisited node state
                    let nodesToVisit = 
                        (state.NodesToVisit, nodes)
                        ||> List.fold (fun acc n -> Set.add n.Id acc)
                    traverser, { state with NodesToVisit = nodesToVisit }
                | Declaration(name, initializerId) when Set.contains name state.Vars ->
                    let state = makeVisited node state
                    let nodesToVisit = Set.add initializerId state.NodesToVisit
                    traverser, { state with NodesToVisit = nodesToVisit }
                // todo: updater can be an operand in some languages
                | Updater(target, aType) when Set.contains target state.Vars ->
                    let state = makeVisited node state
                    let operandsIDs = 
                        match aType with
                        | Assign(id) -> [id]
                        | PlusAssign(id1, id2) -> [id1; id2]
                    let nodesToVisit = 
                        (state.NodesToVisit, operandsIDs)
                        ||> List.fold (fun acc op -> Set.add op acc)
                    traverser, { state with NodesToVisit = nodesToVisit }
                | Operation(tType, operands) when Set.contains node.Id state.NodesToVisit ->
                    let state = makeVisited node state
                    let nodesToVisit = 
                        (state.NodesToVisit, operands)
                        ||> List.fold (fun acc op -> Set.add op acc)
                    traverser, { state with NodesToVisit = nodesToVisit }
                | Literal(_) when Set.contains node.Id state.NodesToVisit -> 
                    traverser, makeVisited node state
                | LoopEnter
                | LoopExit
                | LoopBodyBeg
                | LoopBodyEnd 
                | StartNode -> traverser, makeVisited node state
                | VarRef(name) when Set.contains node.Id state.NodesToVisit ->
                    let state = makeVisited node state
                    traverser, { state with Vars = Set.add name state.Vars }
                | LoopNode ->
                    let state = makeVisited node state
                    let forkVarsSet = defaultArg (Map.tryFind node.Id state.VisitedForks) Set.empty
                    let newVarsSet = Set.difference state.Vars forkVarsSet
                    if Set.isEmpty newVarsSet
                    then
                        setLoopExitDirection traverser, state
                    else 
                        let visitedForks = Map.add node.Id state.Vars state.VisitedForks
                        setLoopBodyDirection traverser, { state with VisitedForks = visitedForks }
                | _ -> traverser, state
            if isFinishedMode traverser
            then state.VisitedNodes
            else findDdgNodes cfg traverser state
        let removeNeedlessNodes (ddg: BidirectGraph<_,_>) (neededNodes: list<GraphNode<_,_>>) =
            let neededSet = neededNodes |> List.map (fun n -> n.Id) |> Set.ofList
            let removeIfNotNeeded (node: GraphNode<_,_>) =
                if not <| Set.contains node.Id neededSet
                then 
                    let inEdges = ddg.InEdges node
                    let outEdges = ddg.OutEdges node
                    do ddg.RemoveVertex node |> ignore
                    inEdges
                    |> Seq.iter
                        (
                            fun ie ->
                                outEdges
                                |> Seq.iter
                                    (fun oe -> ddg.AddEdge(Edge(ie.Source, oe.Target)) |> ignore)
                        )
            let vertices = List.ofSeq ddg.Vertices
            do vertices |> List.iter removeIfNotNeeded
        let ddg =
            let g = BidirectGraph()
            do g.AddVerticesAndEdgeRange cfg.Graph.Edges |> ignore
            removeUnreachable g exitNode
        let traverser = init exitNode
        let initState = { 
            Vars = Set.empty; 
            VisitedForks = Map.empty; 
            NodesToVisit = Set.empty;
            VisitedNodes = [] }
        let neededNodes = findDdgNodes ddg traverser initState
        let ddgRoot = List.head neededNodes
        let ddgExit = Seq.last neededNodes
        do removeNeedlessNodes ddg neededNodes
        { Graph = ddg; Root = ddgRoot; Exit = ddgExit }

    let private addStartNode nodeId (cfg: GenericCFG<_,_>) =
        let newStartNode = { Id = nodeId; Type = StartNode }
        let curStartNodes = cfg.Graph.Vertices |> Seq.filter (fun v -> cfg.Graph.InDegree(v) = 0) |> List.ofSeq
        do cfg.Graph.AddVertex newStartNode |> ignore
        do curStartNodes |> List.iter (fun n -> cfg.Graph.AddEdge (Edge(newStartNode, n)) |> ignore)

    let private addExitNode nodeId (targetNodes: list<GraphNode<_,_>>) (cfg: GenericCFG<_,_>) =
        let newExitNode = { Id = nodeId; Type = ExitNode(targetNodes) }
        do cfg.Graph.AddVertex newExitNode |> ignore
        do targetNodes 
        |> List.iter 
            (fun n -> cfg.Graph.OutEdges n |> List.ofSeq |> List.iter (cfg.Graph.RemoveEdge >> ignore))
        do targetNodes |> List.iter (fun n -> cfg.Graph.AddEdge (Edge(n, newExitNode)) |> ignore)
        newExitNode

    let private ddgForNodes (nodes: list<GraphNode<_,_>>) (cfg: GenericCFG<_,_>) =
        let nodeWithMaxId = Seq.maxBy (fun v -> v.Id) cfg.Graph.Vertices
        do addStartNode (nodeWithMaxId.Id + 1) cfg
        let newExitNode = addExitNode (nodeWithMaxId.Id + 2) nodes cfg
        cfgToDdg newExitNode cfg

    /// Extracts DDG from passed generic CFG for a given node in CFG as 
    /// a target
    let ddgForNode (varRef: GraphNode<_,_>) (cfg: GenericCFG<_,_>) =
        ddgForNodes [varRef] cfg

    /// Extracts DDG from passed generic CFG. All the nodes with no
    /// successors are assumed as a target. Useful when DDG is needed for 
    /// method's or function's return statements
    let ddgForExits (cfg: GenericCFG<_,_>) = 
        let exitNodes = cfg.Graph.Vertices |> Seq.filter (fun v -> cfg.Graph.OutDegree(v) = 0)
        ddgForNodes (List.ofSeq exitNodes) cfg