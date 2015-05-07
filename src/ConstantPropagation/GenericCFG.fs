module GenericCFG

open GenericGraphElements
open DDG

type GenericCFG = {
    FunctionName: string
    Graph: BidirectGraph }

module GenericCFGFuncs =
    open System.Collections.Generic
    open System.IO
    open GraphNodeFuncs
    open GraphUtils.TopoTraverser
    open CfgTopoUpTraverser
    open QuickGraph

    let create name = {
        FunctionName = name
        Graph = BidirectGraphFuns.create () }

    type private ConvertState = {
        Vars: Set<string>
        VisitedForks: Map<int, Set<string>>
        NodesToVisit: Set<int>
        VisitedNodes: list<GraphNode> }

    let private cfgToDdg (exitNode: GraphNode) (cfg: GenericCFG) = 
        let wrongCfgNodeTypeMsg = 
            "Cfg node was expected to be of ExitNode type case"
        let getExitVars = function
        | ExitNode(vars) -> vars
        | _ -> failwith wrongCfgNodeTypeMsg 
        let rec findDdgNodes (cfg: BidirectGraph) traverser (state: ConvertState) =
            let makeVisited (node: GraphNode) (state: ConvertState) =
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
                | Literal(value) when Set.contains node.Id state.NodesToVisit -> 
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
        let removeNeedlessNodes (ddg: BidirectGraph) (neededNodes: list<GraphNode>) =
            let neededSet = neededNodes |> List.map (fun n -> n.Id) |> Set.ofList
            let removeIfNotNeeded (node: GraphNode) =
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
            g
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

    let private addStartNode nodeId (cfg: GenericCFG) =
        let newStartNode = { Id = nodeId; Type = StartNode }
        let curStartNodes = cfg.Graph.Vertices |> Seq.filter (fun v -> cfg.Graph.InDegree(v) = 0) |> List.ofSeq
        do cfg.Graph.AddVertex newStartNode |> ignore
        do curStartNodes |> List.iter (fun n -> cfg.Graph.AddEdge (Edge(newStartNode, n)) |> ignore)

    let private addExitNode nodeId (targetNodes: list<GraphNode>) (cfg: GenericCFG) =
        let newExitNode = { Id = nodeId; Type = ExitNode(targetNodes) }
        do cfg.Graph.AddVertex newExitNode |> ignore
        do targetNodes 
        |> List.iter 
            (fun n -> cfg.Graph.OutEdges n |> List.ofSeq |> List.iter (cfg.Graph.RemoveEdge >> ignore))
        do targetNodes |> List.iter (fun n -> cfg.Graph.AddEdge (Edge(n, newExitNode)) |> ignore)
        newExitNode

    let private ddgForNodes (nodes: list<GraphNode>) (cfg: GenericCFG) =
        let nodeWithMaxId = Seq.maxBy (fun v -> v.Id) cfg.Graph.Vertices
        do addStartNode (nodeWithMaxId.Id + 1) cfg
        let newExitNode = addExitNode (nodeWithMaxId.Id + 2) nodes cfg
        cfgToDdg newExitNode cfg

    let ddgForVar (varRef: GraphNode) (cfg: GenericCFG) =
        ddgForNodes [varRef] cfg

    let ddgForExits (cfg: GenericCFG) = 
        let exitNodes = cfg.Graph.Vertices |> Seq.filter (fun v -> cfg.Graph.OutDegree(v) = 0)
        ddgForNodes (List.ofSeq exitNodes) cfg