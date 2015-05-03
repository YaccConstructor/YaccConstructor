module GenericGraphs

open QuickGraph

type OperationType =
| Replace
| Concat
| Arbitrary of string
// and so on ...

type UpdaterType =
// initializer node ID
| Assign of int
// type and operand nodes' IDs
| PlusAssign of int * int
// function with updatable arg passing

type GraphNodeType = 
// declared name and initializer node ID
| Declaration of string * int
// update target and updater type
| Updater of string * UpdaterType
// type and operand nodes' IDs
| Operation of OperationType * list<int>
| Literal of string
| VarRef of string
| LoopNode
| LoopEnter
| LoopExit
| LoopBodyBeg
| LoopBodyEnd
| OtherNode
| ExitNode of list<GraphNode>

and GraphNode = {
    Id: int
    Type: GraphNodeType }

module GraphNodeFuncs =
    let rec toString (node: GraphNode) =
        match node.Type with
        | Declaration(name, _) -> sprintf "decl(%s)" name
        | Updater(target, aType) -> 
            let typeStr =
                match aType with
                | Assign(_) -> "assign"
                | PlusAssign(_,_) -> "plusAssign"
            sprintf "%s(%s)" typeStr target
        | Operation(oType, operands) -> 
            match oType with
            | Replace -> "replace"
            | Concat -> "concat"
            | Arbitrary(name) -> sprintf "%s(%d)" name (List.length operands)
        | Literal(value) -> sprintf "literal(%s)" value
        | VarRef(name) -> sprintf "varRef(%s)" name
        | LoopNode -> "loopNode"
        | LoopEnter -> "loopEnter"
        | LoopExit -> "loopExit"
        | LoopBodyBeg -> "loopBodyBeg"
        | LoopBodyEnd -> "loopBodyEnd"
        | OtherNode -> "otherNode"
        | ExitNode(vars) -> 
            let labels = vars |> List.map toString
            sprintf "exit(%s)" <| Seq.fold (fun acc v -> acc + "," + v) "" labels

type GenericCFG = BidirectionalGraph<GraphNode, Edge<GraphNode>>

type DDG = {
    Graph: BidirectionalGraph<GraphNode, Edge<GraphNode>>
    Root: GraphNode }

module CfgTopoTraverser =
    open GraphUtils.TopoTraverser

    let isLoopNode (node: GraphNode) =
        match node.Type with LoopNode -> true | _ -> false
    let getId (node: GraphNode) = node.Id

module CfgTopoDownTraverser =
    open GraphUtils.TopoTraverser
    open CfgTopoTraverser

    let private getInputsNumber (cfg: GenericCFG) (node: GraphNode) = 
        cfg.InDegree node
    let private getAllNextNodes (cfg: GenericCFG) (node: GraphNode) = 
        cfg.OutEdges node
        |> List.ofSeq 
        |> List.map (fun e -> e.Target)
    let private getLoopNextNodes (cfg: GenericCFG) (node: GraphNode) =
        let outNodes = cfg.OutEdges node |> Seq.map (fun e -> e.Target)
        let bodyBegNode = 
            outNodes 
            |> Seq.find 
                (fun n -> match n.Type with | LoopBodyBeg -> true | _ -> false)
        let exitNode = 
            outNodes 
            |> Seq.find 
                (fun n -> match n.Type with | LoopExit -> true | _ -> false)
        bodyBegNode, exitNode
    let nextNode cfg (tState: TraverserState<GraphNode>) =
        nextNode getId (getInputsNumber cfg) (getAllNextNodes cfg) isLoopNode (getLoopNextNodes cfg) tState

module CfgTopoUpTraverser =
    open GraphUtils.TopoTraverser
    open CfgTopoTraverser

    let private getInputsNumber (cfg: GenericCFG) (node: GraphNode) = 
        cfg.OutDegree node
    let private getAllNextNodes (cfg: GenericCFG) (node: GraphNode) = 
        cfg.InEdges node
        |> List.ofSeq 
        |> List.map (fun e -> e.Source)
    let private getLoopNextNodes (cfg: GenericCFG) (node: GraphNode) =
        let inNodes = cfg.InEdges node |> Seq.map (fun e -> e.Source)
        let bodyEndNode = 
            inNodes 
            |> Seq.find 
                (fun n -> match n.Type with | LoopBodyEnd -> true | _ -> false)
        let enterNode = 
            inNodes 
            |> Seq.find 
                (fun n -> match n.Type with | LoopEnter -> true | _ -> false)
        bodyEndNode, enterNode
    let nextNode cfg (tState: TraverserState<GraphNode>) =
        nextNode getId (getInputsNumber cfg) (getAllNextNodes cfg) isLoopNode (getLoopNextNodes cfg) tState

module GenericCFGFuncs =
    open System.Collections.Generic
    open System.IO
    open GraphNodeFuncs
    open GraphUtils.TopoTraverser
    open CfgTopoUpTraverser

    let create (): GenericCFG = 
        new BidirectionalGraph<GraphNode, Edge<GraphNode>>(allowParallelEdges = false)

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

        let rec findDdgNodes (cfg: GenericCFG) traverser (state: ConvertState) =
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
                | LoopBodyEnd -> traverser, makeVisited node state
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

        let removeNeedlessNodes (ddg: GenericCFG) (neededNodes: list<GraphNode>) =
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
            let g = BidirectionalGraph<GraphNode, Edge<GraphNode>>()
            do g.AddVerticesAndEdgeRange cfg.Edges |> ignore
            g
        let traverser = init exitNode
        let initState = { 
            Vars = Set.empty; 
            VisitedForks = Map.empty; 
            NodesToVisit = Set.empty;
            VisitedNodes = [] }
        let neededNodes = findDdgNodes ddg traverser initState
        let ddgRoot = List.head neededNodes
        do removeNeedlessNodes ddg neededNodes
        { Graph = ddg; Root = ddgRoot }

    let exitNodeTypeMsg = "only VarRef typed nodes are supported as exit nodes for now"
    let private ddgForNodes (nodes: list<GraphNode>) (cfg: GenericCFG) =
        let nodeWithMaxId = Seq.maxBy (fun v -> v.Id) cfg.Vertices
        let newExitNodeId = 1 + nodeWithMaxId.Id
        let newExitNode = { Id = newExitNodeId; Type = ExitNode(nodes) }
        do cfg.AddVertex newExitNode |> ignore
        do nodes 
        |> List.iter 
            (fun n -> cfg.OutEdges n |> List.ofSeq |> List.iter (cfg.RemoveEdge >> ignore))
        do nodes |> List.iter (fun n -> cfg.AddEdge (Edge(n, newExitNode)) |> ignore)
        cfgToDdg newExitNode cfg

    let ddgForVar (varRef: GraphNode) (cfg: GenericCFG) =
        ddgForNodes [varRef] cfg

    let ddgForExits (cfg: GenericCFG) = 
        let exitNodes =  cfg.Vertices |> Seq.filter (fun v -> cfg.OutDegree(v) = 0)
        ddgForNodes (List.ofSeq exitNodes) cfg

    let toDot (cfg: GenericCFG) name path =
        use file = FileInfo(path).CreateText()
        file.WriteLine("digraph " + name + " {")
        cfg.Vertices
        |> List.ofSeq
        |> List.map 
            (
                fun node ->
                    let text = sprintf "%d_%s" node.Id (toString node)
                    node.Id.ToString() + " [label=\"" + text + "\"]"
            )
        |> List.iter file.WriteLine
        cfg.Edges
        |> List.ofSeq
        |> List.map
            (
                fun edge -> edge.Source.Id.ToString() + " -> " + edge.Target.Id.ToString()
            )
        |> List.iter file.WriteLine
        file.WriteLine("}")

module DDGFuncs =
    open System.IO
    open GraphNodeFuncs

    let toDot = GenericCFGFuncs.toDot