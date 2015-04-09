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
// enter node index and body node index in Entries array
| LoopNode of int * int
| OtherNode

type GraphNode = {
    Id: int
    Type: GraphNodeType }

module GraphNodeFuncs =
    let toString (node: GraphNode) =
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
        | LoopNode(_,_) -> "loopNode"
        | OtherNode -> "otherNode"

type GenericCFG = BidirectionalGraph<GraphNode, Edge<GraphNode>>

type DDG = {
    Graph: BidirectionalGraph<GraphNode, Edge<GraphNode>>
    Root: GraphNode
    VarName: string }

module GenericCFGFuncs =
    open GraphNodeFuncs

    // exception messages 
    let private wrongCFGNodeTypeMsg = 
        "CFGNode was expected to be of VarRef type case"

    let private zeroCounterDecrMsg = "attempt to decrement zero counter"

    let private updaterAssumptionMsg = "updated assumption failed"

    type private DDGraphBuildInfo = {   
        Graph: BidirectionalGraph<GraphNode, Edge<GraphNode>>
        ConnectionNode: GraphNode }

    module private DDGBuildFuncs =
        // exception messages
        let private noSuchNodeMsg = 
            "cannot create edge - src or dst node is not added to graph"

        let private addNode (ddGraph: DDGraphBuildInfo) node = 
            if ddGraph.Graph.ContainsVertex node |> not
            then ddGraph.Graph.AddVertex node |> ignore
            ddGraph

        let private failIfNoSuchKey (ddGraph: DDGraphBuildInfo) node =
            if (not << ddGraph.Graph.ContainsVertex) node
            then failwith noSuchNodeMsg

        let private addEdge (ddGraph: DDGraphBuildInfo) src dst =
            let graph = ddGraph.Graph
            failIfNoSuchKey ddGraph src
            failIfNoSuchKey ddGraph dst
            if (not << (graph.ContainsEdge : GraphNode * GraphNode -> bool)) (src, dst)
            then graph.AddEdge (new Edge<GraphNode>(src, dst)) |> ignore
            ddGraph

        let create(finalNode: GraphNode) =
            let ddGraph = {
                Graph = new BidirectionalGraph<GraphNode, Edge<GraphNode>>()
                ConnectionNode = finalNode }
            addNode ddGraph finalNode

        let private addNodeAndEdge (ddGraph: DDGraphBuildInfo) srcNode =
            let ddg = addNode ddGraph srcNode
            addEdge ddg srcNode ddg.ConnectionNode

        let addConnectionNodeAndEdge (ddGraph: DDGraphBuildInfo) srcNode =
            let ddg' = addNodeAndEdge ddGraph srcNode
            { ddg' with ConnectionNode = srcNode }

    type private BuildState = {   
        GraphInfo: DDGraphBuildInfo
        VisitedForks: Map<int, Set<string>>
        NodesToVisit: Set<int> }

    let rec ddgForVar (varRef: GraphNode) (cfg: GenericCFG) = 
        let varRefName = function
        | VarRef(name) -> name
        | _ -> failwith wrongCFGNodeTypeMsg

        let setGraphConnectionNode nodeId (state: BuildState) =
            let graph' = { state.GraphInfo with ConnectionNode = nodeId }
            { state with GraphInfo = graph' }

        let rec build (cfNode: GraphNode) (cfg: GenericCFG) (varsSet: Set<string>) (state: BuildState) =
            let processEntries (entries: list<Edge<GraphNode>>) (varsSet: Set<string>) (state: BuildState) =
                let curConnectionNode = state.GraphInfo.ConnectionNode
                entries
                |> List.map (fun e -> e.Source) 
                |> List.fold 
                    (
                        fun accState src -> 
                            build src cfg varsSet accState 
                            |> setGraphConnectionNode curConnectionNode
                    ) 
                    state
            
            let entries, updState, updVarsSet =
                match cfNode.Type with
                | Declaration(name, initializerId) when Set.contains name varsSet ->
                    let gInfo' = DDGBuildFuncs.addConnectionNodeAndEdge state.GraphInfo cfNode
                    let nodesToVisit = Set.add initializerId state.NodesToVisit
                    let state' = { state with GraphInfo = gInfo'; NodesToVisit = nodesToVisit }
                    let entries = cfg.InEdges(cfNode) |> List.ofSeq
                    entries, state', varsSet
                // todo: updater can be an operand is some languages
                | Updater(target, aType) when Set.contains target varsSet ->
                    let gInfo' = DDGBuildFuncs.addConnectionNodeAndEdge state.GraphInfo cfNode
                    let operandsIDs = 
                        match aType with
                        | Assign(id) -> [id]
                        | PlusAssign(id1, id2) -> [id1; id2]
                    let nodesToVisit = 
                        operandsIDs 
                        |> List.fold (fun acc op -> Set.add op acc) state.NodesToVisit
                    let state' = { state with GraphInfo = gInfo'; NodesToVisit = nodesToVisit }
                    let entries = cfg.InEdges(cfNode) |> List.ofSeq
                    entries, state', varsSet
                | Operation(tType, operands) when Set.contains cfNode.Id state.NodesToVisit ->
                    let gInfo' = DDGBuildFuncs.addConnectionNodeAndEdge state.GraphInfo cfNode
                    let nodesToVisit = 
                        operands 
                        |> List.fold (fun acc op -> Set.add op acc) state.NodesToVisit
                    let state' = { state with GraphInfo = gInfo'; NodesToVisit = nodesToVisit }
                    let entries = cfg.InEdges(cfNode) |> List.ofSeq
                    entries, state', varsSet
                | Literal(value) when Set.contains cfNode.Id state.NodesToVisit ->
                    let gInfo' = DDGBuildFuncs.addConnectionNodeAndEdge state.GraphInfo cfNode
                    let state' = { state with GraphInfo = gInfo' }
                    let entries = cfg.InEdges(cfNode) |> List.ofSeq
                    entries, state', varsSet
                | VarRef(name) when Set.contains cfNode.Id state.NodesToVisit ->
                    let gInfo' = DDGBuildFuncs.addConnectionNodeAndEdge state.GraphInfo cfNode
                    let state' = { state with GraphInfo = gInfo' }
                    let varsSet' = Set.add name varsSet
                    let entries = cfg.InEdges(cfNode) |> List.ofSeq
                    entries, state', varsSet'
                | LoopNode(enterIndex, bodyIndex) ->
                    let gInfo' = DDGBuildFuncs.addConnectionNodeAndEdge state.GraphInfo cfNode
                    let forkVarsSet = 
                        match Map.tryFind cfNode.Id state.VisitedForks with
                        | None -> Set.empty
                        | Some(vs) -> vs
                    let newVarsSet = Set.difference varsSet forkVarsSet
                    if Set.isEmpty newVarsSet
                    then
                        let enterEdge = cfg.InEdge(cfNode, enterIndex)
                        [enterEdge], { state with GraphInfo = gInfo' }, varsSet
                    else 
                        let visitedForks' = Map.add cfNode.Id varsSet state.VisitedForks
                        let bodyExitEdge = cfg.InEdge(cfNode, bodyIndex)
                        [bodyExitEdge], { state with GraphInfo = gInfo'; VisitedForks = visitedForks' }, varsSet
                | _ -> 
                    let entries = cfg.InEdges(cfNode) |> List.ofSeq
                    entries, state, varsSet
            processEntries entries updVarsSet updState 

        let graphInfo = DDGBuildFuncs.create(varRef)
        let varName = varRefName varRef.Type
        let varsSet = Set.ofList [varName]
        let initState = { 
            GraphInfo = graphInfo;
            VisitedForks = Map.empty
            NodesToVisit = Set.empty }
        let resState = build varRef cfg varsSet initState
        let root = 
            // bad algo
            // todo: improve
            resState.GraphInfo.Graph.Vertices
            |> List.ofSeq
            |> List.find (fun n -> resState.GraphInfo.Graph.InDegree(n) = 0)
        { Graph = resState.GraphInfo.Graph; Root = root; VarName = varName }

    let create(): GenericCFG = 
        new BidirectionalGraph<GraphNode, Edge<GraphNode>>(allowParallelEdges = false)

module DDGFuncs =
    open System.IO
    open GraphNodeFuncs

    let toDot (ddGraph: DDG) name path =
        use file = FileInfo(path).CreateText()
        file.WriteLine("digraph " + name + " {")
        ddGraph.Graph.Vertices
        |> List.ofSeq
        |> List.map 
            (
                fun node ->
                    let text = toString node
                    node.Id.ToString() + " [label=\"" + text + "\"]"
            )
        |> List.iter file.WriteLine
        ddGraph.Graph.Edges
        |> List.ofSeq
        |> List.map
            (
                fun edge -> edge.Source.Id.ToString() + " -> " + edge.Target.Id.ToString()
            )
        |> List.iter file.WriteLine
        file.WriteLine("}")