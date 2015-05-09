module DDG

open QuickGraph

open Utils
open GenericGraphElements
open GraphUtils
open BidirectGraphFuns

type DDG = GraphWithSingleEnds

let ddgIsVisited (n: GraphNode) v = Set.contains n.Id v
let ddgMakeVisited (n: GraphNode) v = Set.add n.Id v

let private exitNodeExpectedMsg = "Node of ExitNode type expected"
let private tryFindTailRecCall functionName = function
    | { Id = _; Type = ExitNode(preExits) } ->
        let isRecCallNode = function 
            | { Id = _; 
                Type = Operation(Arbitrary({ Name = operationName; Info = _ }), _) } ->
                operationName = functionName
            | _ -> false
        preExits |> List.tryFind isRecCallNode
    | _ -> failwith exitNodeExpectedMsg

let private removeRecCallToExitEdge (ddg: DDG) recCallNode newExitNodeId =
    let newExitPreds = 
        preds ddg.Exit ddg.Graph
        |> Seq.filter (fun n -> n.Id <> recCallNode.Id)
        |> List.ofSeq
    do ddg.Graph.InEdges ddg.Exit 
    |> List.ofSeq 
    |> List.iter (ddg.Graph.RemoveEdge >> ignore)
    do ddg.Graph.RemoveVertex ddg.Exit |> ignore
    let newExitNode = { Id = newExitNodeId; Type = ExitNode(newExitPreds) }
    do newExitPreds
    |> List.iter (fun n -> addVerticesAndEdge n newExitNode ddg.Graph)
    ddg.Graph, ddg.Root, newExitNode

let private basicDdgDfsParts preProcess processNode getNextNodes postProcess = {
    IsVisited = ddgIsVisited
    MakeVisited = ddgMakeVisited
    PreProcess = preProcess
    ProcessNode = processNode
    GetNextNodes = getNextNodes
    PostProcess = postProcess }

let private showNextNodesStartNode node (graph: BidirectGraph) =
    let nodePreds = preds node graph |> List.ofSeq
    match nodePreds |> List.tryFind (fun n -> n.Type = StartNode) with
    | Some(_) -> []
    | _ -> nodePreds

let private copyExitBranch (graph: BidirectGraph) (exitBranchEnd: GraphNode) startId =
    let state = ([], [], startId)
    let copyNode (node: GraphNode) (edgesAcc, nodesAcc, lastId) =
        let copyOfNode = { Id = lastId + 1; Type = node.Type }
        copyOfNode, (edgesAcc, nodesAcc, lastId + 1)
    let addEdge succ par (edgesAcc, nodesAcc, lastId) =
        (Edge (par, succ) :: edgesAcc, nodesAcc, lastId)
    let addNode node (edgesAcc, nodesAcc, lastId) =
        (edgesAcc, node :: nodesAcc, lastId)
    let showNextNodes node state =
        showNextNodesStartNode node graph
    let getNextNodes node state = showNextNodes node state, state
    let algoParts = {
        CopyNode = copyNode
        AddEdge = addEdge
        AddFirstNode = addNode
        AddLastNode = addNode
        ShowNextNodes = showNextNodes
        GetNextNodes = getNextNodes
        BasicDfsParts = basicDdgDfsParts }
    let edges, nodes, lastId = 
        copyGraph algoParts exitBranchEnd Set.empty state
    let graph = BidirectGraphFuns.create ()
    if List.length nodes = 1 && List.length edges = 0
    then 
        graph.AddVertex (List.head nodes) |> ignore
        let graph = { Graph = graph; Exit = List.head nodes; Roots = nodes }
        graph, lastId
    else
        edges |> List.iter (fun e -> graph.AddVerticesAndEdge e |> ignore)
        let nodesRev = List.rev nodes
        let graph = { Graph = graph; Exit = List.head nodesRev; Roots = List.tail nodesRev }
        graph, lastId

let private onlyVisitDdgDfs node (nextNodes: GraphNode -> list<GraphNode>) =
    let doNothing n s = s
    let getNextNodes n s = nextNodes n, s
    let algoParts = basicDdgDfsParts doNothing doNothing getNextNodes doNothing
    fst <| dfs algoParts node Set.empty ()

let private collectNodesDdgDfs node (nextNodes: GraphNode -> list<GraphNode>) =
    let state: list<GraphNode> = []
    let doNothing n s = s
    let processNode = List.cons
    let getNextNodes n s = nextNodes n, s
    let algoParts = basicDdgDfsParts doNothing processNode getNextNodes doNothing
    snd <| dfs algoParts node Set.empty state

let private removeExitBranch (graph: BidirectGraph) exitBranchEnd recBranchEnd =
    let getNextNodes node = showNextNodesStartNode node graph
    let recBranchNodes = onlyVisitDdgDfs recBranchEnd getNextNodes
    let getNextNodes (node: GraphNode) =
        if Set.contains node.Id recBranchNodes
        then []
        else showNextNodesStartNode node graph
    let exitBranchNodes = collectNodesDdgDfs exitBranchEnd getNextNodes
    do exitBranchNodes |> Seq.iter (graph.RemoveVertex >> ignore)

// todo: here I use 0 as argument of Assign constructor and this number is usless 
// because DDG will be passed to FsaGenerator where this info is not used.
// So it is clear that GraphNode must be split into 2 separate types - one
// for GenericCFG, the other for DDG
let private createAssignsChain paramNames startId =
    let lastId, chainNodes = 
        ((startId, []), paramNames) 
        ||> List.fold 
            (fun (lastId, nodes) name -> 
                let assignNode = { Id = lastId + 1; Type = Updater(name, Assign(0)) }
                lastId + 1, assignNode :: nodes)
    let rec createChain nodes accGraph =
        match nodes with
        | n1 :: [] -> 
            do addVertex accGraph n1
            accGraph
        | n1 :: n2 :: ns -> 
            do addVerticesAndEdge n1 n2 accGraph
            createChain (n2 :: ns) accGraph
        | [] -> accGraph
    let chainGraph = BidirectGraphFuns.create ()
    let chainGraph = createChain chainNodes chainGraph
    let chainBeg = List.head chainNodes
    let chainEnd = Seq.last chainNodes
    { Graph = chainGraph; Root = chainBeg; Exit = chainEnd }, lastId

let private replaceRecCallWithAssigns (recCallBranch: GraphWithSingleEnds) paramNames startId =
    let assignsChain, lastId = createAssignsChain paramNames startId
    let exitPreds = preds recCallBranch.Exit recCallBranch.Graph |> List.ofSeq 
    do removeVertex recCallBranch.Graph recCallBranch.Exit
    do exitPreds |> List.iter (fun ep -> addVerticesAndEdge ep assignsChain.Root recCallBranch.Graph)
    do assignsChain.Graph.Edges |> Seq.iter (addEdgeAndVertices recCallBranch.Graph)
    let graph = { Graph = recCallBranch.Graph; Root = recCallBranch.Root; Exit = assignsChain.Exit }
    graph, lastId

let private toLoop startId (recCallBranch: GraphWithSingleEnds) (exitBranch: GraphWithSingleExit) =
    let resGraph = recCallBranch.Graph
    let resRoot = recCallBranch.Root
    let resExit = exitBranch.Exit
    let loopNode = { Id = startId; Type = LoopNode }
    let loopEnter, loopBodyEnd,
        loopExit, loopBodyBeg,
        lastNodeId = GraphNodeFuncs.createLoopMarkers (startId + 1)
    // add all exitBranch edges and vertices to recCallBranch
    do exitBranch.Graph.Edges |> Seq.iter (addEdgeAndVertices resGraph)
    // remove start node out edges
    let startOutEdges = resGraph.OutEdges resRoot |> List.ofSeq
    do startOutEdges |> List.iter (removeEdge resGraph)
    // connect start to loop enter
    do addVerticesAndEdge resRoot loopEnter resGraph
    do addVerticesAndEdge loopEnter loopNode resGraph
    // connect loop body beg to rec call branch
    let startSuccs = startOutEdges |> List.map (fun e -> e.Target)
    do addVerticesAndEdge loopNode loopBodyBeg resGraph
    do startSuccs |> List.iter (fun s -> addVerticesAndEdge loopBodyBeg s resGraph)
    // connect rec call branch to loop body end
    do addVerticesAndEdge recCallBranch.Exit loopBodyEnd resGraph
    do addVerticesAndEdge loopBodyEnd loopNode resGraph
    // connect loop exit to exit branch
    do addVerticesAndEdge loopNode loopExit resGraph
    do exitBranch.Roots |> List.iter (fun r -> addVerticesAndEdge loopExit r resGraph)
    { Graph = resGraph; Root = resRoot; Exit = resExit }

let isTailRecursive functionName (ddg: DDG) =
    match tryFindTailRecCall functionName ddg.Exit with
    | Some(_) -> true
    | None -> false

let tailRecursionToLoop functionName paramNames (ddg: DDG): DDG =
    match tryFindTailRecCall functionName ddg.Exit with
    | Some(recCallNode) -> 
        let lastId = ddg.Root.Id + 1
        let graph, root, exit = removeRecCallToExitEdge ddg recCallNode (lastId + 1)
        let exitBranch, lastId = copyExitBranch graph exit (lastId + 2)
        do removeExitBranch graph exit recCallNode
        let recCallBranch = { Graph = graph; Root = root; Exit = recCallNode }
        let recCallBranch, lastId = replaceRecCallWithAssigns recCallBranch paramNames <| lastId + 1
        toLoop (lastId + 1) recCallBranch exitBranch
    | None -> failwith "graph is not tail recursive"