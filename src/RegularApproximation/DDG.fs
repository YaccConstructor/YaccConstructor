/// Data dependency graph type  and processing functions
module DDG

open QuickGraph

open Utils
open GenericGraphs
open GraphUtils
open BidirectGraphFuns

/// Data depencendency graph. DDG's root always has StarNode type
/// and end has ExitNode type
type DDG<'Lit, 'OpInfo> = GraphWithSingleEnds<'Lit, 'OpInfo>

/// IsVisited part of the DFS algo specific for DDG
let ddgIsVisited (n: GraphNode<_,_>) v = Set.contains n.Id v

/// MakeVisited part of the DFS algo specific for DDG
let ddgMakeVisited (n: GraphNode<_,_>) v = Set.add n.Id v

let private exitNodeExpectedMsg = "Node of ExitNode type expected"
let private findTailRecCalls functionName = function
    | { Id = _; Type = ExitNode(preExits) } ->
        let isRecCallNode = function 
            | { Id = _; 
                Type = Operation(Arbitrary(Some { Name = operationName; Info = _ }), _) } ->
                operationName = functionName
            | _ -> false
        preExits |> List.filter isRecCallNode
    | _ -> failwith exitNodeExpectedMsg

let private removeRecCallToExitEdges (ddg: DDG<_,_>) recCallNodes newExitNodeId =
    let recCallNodesSet = 
        recCallNodes 
        |> List.map (fun n -> n.Id)
        |> Set.ofList
    let newExitPreds = 
        preds ddg.Exit ddg.Graph
        |> Seq.filter (fun n -> not <| Set.contains n.Id recCallNodesSet)
        |> List.ofSeq
    do ddg.Graph.InEdges ddg.Exit 
    |> List.ofSeq 
    |> List.iter (removeEdge ddg.Graph)
    do removeVertex ddg.Graph ddg.Exit
    let newFakeExitNode = { Id = newExitNodeId; Type = OtherNode }
    do newExitPreds
    |> List.iter (fun n -> addVerticesAndEdge n newFakeExitNode ddg.Graph)
    ddg.Graph, ddg.Root, newFakeExitNode

let private basicDdgDfsParts preProcess processNode getNextNodes postProcess = {
    IsVisited = ddgIsVisited
    MakeVisited = ddgMakeVisited
    PreProcess = preProcess
    ProcessNode = processNode
    GetNextNodes = getNextNodes
    PostProcess = postProcess }

let private showNextNodesStartNode node (graph: BidirectGraph<_,_>) =
    let nodePreds = preds node graph |> List.ofSeq
    match nodePreds |> List.tryFind (fun n -> n.Type = StartNode) with
    | Some(_) -> []
    | _ -> nodePreds

let private copyBranch (graph: BidirectGraph<_,_>) (exitBranchEnd: GraphNode<_,_>) startId =
    let state = ([], [], startId)
    let copyNode (node: GraphNode<_,_>) (edgesAcc, nodesAcc, lastId) =
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
    copyGraph algoParts exitBranchEnd Set.empty state

let private emptyExitBranchMsg = 
    "exit branch has only exit node, graph structure assumption failed"
let private copyExitBranch (graph: BidirectGraph<_,_>) (exitBranchEnd: GraphNode<_,_>) startId =
    let edges, nodes, lastId = copyBranch graph exitBranchEnd startId
    if List.length nodes = 1 && List.length edges = 0
    then failwith emptyExitBranchMsg
    else
        let graph = BidirectGraphFuns.create ()
        edges |> List.iter (addEdgeAndVertices graph)
        let roots, fakeExit =
            let nodesRev = List.rev nodes
            List.tail nodesRev, List.head nodesRev
        let fakeExitPreds = preds fakeExit graph |> List.ofSeq
        do removeVertex graph fakeExit
        let newExit = { Id = lastId + 1; Type = ExitNode(fakeExitPreds) }
        do fakeExitPreds |> List.iter (fun e -> addVerticesAndEdge e newExit graph)
        let graph = { Graph = graph; Exit = newExit; Roots = roots }
        graph, lastId + 1

let private onlyVisitDdgDfs node (nextNodes: GraphNode<_,_> -> list<GraphNode<_,_>>) =
    let doNothing n s = s
    let getNextNodes n s = nextNodes n, s
    let algoParts = basicDdgDfsParts doNothing doNothing getNextNodes doNothing
    fst <| dfs algoParts node Set.empty ()

let private collectNodesDdgDfs node (nextNodes: GraphNode<_,_> -> list<GraphNode<_,_>>) =
    let state: list<GraphNode<_,_>> = []
    let doNothing n s = s
    let processNode = List.cons
    let getNextNodes n s = nextNodes n, s
    let algoParts = basicDdgDfsParts doNothing processNode getNextNodes doNothing
    snd <| dfs algoParts node Set.empty state

let private removeExitBranch (graph: BidirectGraph<_,_>) exitBranchEnd recBranchEnd =
    let getNextNodes node = showNextNodesStartNode node graph
    let recBranchNodes = onlyVisitDdgDfs recBranchEnd getNextNodes
    let getNextNodes (node: GraphNode<_,_>) =
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

let private replaceRecCallWithAssigns (recCallBranch: GraphWithSingleRoot<_,_>) paramNames (startId: int) =
    let exitPredsList = 
        recCallBranch.Exits 
        |> Seq.map (fun e -> preds e recCallBranch.Graph |> List.ofSeq)
        |> List.ofSeq
    do recCallBranch.Exits |> List.iter (removeVertex recCallBranch.Graph)
    let assignsChains, lastId = 
        let computations = List.replicate (List.length exitPredsList) ()
        (([], startId), computations)
        ||> List.fold
            (fun (accChains, nextId) _ -> 
                let chain, lastId = createAssignsChain paramNames nextId
                chain :: accChains, lastId + 1)
    let connectExitPredsToAssignsChain (exitPreds, assignsChain) =     
        do exitPreds 
        |> List.iter 
            (fun ep -> addVerticesAndEdge ep assignsChain.Root recCallBranch.Graph)
        do assignsChain.Graph.Edges 
        |> Seq.iter (addEdgeAndVertices recCallBranch.Graph)
    do (exitPredsList, assignsChains)
    ||> List.zip
    |> List.iter connectExitPredsToAssignsChain
    let newExits = assignsChains |> List.map (fun ac -> ac.Exit)
    let graph = { Graph = recCallBranch.Graph; Root = recCallBranch.Root; Exits = newExits }
    graph, lastId

let private toLoop startId (recCallBranch: GraphWithSingleRoot<_,_>) (exitBranch: GraphWithSingleExit<_,_>) =
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
    do recCallBranch.Exits |> List.iter (fun e -> addVerticesAndEdge e loopBodyEnd resGraph)
    do addVerticesAndEdge loopBodyEnd loopNode resGraph
    // connect loop exit to exit branch
    do addVerticesAndEdge loopNode loopExit resGraph
    do exitBranch.Roots |> List.iter (fun r -> addVerticesAndEdge loopExit r resGraph)
    { Graph = resGraph; Root = resRoot; Exit = resExit }

/// Returns true if passed DDG represents tail recursive method or function
let isTailRecursive functionName (ddg: DDG<_,_>) =
    match findTailRecCalls functionName ddg.Exit with
    | [] -> false
    | _ -> true

/// Turns passed tail recursive DDG to DDG with loop. It makes approximation algo able to 
/// achieve automata building fixpoint (for tail recursive DDG it will not be acheved)
let tailRecursionToLoop functionName paramNames (ddg: DDG<_,_>): DDG<_,_> =
    match findTailRecCalls functionName ddg.Exit with
    | [] -> failwith "graph is not tail recursive"
    | recCallNodes -> 
        let lastId = ddg.Root.Id + 1
        let graph, root, exit = removeRecCallToExitEdges ddg recCallNodes (lastId + 1)
        let exitBranch, lastId = copyExitBranch graph exit (lastId + 2)
        do removeExitBranch graph exit <| List.head recCallNodes
        let recCallBranch = { Graph = graph; Root = root; Exits = recCallNodes }
        let recCallBranch, lastId = replaceRecCallWithAssigns recCallBranch paramNames <| lastId + 1
        toLoop (lastId + 1) recCallBranch exitBranch