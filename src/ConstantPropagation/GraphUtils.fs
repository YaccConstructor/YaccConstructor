module GraphUtils

open System.Collections.Generic

type VisitedChecker<'Node, 'VisSet> = 'Node -> 'VisSet -> bool
type NodeVisitor<'Node, 'VisSet> = 'Node -> 'VisSet -> 'VisSet
type NodeProcessor<'Node, 'State> = 'Node -> 'State -> 'State
type NodesGenerator<'Node, 'State> = 'Node -> 'State -> list<'Node> * 'State

type DfsParts<'Node, 'VisSet, 'State> = {
    IsVisited: VisitedChecker<'Node, 'VisSet>
    MakeVisited: NodeVisitor<'Node, 'VisSet>
    PreProcess: NodeProcessor<'Node, 'State>
    ProcessNode: NodeProcessor<'Node, 'State>
    GetNextNodes: NodesGenerator<'Node, 'State>
    PostProcess: NodeProcessor<'Node, 'State> }

/// Generic DFS algorithm
let rec dfs  (algoParts: DfsParts<'N, 'V, 'S>) (node: 'N) (visited: 'V) (state: 'S) =
    if not <| algoParts.IsVisited node visited
    then
        let visited = algoParts.MakeVisited node visited
        let nextNodes, state = 
            state 
            |> algoParts.PreProcess node 
            |> algoParts.ProcessNode node 
            |> algoParts.GetNextNodes node
        let visited, state =
            ((visited, state), nextNodes)
            ||> List.fold 
                (fun (v, s) n -> dfs algoParts n v s)
        let state = algoParts.PostProcess node state
        visited, state
    else visited, state

type CopyGraphParts<'Node, 'CopyNode, 'State1, 'VisSet, 'State2> = {
    CopyNode: 'Node -> 'State1 -> 'CopyNode * 'State1
    AddEdge: 'CopyNode -> 'CopyNode -> 'State1 -> 'State1
    AddFirstNode: 'CopyNode -> 'State1 -> 'State1
    AddLastNode: 'CopyNode -> 'State1 -> 'State1
    ShowNextNodes: 'Node -> 'State1 -> list<'Node>
    GetNextNodes: 'Node -> 'State1 -> list<'Node> * 'State1
    BasicDfsParts: NodeProcessor<'Node, 'State2> -> 
                   NodeProcessor<'Node, 'State2> ->
                   NodesGenerator<'Node, 'State2> ->
                   NodeProcessor<'Node, 'State2> ->
                   DfsParts<'Node, 'VisSet, 'State2> }

let copyGraph 
        (algoParts: CopyGraphParts<'N, 'CN, 'S1, 'V, 'S2>)
        (node: 'N) 
        (visited: 'V)
        (state: 'S1) =
    let state' = (state, [], Dictionary<'N, 'CN>())
    let processNode node (state, parentsStack, mapping: Dictionary<'N, 'CN>) = 
        let copyOfNode, state = algoParts.CopyNode node state
        do mapping.Add (node, copyOfNode)
        let state =
            if List.isEmpty parentsStack
            then state |> algoParts.AddFirstNode copyOfNode
            else algoParts.AddEdge (List.head parentsStack) copyOfNode state
        (state, copyOfNode :: parentsStack, mapping)
    let connectToTraversedSuccessors (node: 'N) (mapping: Dictionary<'N, 'CN>) (state: 'S1) =
        let tryGetTraversedVersion (node: 'N) =
            match mapping.TryGetValue node with
            | true, s -> Some(s)
            | false, _ -> None
        let copyOfNode = mapping.[node]
        algoParts.ShowNextNodes node state
        |> List.choose tryGetTraversedVersion
        |> List.fold (fun accSt succ -> algoParts.AddEdge copyOfNode succ state) state
    let postProcess node (state, parentsStack, mapping: Dictionary<'N, 'CN>) = 
        let state =
            if List.isEmpty <| algoParts.ShowNextNodes node state
            then algoParts.AddLastNode mapping.[node] state
            else connectToTraversedSuccessors node mapping state
        (state, List.tail parentsStack, mapping)
    let getNextNodes' n (state, parentsStack, mapping) =
        let next, state = algoParts.GetNextNodes n state
        next, (state, parentsStack, mapping)
    let preProcess n s = s
    let dfsAlgoParts = algoParts.BasicDfsParts preProcess processNode getNextNodes' postProcess
    let _, (state, _, _) = dfs dfsAlgoParts node visited state'
    state

module TopoTraverser =
    type TraverserMode = Init | Normal | LoopNodeMode | Finished 
    type TraverserState<'Node> = {
        NodesStack: list<'Node>
        VisitedCounters: Map<int, int>
        Mode: TraverserMode
        LoopFirstNodes: Option<'Node * 'Node>
        LoopBodyDirection: bool
        PrevNodes: list<'Node> }

    let init node = { 
        NodesStack = [node]; 
        VisitedCounters = Map.empty; 
        Mode = Init;
        LoopFirstNodes = None;
        LoopBodyDirection = false;
        PrevNodes = [] }

    let isLoopNodeMode (tState: TraverserState<'n>) = 
        match tState.Mode with
        | LoopNodeMode -> true
        | _ -> false

    let isFinishedMode (tState: TraverserState<'n>) = 
        match tState.Mode with
        | Finished -> true
        | _ -> false

    let private pushPrevNode node prevNodes =
        if List.length prevNodes >= 2
        then node :: [List.head prevNodes]
        else node :: prevNodes

    let prevNode (tState: TraverserState<'n>) =
        if List.length tState.PrevNodes < 2
        then None
        else Some(List.head <| List.tail tState.PrevNodes)

    let private directionSetProblemMsg = 
        "direction can be set only on LoopNode mode"
    let setLoopBodyDirection (tState: TraverserState<'n>) =
        if not <| isLoopNodeMode tState
        then failwith directionSetProblemMsg
        else { tState with LoopBodyDirection = true }

    let setLoopExitDirection (tState: TraverserState<'n>) =
        if not <| isLoopNodeMode tState
        then failwith directionSetProblemMsg
        else { tState with LoopBodyDirection = false }

    let private setFinishedIfNeeded (tState: TraverserState<'n>) =
        if not <| isLoopNodeMode tState && List.isEmpty tState.NodesStack
        then { tState with Mode = Finished }
        else tState

    let private loopModeMsg =
        @"Can't provide next node, current node is loop node. 
        Use loopBodyFirstNode or loopExitFirstNode function instead"
    let private noMoreNodesMsg = "All nodes has already been provided"
    let private nextUsualNode getId getInputsNumber getAllNextNodes isLoopNode getLoopNextNodes (tState: TraverserState<'n>) =
        let getVisitedCounter nodeId (tState: TraverserState<'n>) =
            (Map.tryFind nodeId tState.VisitedCounters, 0) ||> defaultArg
        let canBeVisited node =
            if isLoopNode node
            then true
            else
                match tState.Mode with
                | Init -> true
                | _ -> 
                    let already = (getVisitedCounter (getId node) tState) + 1 
                    let needed = getInputsNumber node
                    already = needed
        let incrVisitedCounter visitedCounters node =
            let nodeId = getId node
            Map.add nodeId (1 + getVisitedCounter nodeId tState) visitedCounters
        let nodesToSkip, restStack = List.partition (not << canBeVisited) tState.NodesStack
        let nodeToVisit = List.head restStack
        let visitedCounters = 
            List.fold incrVisitedCounter tState.VisitedCounters nodesToSkip 
            |> Map.remove (getId nodeToVisit)
        let tState = 
            { tState with 
                NodesStack = List.tail restStack
                VisitedCounters = visitedCounters
                PrevNodes = pushPrevNode nodeToVisit tState.PrevNodes }
        let tState =
            if isLoopNode nodeToVisit
            then { 
                tState with 
                    Mode = LoopNodeMode
                    LoopFirstNodes = Some(getLoopNextNodes nodeToVisit) }
            else { 
                tState with 
                    Mode = Normal;
                    NodesStack = (getAllNextNodes nodeToVisit) @ tState.NodesStack }
        nodeToVisit, setFinishedIfNeeded tState

    let private notLoopModeMsg =
        @"Can't provide next node, current node is not loop node. 
        Use nextNode function instead"
    let private inconsistentLoopNodeState =
        "Inconsistent state: current mode is LoopNode but TraverserState.LoopFirstNodes is None"
    let private nextAfterLoopNode (bodyOrExit: 'n * 'n -> 'n) getAllNextNodes (tState: TraverserState<'n>) =
        let firstAfterLoopNode =
            match tState.LoopFirstNodes with
            | None -> failwith inconsistentLoopNodeState
            | Some(fn) -> bodyOrExit fn
        let tState =
            { tState with
                NodesStack = (getAllNextNodes firstAfterLoopNode) @ tState.NodesStack
                Mode = Normal
                PrevNodes = pushPrevNode firstAfterLoopNode tState.PrevNodes }
        firstAfterLoopNode, setFinishedIfNeeded tState

    let private loopBodyFirstNode getAllNextNodes (tState: TraverserState<'n>) =
        nextAfterLoopNode fst getAllNextNodes tState

    let private loopExitFirstNode getAllNextNodes (tState: TraverserState<'n>) =
        nextAfterLoopNode snd getAllNextNodes tState

    let nextNode getId getInputsNumber getAllNextNodes isLoopNode getLoopNextNodes (tState: TraverserState<'n>) =
        if isFinishedMode tState
        then failwith noMoreNodesMsg
        elif isLoopNodeMode tState
        then 
            if tState.LoopBodyDirection
            then loopBodyFirstNode getAllNextNodes tState
            else loopExitFirstNode getAllNextNodes tState
        else nextUsualNode getId getInputsNumber getAllNextNodes isLoopNode getLoopNextNodes tState