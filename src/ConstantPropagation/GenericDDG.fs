module GenericDDG

open QuickGraph

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.CSharp.Tree

open Utils
open CSharpCFGInfo

open System.Collections.Generic
open System.IO
open GenericCFG

type DDGraph = {
    Graph: BidirectionalGraph<CFGNode, Edge<CFGNode>> }

module DDGraphFuncs =
    open JetBrains.ReSharper.Psi
    open GenericCFG.CFGNodeFuncs

    // exception messages 
    let private wrongCFGNodeTypeMsg = 
        "CFGNode was expected to be of VarRef type case"

    let private zeroCounterDecrMsg = "attempt to decrement zero counter"

    let private updaterAssumptionMsg = "updated assumption failed"

    type private DDGraphBuildInfo = {   
        Graph: BidirectionalGraph<CFGNode, Edge<CFGNode>>
        ConnectionNode: CFGNode }

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
            if (not << (graph.ContainsEdge : CFGNode * CFGNode -> bool)) (src, dst)
            then graph.AddEdge (new Edge<CFGNode>(src, dst)) |> ignore
            ddGraph

        let create(finalNode: CFGNode) =
            let ddGraph = {
                Graph = new BidirectionalGraph<CFGNode, Edge<CFGNode>>()
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
        OperandsCounter: int }

    let rec buildForVar (varRef: CFGNode) (cfg: GenericCFG) = 
        let varRefName = function
        | VarRef(name) -> name
        | _ -> failwith wrongCFGNodeTypeMsg

        let setGraphConnectionNode nodeId (state: BuildState) =
            let graph' = { state.GraphInfo with ConnectionNode = nodeId }
            { state with GraphInfo = graph' }

        let decrCounter state = 
            if state.OperandsCounter = 0
            then failwith zeroCounterDecrMsg
            { state with OperandsCounter = state.OperandsCounter - 1 }

        let rec build (cfNode: CFGNode) (cfg: GenericCFG) (varsSet: Set<string>) (state: BuildState) =
            let processEntries (entries: list<Edge<CFGNode>>) (varsSet: Set<string>) (state: BuildState) =
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
                | Declaration(name) when Set.contains name varsSet ->
                    let gInfo' = DDGBuildFuncs.addConnectionNodeAndEdge state.GraphInfo cfNode
                    let newCounterVal = state.OperandsCounter + 1
                    let state' = { state with GraphInfo = gInfo'; OperandsCounter = newCounterVal }
                    let entries = cfg.Graph.InEdges(cfNode) |> List.ofSeq
                    entries, state', varsSet
                // updater can be an operand is some languages
                | Updater(target, aType) when Set.contains target varsSet ->
                    let gInfo' = DDGBuildFuncs.addConnectionNodeAndEdge state.GraphInfo cfNode
                    // assume updater cannot be an operand (for now)
                    if state.OperandsCounter <> 0
                    then failwith updaterAssumptionMsg
                    let operandsNum = 
                        match aType with
                        | Assign -> 1
                        | PlusAssign -> 2
                    let newCounterVal = state.OperandsCounter + operandsNum
                    let state' = { state with GraphInfo = gInfo'; OperandsCounter = newCounterVal }
                    let entries = cfg.Graph.InEdges(cfNode) |> List.ofSeq
                    entries, state', varsSet
                | Operation(tType, operandsNum) when state.OperandsCounter <> 0 ->
                    let gInfo' = DDGBuildFuncs.addConnectionNodeAndEdge state.GraphInfo cfNode
                    let state' = decrCounter state
                    let newCounterVal = state'.OperandsCounter + operandsNum
                    let state' = { state with GraphInfo = gInfo'; OperandsCounter = newCounterVal }
                    let entries = cfg.Graph.InEdges(cfNode) |> List.ofSeq
                    entries, state', varsSet
                | Literal(value) when state.OperandsCounter <> 0 ->
                    let gInfo' = DDGBuildFuncs.addConnectionNodeAndEdge state.GraphInfo cfNode
                    let state' = decrCounter state
                    let state' = { state' with GraphInfo = gInfo' }
                    let entries = cfg.Graph.InEdges(cfNode) |> List.ofSeq
                    entries, state', varsSet
                | VarRef(name) when state.OperandsCounter <> 0 ->
                    let gInfo' = DDGBuildFuncs.addConnectionNodeAndEdge state.GraphInfo cfNode
                    let state' = decrCounter state
                    let state' = { state' with GraphInfo = gInfo' }
                    let varsSet' = Set.add name varsSet
                    let entries = cfg.Graph.InEdges(cfNode) |> List.ofSeq
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
                        let enterEdge = cfg.Graph.InEdge(cfNode, enterIndex)
                        [enterEdge], { state with GraphInfo = gInfo' }, varsSet
                    else 
                        let visitedForks' = Map.add cfNode.Id varsSet state.VisitedForks
                        let bodyExitEdge = cfg.Graph.InEdge(cfNode, bodyIndex)
                        [bodyExitEdge], { state with GraphInfo = gInfo'; VisitedForks = visitedForks' }, varsSet
                | _ -> 
                    let entries = cfg.Graph.InEdges(cfNode) |> List.ofSeq
                    entries, state, varsSet
            processEntries entries updVarsSet updState 

        let graphInfo = DDGBuildFuncs.create(varRef)
        let varsSet = Set.ofList [(varRefName varRef.Type)]
        let initState = { 
            GraphInfo = graphInfo;
            VisitedForks = Map.empty;
            OperandsCounter = 0 }
        let resState = build varRef cfg varsSet initState
        let ddGraph: DDGraph = { 
            Graph = resState.GraphInfo.Graph }
        ddGraph

    let toDot (ddGraph: DDGraph) name path =
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
                fun edge -> edge.Source.ToString() + " -> " + edge.Target.ToString()
            )
        |> List.iter file.WriteLine
        file.WriteLine("}")