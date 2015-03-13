module ImmutableDDG

open QuickGraph

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.ControlFlow

open Utils
open Utils.StateMonad

open System.Collections.Generic
open System.IO
open JetBrains.ReSharper.Psi.CSharp.Tree

type NodeInfo = 
    {   Label: string
        AstElem: ITreeNode }

type DDNode =
| RootNode
| InnerNode of NodeInfo

type ImmutableDDG = 
    {   Graph: AdjacencyGraph<int, Edge<int>>
        NodeIdInfoDict: Dictionary<int, DDNode>
        ConnectionNode: int
        FinalNode: int }

module ImmutableDDGFuncs =
    // exception messages
    let private dstNodeIsNotSetMsg = 
        "cannot create edge - dst node is not set"
    let private noSuchNodeMsg = 
        "cannot create edge - src or dst node is not added to graph"

    let private addNode (ddGraph: ImmutableDDG) id info = 
        let nodes = ddGraph.NodeIdInfoDict
        if nodes.ContainsKey id
        then 
            nodes.[id] <- info
        else
            nodes.Add (id, info)
            ddGraph.Graph.AddVertex id |> ignore
        ddGraph

    let private failIfNoSuchKey (dict: Dictionary<int, _>) id =
        if (not << dict.ContainsKey) id
        then failwith noSuchNodeMsg

    let private addEdge (ddGraph: ImmutableDDG) src dst =
        let nodes = ddGraph.NodeIdInfoDict
        let graph = ddGraph.Graph
        failIfNoSuchKey nodes src
        failIfNoSuchKey nodes dst
        if (not << (ddGraph.Graph.ContainsEdge : int * int -> bool)) (src, dst)
        then graph.AddEdge (new Edge<int>(src, dst)) |> ignore
        ddGraph

    let create(finalNodeId: int, finalNodeInfo: DDNode) =
        let ddGraph = {
            Graph = new AdjacencyGraph<int, Edge<int>>()
            NodeIdInfoDict = new Dictionary<int, DDNode>()
            ConnectionNode = finalNodeId
            FinalNode = finalNodeId }
        addNode ddGraph finalNodeId finalNodeInfo

    let addEdgeFrom (ddGraph: ImmutableDDG) (srcNodeId, srcNodeInfo) =
        let ddg = addNode ddGraph srcNodeId srcNodeInfo
        addEdge ddg srcNodeId ddg.ConnectionNode

    let merge (dstGraph: ImmutableDDG) (srcGraph: ImmutableDDG) =
        srcGraph.NodeIdInfoDict 
        |> List.ofSeq
        |> List.iter (fun entry -> dstGraph.NodeIdInfoDict.Add (entry.Key, entry.Value))
        dstGraph.Graph.AddEdgeRange srcGraph.Graph.Edges |> ignore
        addEdge dstGraph srcGraph.FinalNode dstGraph.ConnectionNode

    let toDot (ddGraph: ImmutableDDG) name path =
        use file = FileInfo(path).CreateText()
        file.WriteLine("digraph " + name + " {")
        ddGraph.NodeIdInfoDict
        |> List.ofSeq
        |> List.map 
            (
                fun kvp ->
                    let text = 
                        match kvp.Value with
                        | RootNode -> "root"
                        | InnerNode(nodeInfo) -> nodeInfo.Label
                    kvp.Key.ToString() + " [label=\"" + text + "\"]"
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

type NodeIdProvider = 
    {   NextId: int
        GeneratedIds: Dictionary<ITreeNode, int> } 

module NodeIdProviderFuncs =
    let create = {NextId = 0; GeneratedIds = new Dictionary<ITreeNode, int>()}

    let getId (provider: NodeIdProvider) (node: ITreeNode) =
        let idsDict = provider.GeneratedIds
        if idsDict.ContainsKey node
        then idsDict.[node], provider
        else
            do idsDict.Add (node, provider.NextId)
            provider.NextId, {provider with NextId = provider.NextId + 1 }

type BuildState = 
    {   Graph: ImmutableDDG
        Provider: NodeIdProvider
        AstCfgMap: Dictionary<ITreeNode, IControlFlowElement> }

let rec build (cfgNode: IControlFlowElement) (varName: string) (state: BuildState) =
    let updateGraph treeNode label (state: BuildState) =
        let nodeId, provider' = NodeIdProviderFuncs.getId state.Provider treeNode
        let nodeInfo = { Label = label; AstElem = treeNode }
        let graph' = ImmutableDDGFuncs.addEdgeFrom state.Graph (nodeId, InnerNode(nodeInfo))
        nodeId, {state with Graph = graph'; Provider = provider'}

    let processExpr (expr: ICSharpExpression) (state: BuildState) =
        match expr with
        | :? ICSharpLiteralExpression as literalExpr ->
            let literalVal = literalExpr.Literal.GetText().Trim[|'\"'|]
            let label = "literal(" + literalVal + ")"
            let _, state' = updateGraph literalExpr label state
            state'
        // not implemented (but must precede IReferenceExpression case)
        | :? IInvocationExpression -> state
        | :? IReferenceExpression as refExpr ->
            let curVarName = refExpr.NameIdentifier.Name
            let label = "refExpr(" + curVarName + ")"
            let addedNodeId, state' = updateGraph refExpr label state
            if curVarName = varName
            then 
                let curConnectionNode = state'.Graph.ConnectionNode
                let graph' = { state'.Graph with ConnectionNode = addedNodeId }
                let state' = { state' with Graph = graph' }
                let curCfgNode = state'.AstCfgMap.[refExpr]
                let state' = build curCfgNode varName state'
                let graph' = { state'.Graph with ConnectionNode = curConnectionNode }
                { state' with Graph = graph' }
            else 
                failwith ("not implemented new ref case in processExpr")
        | _ -> failwith ("not implemented case in processExpr: " + expr.NodeType.ToString())

    let processAssignment (assignExpr: IAssignmentExpression) (state: BuildState) = 
        let assingnType, operands = 
            if assignExpr.AssignmentType = AssignmentType.EQ
            then 
                let ops = assignExpr.OperatorOperands |> List.ofSeq |> List.tail
                "assignment", ops
            else 
                let ops = assignExpr.OperatorOperands |> List.ofSeq
                "plusAssignment", ops
        let label = assingnType + "(" + varName + ")"
        let addedNode, state' = updateGraph assignExpr label state
        let graph' = { state'.Graph with ConnectionNode = addedNode }
        let state' = { state' with Graph = graph' }
        operands |> List.fold (fun st op -> processExpr op st) state'

    let processInitializer (initializer: ITreeNode) (state: BuildState) = 
        match initializer with
        | :? ICSharpExpression as expr ->
            processExpr expr state
        | :? IExpressionInitializer as exprInitializer ->
            processExpr exprInitializer.Value state
        | _ -> failwith ("not implemented case in processInitializer: " + initializer.NodeType.ToString())

    let processLocVarDecl (locVarDecl: ILocalVariableDeclaration) (state: BuildState) =
        let addedNode, state' = 
            updateGraph locVarDecl ("varDecl(" + varName + ")") state
        let graph' = { state'.Graph with ConnectionNode = addedNode }
        processInitializer locVarDecl.Initializer { state' with Graph = graph' }

    let processEntries (cfgNode: IControlFlowElement) (state: BuildState) =
        let curConnectionNode = state.Graph.ConnectionNode
        cfgNode.Entries
        |> List.ofSeq
        |> List.choose (fun rib -> if rib.Source <> null then Some(rib.Source) else None) 
        |> List.fold 
            (
                fun accState src ->
                    let graph' = { accState.Graph with ConnectionNode = curConnectionNode }
                    build src varName { accState with Graph = graph' }
            ) 
            state

    let astNode = cfgNode.SourceElement
    match astNode with
    | :? IAssignmentExpression as assignExpr 
        when 
            match assignExpr.Dest with 
            | :? IReferenceExpression as dst 
                when dst.NameIdentifier.Name = varName -> true
            | _ -> false
        -> 
            processAssignment assignExpr state
    | :? ILocalVariableDeclaration as locVarDecl
        when locVarDecl.NameIdentifier.Name = varName 
        -> 
            processLocVarDecl locVarDecl state
    | _ ->
        processEntries cfgNode state

let buildForVar (varRef: IReferenceExpression) (astCfgMap: Dictionary<ITreeNode, IControlFlowElement>) = 
    let provider = NodeIdProviderFuncs.create
    let finalNodeId, provider = NodeIdProviderFuncs.getId provider varRef
    let varName = varRef.NameIdentifier.Name
    let finalNodeInfo = { Label = "varRef(" + varName + ")"; AstElem = varRef }
    let graph = ImmutableDDGFuncs.create(finalNodeId, InnerNode(finalNodeInfo))
    let cfgNode = astCfgMap.[varRef]
    let initState = { Graph = graph; Provider = provider; AstCfgMap = astCfgMap }
    let res = build cfgNode varName initState
    res.Graph

//let rec build (cfgNode: IControlFlowElement) (varName: string): State<BuildState, unit> =
//    let updateGraph treeNode textInfo = withState<BuildState> {
//        let! st = getState
//        let nodeId, provider = NodeIdProviderFuncs.getId st.Provider treeNode
//        let nodeInfo = { Label = textInfo; AstElem = treeNode }
//        let graph = ImmutableDDGFuncs.addEdgeFrom st.Graph (nodeId, InnerNode(nodeInfo))
//        do! putState { st with Graph = graph; Provider = provider }
//        return nodeId
//    }
//
//    let processExpr (expr: ICSharpExpression) = withState<BuildState> {
//        match expr with
//        | :? ICSharpLiteralExpression as literalExpr ->
//            let literalVal = literalExpr.Literal.GetText().Trim[|'\"'|]
//            updateGraph literalExpr ("literal(" + literalVal + ")") |> ignore
//            return ()
//        // not implemented (but must precede IReferenceExpression case)
//        | :? IInvocationExpression -> return ()
//        | :? IReferenceExpression as refExpr ->
//            let curVarName = refExpr.NameIdentifier.Name
//            let! addedNodeId = updateGraph refExpr ("refExpr(" + curVarName + ")")
//            if curVarName = varName
//            then 
//                let! st = getState
//                let curConnectionNode = st.Graph.ConnectionNode
//                let graph = { st.Graph with ConnectionNode = addedNodeId }
//                do! putState { st with Graph = graph }
//                let curCfgNode = st.AstCfgMap.[refExpr]
//                build curCfgNode varName |> ignore
//                let! st = getState
//                let graph = { st.Graph with ConnectionNode = curConnectionNode }
//                do! putState { st with Graph = graph }
//                return ()
//            else 
//                do! failwith ("not implemented new ref case in processExpr")
//        | _ -> do! failwith ("not implemented case in processExpr: " + expr.NodeType.ToString())
//    }
//
//    let processAssignment (assignExpr: IAssignmentExpression) = withState<BuildState> {
//        let assingText, operands = 
//            if assignExpr.AssignmentType = AssignmentType.EQ
//            then 
//                "assignment", assignExpr.OperatorOperands |> List.ofSeq |> List.tail
//            else 
//                "plusAssignment", assignExpr.OperatorOperands |> List.ofSeq
//        let! addedNodeId = updateGraph assignExpr (assingText + "(" + varName + ")")
//        let! st = getState
//        let graph = { st.Graph with ConnectionNode = addedNodeId }
//        do! putState { st with Graph = graph }
//        operands |> List.iter (processExpr >> ignore)
//        return ()
//    }
//
//    let processInitializer (initializer: ITreeNode) = 
//        match initializer with
//        | :? ICSharpExpression as expr ->
//            processExpr expr
//        | :? IExpressionInitializer as exprInitializer ->
//            processExpr exprInitializer.Value
//        | _ -> failwith ("not implemented case in processInitializer: " + initializer.NodeType.ToString())
//
//    let processLocVarDecl (locVarDecl: ILocalVariableDeclaration) = withState<BuildState> {
//        let! addedNodeId = updateGraph locVarDecl ("varDecl(" + varName + ")")
//        let! st = getState
//        let graph = { st.Graph with ConnectionNode = addedNodeId }
//        do! putState { st with Graph = graph }
//        processInitializer locVarDecl.Initializer |> ignore
//        return ()
//    }
//
//    let processEntries (cfgNode: IControlFlowElement) = withState<BuildState> {
//        let! st = getState
//        let curConnectionNode = st.Graph.ConnectionNode
//        let f src = withState<BuildState> {
//            let! st = getState
//            let graph = { st.Graph with ConnectionNode = curConnectionNode }
//            do! putState { st with Graph = graph }
//            do! build src varName
//            return ()
//        }
//        cfgNode.Entries
//        |> List.ofSeq
//        |> List.choose (fun rib -> if rib.Source <> null then Some(rib.Source) else None) 
//        |> List.iter (f >> ignore)
//        return ()
//    }
//
//    let astNode = cfgNode.SourceElement
//    match astNode with
//    | :? IAssignmentExpression as assignExpr 
//        when 
//            match assignExpr.Dest with 
//            | :? IReferenceExpression as dst 
//                when dst.NameIdentifier.Name = varName -> true
//            | _ -> false
//        -> 
//            processAssignment assignExpr
//    | :? ILocalVariableDeclaration as locVarDecl
//        when locVarDecl.NameIdentifier.Name = varName 
//        -> 
//            processLocVarDecl locVarDecl
//    | _ ->
//        processEntries cfgNode
//
//let buildForVar (varRef: IReferenceExpression) (astCfgMap: Dictionary<ITreeNode, IControlFlowElement>) = 
//    let provider = NodeIdProviderFuncs.create
//    let finalNodeId, provider = NodeIdProviderFuncs.getId provider varRef
//    let varName = varRef.NameIdentifier.Name
//    let finalNodeInfo = { Label = "varRef(" + varName + ")"; AstElem = varRef }
//    let graph = ImmutableDDGFuncs.create(finalNodeId, InnerNode(finalNodeInfo))
//    let cfgNode = astCfgMap.[varRef]
//    let initState = { Graph = graph; Provider = provider; AstCfgMap = astCfgMap }
//    let buildCombutation = build cfgNode varName
//    let _, finalState = runState buildCombutation initState
//    finalState.Graph