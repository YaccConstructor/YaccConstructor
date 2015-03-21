module YC.ReSharper.AbstractAnalysis.LanguageApproximation.DataDependencyGraph

open QuickGraph

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.CSharp.Tree

open Utils
open CSharpCFGInfo

open System.Collections.Generic
open System.IO

type NodeInfo = {   
    Label: string
    AstElem: ITreeNode }

type DDNode =
| RootNode
| InnerNode of NodeInfo

type DDGraph = {
    Graph: AdjacencyGraph<int, Edge<int>>
    NodeIdInfoDict: Dictionary<int, DDNode>
    RootId: int }

module DDGraphFuncs =
    // exception messages 
    let private multipleCfgNodesForAstNodeMsg = 
        "ast node maps to multiple cfg nodes where single mapping expected"

    let private forNodeIncorrectMappingMsg =
        "_for_ node mapping is incorrect, it must be mapped to exactly 3 cfg nodes"

    let private bodyForNodeManyInputsMsg =
        "body for node has more than one input"

    let private emptyListPopMsg = "pop on empty list"
    let private emptyListVisitMsg = "visit on empty list"

    type private DDGraphBuildInfo = {   
        Graph: AdjacencyGraph<int, Edge<int>>
        NodeIdInfoDict: Dictionary<int, DDNode>
        ConnectionNode: int
        FinalNodeId: int
        MarkedNodes: list<int>
        RootId: option<int> }

    module private DDGBuildFuncs =
        // exception messages
        let private noSuchNodeMsg = 
            "cannot create edge - src or dst node is not added to graph"

        let private addNode (ddGraph: DDGraphBuildInfo) id info = 
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

        let private addEdge (ddGraph: DDGraphBuildInfo) src dst =
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
                FinalNodeId = finalNodeId
                MarkedNodes = []
                RootId = None }
            addNode ddGraph finalNodeId finalNodeInfo

        let addNodeAndEdge (ddGraph: DDGraphBuildInfo) srcNodeId srcNodeInfo =
            let ddg = addNode ddGraph srcNodeId srcNodeInfo
            addEdge ddg srcNodeId ddg.ConnectionNode

        let addMarkedNodeAndEdge (ddg: DDGraphBuildInfo) srcNodeId srcNodeInfo =
            let ddg' = addNodeAndEdge ddg srcNodeId srcNodeInfo
            { ddg' with MarkedNodes = srcNodeId :: ddg'.MarkedNodes }

        let markConnectionNode (ddg: DDGraphBuildInfo) =
            { ddg with MarkedNodes = ddg.ConnectionNode :: ddg.MarkedNodes }

        let foldMarkedOnExisting (ddg: DDGraphBuildInfo) existingId =
            failIfNoSuchKey ddg.NodeIdInfoDict existingId
            ddg.MarkedNodes
            |> List.fold (fun accDdg id -> addEdge accDdg existingId id) ddg
            |> fun ddg -> { ddg with MarkedNodes = [] }

        let foldMarkedOnNew (ddg: DDGraphBuildInfo) newNodeId newNodeInfo =
            let ddg' = addNode ddg newNodeId newNodeInfo
            foldMarkedOnExisting ddg' newNodeId

        let merge (dstGraph: DDGraphBuildInfo) (srcGraph: DDGraphBuildInfo) =
            srcGraph.NodeIdInfoDict 
            |> List.ofSeq
            |> List.iter (fun entry -> dstGraph.NodeIdInfoDict.Add (entry.Key, entry.Value))
            dstGraph.Graph.AddEdgeRange srcGraph.Graph.Edges |> ignore
            addEdge dstGraph srcGraph.FinalNodeId dstGraph.ConnectionNode

    type private NodeIdProvider = {   
        NextId: int
        GeneratedIds: Dictionary<ITreeNode, int> } 

    module private NodeIdProviderFuncs =
        let create = { NextId = 0; GeneratedIds = new Dictionary<ITreeNode, int>() }

        let tryGetId (provider: NodeIdProvider) (node: ITreeNode) =
            let idsDict = provider.GeneratedIds
            if idsDict.ContainsKey node
            then Some(idsDict.[node])
            else None

        let getId (provider: NodeIdProvider) (node: ITreeNode) =
            match tryGetId provider node with
            | Some(id) -> id, provider
            | None ->
                do provider.GeneratedIds.Add (node, provider.NextId)
                provider.NextId, {provider with NextId = provider.NextId + 1 }

    type private BuildState = {   
        GraphInfo: DDGraphBuildInfo
        Provider: NodeIdProvider
        LoopNodesStack: list<int> }

    let rec buildForVar (varRef: IReferenceExpression) (cfgInfo: CSharpCFGInfo) = 
        let addNodeUsingFun treeNode label addFunc (state: BuildState) =
            let nodeId, provider' = NodeIdProviderFuncs.getId state.Provider treeNode
            let nodeInfo = { Label = label; AstElem = treeNode }
            let graph' = addFunc state.GraphInfo nodeId (InnerNode(nodeInfo))
            nodeId, {state with GraphInfo = graph'; Provider = provider'}

        let addNode treeNode label (state: BuildState) =
            let addFun = DDGBuildFuncs.addNodeAndEdge
            addNodeUsingFun treeNode label addFun state

        let addMarkedNode treeNode label (state: BuildState) =
            let addFun = DDGBuildFuncs.addMarkedNodeAndEdge
            addNodeUsingFun treeNode label addFun state

        let setGraphConnectionNode nodeId (state: BuildState) =
            let graph' = { state.GraphInfo with ConnectionNode = nodeId }
            { state with GraphInfo = graph' }

        let addNodeAsConnectionNode treeNode label (state: BuildState) =
            let addedNode, state' = addNode treeNode label state
            setGraphConnectionNode addedNode state'

        let getCorespondingCfgNode (treeNode: ITreeNode) =
            let cfgNodes = cfgInfo.AstCfgMap.[treeNode.GetHashCode()]
            if cfgNodes.Count > 1
            then failwith multipleCfgNodesForAstNodeMsg
            else cfgNodes |> List.ofSeq |> List.head

        let rec build (cfe: IControlFlowElement) (varName: string) (state: BuildState) =
            let processExpr (expr: ICSharpExpression) (state: BuildState) =
                match expr with
                | :? ICSharpLiteralExpression as literalExpr ->
                    let literalVal = literalExpr.Literal.GetText().Trim[|'\"'|]
                    let label = "literal(" + literalVal + ")"
                    addMarkedNode literalExpr label state |> snd
                // not implemented (but must precede IReferenceExpression case)
                | :? IInvocationExpression -> state
                | :? IReferenceExpression as refExpr ->
                    let curVarName = refExpr.NameIdentifier.Name
                    let label = "refExpr(" + curVarName + ")"
                    let addedNode, state' = addNode refExpr label state
                    let exprCfgNode = getCorespondingCfgNode refExpr |> fun w -> w.Value
                    let curConnectionNode = state'.GraphInfo.ConnectionNode
                    let state' = setGraphConnectionNode addedNode state'
                    let state' = build exprCfgNode curVarName state'
                    setGraphConnectionNode curConnectionNode state'
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
                let state' = addNodeAsConnectionNode assignExpr label state
                operands |> List.fold (fun st op -> processExpr op st) state'

            let processInitializer (initializer: ITreeNode) (state: BuildState) = 
                match initializer with
                | :? ICSharpExpression as expr ->
                    processExpr expr state
                | :? IExpressionInitializer as exprInitializer ->
                    processExpr exprInitializer.Value state
                | _ -> failwith ("not implemented case in processInitializer: " + initializer.NodeType.ToString())

            let processLocVarDecl (locVarDecl: ILocalVariableDeclaration) (state: BuildState) =
                let label = "varDecl(" + varName + ")"
                let state' = addNodeAsConnectionNode locVarDecl label state
                processInitializer locVarDecl.Initializer state'

            let processEntries (entries: list<IControlFlowRib>) (state: BuildState) =
                let curConnectionNode = state.GraphInfo.ConnectionNode
                entries
                |> List.choose (fun rib -> if rib.Source <> null then Some(rib.Source) else None) 
                |> List.fold 
                    (
                        fun accState src -> 
                            build src varName accState 
                            |> setGraphConnectionNode curConnectionNode
                    ) 
                    state

            let (|LoopNode|_|) (cfe: IControlFlowElement) =
                match Map.tryFind cfe.Id cfgInfo.LoopNodes with
                | Some(lnInfo) as i -> i
                | _ -> None

            let loopNodeAlreadyMet (cfe: IControlFlowElement) (state: BuildState) =
                let stack = state.LoopNodesStack
                not stack.IsEmpty && stack.Head = cfe.Id

            let (|SameNameAssignExpr|_|) (node: ITreeNode) =
                match node with
                | :? IAssignmentExpression as assignExpr 
                    when 
                        match assignExpr.Dest with 
                        | :? IReferenceExpression as dst 
                            when dst.NameIdentifier.Name = varName -> true
                        | _ -> false
                    -> Some(assignExpr)
                | _ -> None

            let processLoopNode (cfe: IControlFlowElement) (info: LoopNodeInfo) (state: BuildState) =
                if loopNodeAlreadyMet cfe state
                then
                    addNodeAsConnectionNode cfe.SourceElement "loopNode" state
                else
                    let bodyExitNode = cfe.Entries.[info.BodyExitEdgeIndex]
                    let enterNode = cfe.Entries.[info.EnterEdgeIndex]
                    addNodeAsConnectionNode cfe.SourceElement "loopNode" state
                    |> fun st -> { st with LoopNodesStack = cfe.Id :: st.LoopNodesStack }
                    |> processEntries [bodyExitNode] 
                    |> fun st -> { st with LoopNodesStack = st.LoopNodesStack.Tail }
                    |> processEntries [enterNode]

            match cfe with
            | LoopNode(info) -> processLoopNode cfe info state
            | _ ->
                let astNode = cfe.SourceElement
                match astNode with
                | SameNameAssignExpr(assignExpr) -> 
                    processAssignment assignExpr state
                | :? ILocalVariableDeclaration as locVarDecl
                    when locVarDecl.NameIdentifier.Name = varName 
                    -> 
                    processLocVarDecl locVarDecl state 
                | _ ->
                    processEntries (List.ofSeq cfe.Entries) state

        let provider = NodeIdProviderFuncs.create
        let finalNodeId, provider = NodeIdProviderFuncs.getId provider varRef
        let varName = varRef.NameIdentifier.Name
        let finalNodeInfo = { Label = "varRef(" + varName + ")"; AstElem = varRef }
        let graphInfo = DDGBuildFuncs.create(finalNodeId, InnerNode(finalNodeInfo))
        let cfgNode = getCorespondingCfgNode varRef |> fun w -> w.Value
        let initState = { GraphInfo = graphInfo; Provider = provider; LoopNodesStack = [] }
        let resState = build cfgNode varName initState
        let root = RootNode
        let rootId = resState.Provider.NextId
        let graphInfo' = DDGBuildFuncs.foldMarkedOnNew resState.GraphInfo rootId root
        let ddGraph: DDGraph = { 
            Graph = graphInfo'.Graph
            NodeIdInfoDict = graphInfo'.NodeIdInfoDict
            RootId = rootId }
        ddGraph

    let toDot (ddGraph: DDGraph) name path =
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