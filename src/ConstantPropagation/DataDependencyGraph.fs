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
    NodeIdInfoDict: Dictionary<int, DDNode> }

module DDGraphFuncs =
    open JetBrains.ReSharper.Psi

    // exception messages 
    let private multipleCfgNodesForAstNodeMsg = 
        "ast node maps to multiple cfg nodes where single mapping expected"

    let private forNodeIncorrectMappingMsg =
        "_for_ node mapping is incorrect, it must be mapped to exactly 3 cfg nodes"

    let private bodyForNodeManyInputsMsg =
        "body for node has more than one input"

    let private unexpectedInitializerTypeMsg =
        "unexpected initializer type in local variable declaration"

    let private badIRefExprCastMsg = 
        "unable to perform cast to IReferenceExpression"


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

        let private addNodeAndEdge (ddGraph: DDGraphBuildInfo) srcNodeId srcNodeInfo =
            let ddg = addNode ddGraph srcNodeId srcNodeInfo
            addEdge ddg srcNodeId ddg.ConnectionNode

        let addConnectionNodeAndEdge (ddGraph: DDGraphBuildInfo) srcNodeId srcNodeInfo =
            let ddg' = addNodeAndEdge ddGraph srcNodeId srcNodeInfo
            { ddg' with ConnectionNode = srcNodeId }

        let addMarkedNodeAndEdge (ddg: DDGraphBuildInfo) srcNodeId srcNodeInfo =
            let ddg' = addNodeAndEdge ddg srcNodeId srcNodeInfo
            { ddg' with MarkedNodes = srcNodeId :: ddg'.MarkedNodes }

        let addEdgeFromExistingNode (ddg: DDGraphBuildInfo) existingNodeId =
            addEdge ddg existingNodeId ddg.ConnectionNode

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
        VisitedForks: Map<int, Set<string>>
        NodesToVisit: Set<int> } 

    let rec buildForVar (varRef: IReferenceExpression) (cfgInfo: CSharpCFGInfo) = 
        let addNodeUsingFun treeNode label addFunc (state: BuildState) =
            let nodeId, provider' = NodeIdProviderFuncs.getId state.Provider treeNode
            let nodeInfo = { Label = label; AstElem = treeNode }
            let graph' = addFunc state.GraphInfo nodeId (InnerNode(nodeInfo))
            { state with GraphInfo = graph'; Provider = provider' }

        let addNodeAsConnectionNode treeNode label (state: BuildState) =
            let addFun = DDGBuildFuncs.addConnectionNodeAndEdge
            addNodeUsingFun treeNode label addFun state

        let setGraphConnectionNode nodeId (state: BuildState) =
            let graph' = { state.GraphInfo with ConnectionNode = nodeId }
            { state with GraphInfo = graph' }

        let getCorespondingCfgNode (treeNode: ITreeNode) =
            let cfgNodes = cfgInfo.AstCfgMap.[treeNode.GetHashCode()]
            if cfgNodes.Count > 1
            then failwith multipleCfgNodesForAstNodeMsg
            else cfgNodes |> List.ofSeq |> List.head

        let rec build (cfe: IControlFlowElement) (varsSet: Set<string>) (state: BuildState) =
            let (|TrackedVarAssignExpr|_|) (node: ITreeNode) =
                match node with
                | :? IAssignmentExpression as assignExpr 
                    when 
                        match assignExpr.Dest with 
                        | :? IReferenceExpression as dst 
                            when Set.contains dst.NameIdentifier.Name varsSet 
                            -> true
                        | _ -> false
                    -> Some(assignExpr)
                | _ -> None

            let (|LoopNode|_|) (cfe: IControlFlowElement) =
                match Map.tryFind cfe.Id cfgInfo.LoopNodes with
                | Some(lnInfo) as i -> i
                | _ -> None

            let castToIRefExpr (n: ITreeNode) =
                if n :? IReferenceExpression
                then n :?> IReferenceExpression
                else failwith badIRefExprCastMsg

            let addNewVars (nodes: list<'a>) chooser varsSet =
                nodes
                |> List.choose chooser
                |> List.fold (fun accSet var -> Set.add var accSet) varsSet

            let addNewVarsFromExprs (args: list<ICSharpExpression>) varsSet =
                let f (a: ICSharpExpression) =
                    match a with
                    | :? IReferenceExpression as re -> (Some(re.NameIdentifier.Name))
                    | _ -> None
                addNewVars args f varsSet

            let addNewVarsFromArgs (args: list<ICSharpArgument>) varsSet =
                let f (a: ICSharpArgument) =
                    match a with
                    | :? IReferenceExpression as re -> (Some(re.NameIdentifier.Name))
                    | _ -> None
                addNewVars args f varsSet

            let addNodesToVisit nodes nodesToVisit =
                nodes
                |> List.map (fun op -> (getCorespondingCfgNode op).Value.Id)
                |> List.fold (fun accSet id -> Set.add id accSet ) nodesToVisit

            let isUpdateMethod (name: string) (callTargetType: IType) =
                name = "Replace" && callTargetType.IsString()

            let processEntries (entries: list<IControlFlowRib>) (varsSet: Set<string>) (state: BuildState) =
                let curConnectionNode = state.GraphInfo.ConnectionNode
                entries
                |> List.choose (fun rib -> if rib.Source <> null then Some(rib.Source) else None) 
                |> List.fold 
                    (
                        fun accState src -> 
                            build src varsSet accState 
                            |> setGraphConnectionNode curConnectionNode
                    ) 
                    state 

            let entries, updState, updVarsSet =
                match cfe with
                | LoopNode(info) ->
                    let state' = addNodeAsConnectionNode cfe.SourceElement "loopNode" state
                    let forkVarsSet = 
                        match Map.tryFind cfe.Id state.VisitedForks with
                        | None -> Set.empty
                        | Some(vs) -> vs
                    let newVarsSet = Set.difference varsSet forkVarsSet
                    if Set.isEmpty newVarsSet
                    then
                        let enterEdge = cfe.Entries.[info.EnterEdgeIndex]
                        [enterEdge], state', varsSet
                    else 
                        let visitedForks' = Map.add cfe.Id varsSet state.VisitedForks
                        let bodyExitEdge = cfe.Entries.[info.BodyExitEdgeIndex]
                        [bodyExitEdge], { state' with VisitedForks = visitedForks' }, varsSet
                | _ ->
                    let astNode = cfe.SourceElement
                    match astNode with
                    | TrackedVarAssignExpr(assignExpr) -> 
                        let assingnType, operands = 
                            if assignExpr.AssignmentType = AssignmentType.EQ
                            then 
                                let ops = assignExpr.OperatorOperands |> List.ofSeq |> List.tail
                                "assignment", ops
                            else 
                                let ops = assignExpr.OperatorOperands |> List.ofSeq
                                "plusAssignment", ops
                        let state' = 
                            let varName = (assignExpr.Dest :?> IReferenceExpression).NameIdentifier.Name
                            let label = assingnType + "(" + varName + ")"
                            addNodeAsConnectionNode assignExpr label state
                        let varsSet' = addNewVarsFromExprs operands varsSet
                        let nodesToVisit = addNodesToVisit operands state'.NodesToVisit
                        let entries = cfe.Entries |> List.ofSeq
                        entries, { state' with NodesToVisit = nodesToVisit }, varsSet'
                    | :? ILocalVariableDeclaration as locVarDecl
                        when Set.contains locVarDecl.NameIdentifier.Name varsSet
                        -> 
                        let state' = 
                            let label = "varDecl(" + locVarDecl.NameIdentifier.Name + ")"
                            addNodeAsConnectionNode locVarDecl label state
                        let refExprsToVisit' = 
                            match locVarDecl.Initializer with
                            | :? IExpressionInitializer as re ->
                                let initializerId = (getCorespondingCfgNode re.Value).Value.Id
                                Set.add initializerId state'.NodesToVisit
                            | _ -> failwith unexpectedInitializerTypeMsg
                        let entries = cfe.Entries |> List.ofSeq
                        entries, { state' with NodesToVisit = refExprsToVisit' }, varsSet
                    | :? ICSharpLiteralExpression as literalExpr 
                        when Set.contains cfe.Id state.NodesToVisit 
                        ->
                        let label = 
                            let literalVal = literalExpr.Literal.GetText().Trim[|'\"'|]
                            "literal(" + literalVal + ")"
                        let state' = addNodeAsConnectionNode literalExpr label state
                        let entries = cfe.Entries |> List.ofSeq
                        entries, state', varsSet
                    | :? IInvocationExpression as invocExpr
                        when Set.contains cfe.Id state.NodesToVisit 
                        ->
                        let invokedExpr = castToIRefExpr invocExpr.InvokedExpression
                        let methodName = invokedExpr.NameIdentifier.Name
                        let callTargetRefExpr = castToIRefExpr invokedExpr.QualifierExpression
                        let state' = 
                            let callTarget = callTargetRefExpr.NameIdentifier.Name
                            let label = "methodCall(target=" + callTarget + " name=" + methodName + ")"
                            addNodeAsConnectionNode invocExpr label state
                        let args = 
                            invocExpr.Arguments 
                            |> List.ofSeq
                            |> List.map (fun a -> a.Value)
                        let varsSet' = 
                            let vs = addNewVarsFromExprs args varsSet
                            if isUpdateMethod methodName (callTargetRefExpr.Type())
                            then
                                addNewVarsFromExprs [callTargetRefExpr] vs
                            else
                                vs 
                        let nodesToVisit = addNodesToVisit args state.NodesToVisit
                        let entries = cfe.Entries |> List.ofSeq
                        entries, { state' with NodesToVisit = nodesToVisit }, varsSet'
                    // todo: consider remove this, it is never met in tests
//                    | :? ICSharpArgument as arg ->
//                        let varsSet' = addNewVarsFromExprs [arg.Value] varsSet
//                        let nodesToVisit = addNodesToVisit [arg.Value] state.NodesToVisit
//                        let entries = cfe.Entries |> List.ofSeq
//                        entries, { state with NodesToVisit = nodesToVisit }, varsSet'
                    | :? IReferenceExpression as refExpr 
                        when Set.contains cfe.Id state.NodesToVisit 
                        ->
                        let label = 
                            let curVarName = refExpr.NameIdentifier.Name
                            "refExpr(" + curVarName + ")"
                        let state' = addNodeAsConnectionNode refExpr label state
                        let entries = cfe.Entries |> List.ofSeq
                        entries, state', varsSet
                    | :? IAdditiveExpression as addExpr 
                        when Set.contains cfe.Id state.NodesToVisit  
                        ->
                        let state' = addNodeAsConnectionNode addExpr "concat" state
                        let operands = addExpr.OperatorOperands |> List.ofSeq
                        let varsSet' = addNewVarsFromExprs operands varsSet
                        let nodesToVisit = addNodesToVisit operands state'.NodesToVisit
                        let entries = cfe.Entries |> List.ofSeq
                        entries, { state' with NodesToVisit = nodesToVisit }, varsSet'
                    // todo: replace with failwith when all cases will be covered
                    | _ -> cfe.Entries |> List.ofSeq, state, varsSet

            processEntries entries updVarsSet updState 

        let provider = NodeIdProviderFuncs.create
        let finalNodeId, provider = NodeIdProviderFuncs.getId provider varRef
        let varName = varRef.NameIdentifier.Name
        let finalNodeInfo = { Label = "varRef(" + varName + ")"; AstElem = varRef }
        let graphInfo = DDGBuildFuncs.create(finalNodeId, InnerNode(finalNodeInfo))
        let varsSet = Set.ofList [varName]
        let cfgNode = getCorespondingCfgNode varRef |> fun w -> w.Value
        let initState = { 
            GraphInfo = graphInfo; 
            Provider = provider;
            VisitedForks = Map.empty;
            NodesToVisit = Set.empty }
        let resState = build cfgNode varsSet initState
        let ddGraph: DDGraph = { 
            Graph = resState.GraphInfo.Graph
            NodeIdInfoDict = resState.GraphInfo.NodeIdInfoDict }
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