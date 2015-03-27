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

type DDGraph = {
    Graph: AdjacencyGraph<int, Edge<int>>
    NodeIdInfoDict: Dictionary<int, NodeInfo> }

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
        NodeIdInfoDict: Dictionary<int, NodeInfo>
        ConnectionNode: int }

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

        let create(finalNodeId: int, finalNodeInfo: NodeInfo) =
            let ddGraph = {
                Graph = new AdjacencyGraph<int, Edge<int>>()
                NodeIdInfoDict = new Dictionary<int, NodeInfo>()
                ConnectionNode = finalNodeId }
            addNode ddGraph finalNodeId finalNodeInfo

        let private addNodeAndEdge (ddGraph: DDGraphBuildInfo) srcNodeId srcNodeInfo =
            let ddg = addNode ddGraph srcNodeId srcNodeInfo
            addEdge ddg srcNodeId ddg.ConnectionNode

        let addConnectionNodeAndEdge (ddGraph: DDGraphBuildInfo) srcNodeId srcNodeInfo =
            let ddg' = addNodeAndEdge ddGraph srcNodeId srcNodeInfo
            { ddg' with ConnectionNode = srcNodeId }

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
        let addNodeAsConnectionNode treeNode label (state: BuildState) =
            let nodeId, provider' = NodeIdProviderFuncs.getId state.Provider treeNode
            let nodeInfo = { Label = label; AstElem = treeNode }
            let graph' = DDGBuildFuncs.addConnectionNodeAndEdge state.GraphInfo nodeId nodeInfo
            { state with GraphInfo = graph'; Provider = provider' }

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

            let processNode astNode label newVars newNodes trackedVars state  =
                let state' = addNodeAsConnectionNode astNode label state
                let trackedVars' = addNewVarsFromExprs newVars trackedVars
                let nodesToVisit' = addNodesToVisit newNodes state'.NodesToVisit
                trackedVars', { state' with NodesToVisit = nodesToVisit' }

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
                    let optData = 
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
                            let varName = (assignExpr.Dest :?> IReferenceExpression).NameIdentifier.Name
                            let label = assingnType + "(" + varName + ")"
                            Some(label, operands, operands)
                        | :? ILocalVariableDeclaration as locVarDecl
                            when Set.contains locVarDecl.NameIdentifier.Name varsSet
                            ->
                            let label = "varDecl(" + locVarDecl.NameIdentifier.Name + ")"
                            let initializerExprAsList = 
                                match locVarDecl.Initializer with
                                | :? IExpressionInitializer as re -> [re.Value]
                                | _ -> failwith unexpectedInitializerTypeMsg
                            Some(label, initializerExprAsList, initializerExprAsList)
                        | :? ICSharpLiteralExpression as literalExpr 
                            when Set.contains cfe.Id state.NodesToVisit 
                            ->
                            let literalVal = literalExpr.Literal.GetText().Trim[|'\"'|]
                            let label = "literal(" + literalVal + ")"
                            Some(label, [], [])
                        | :? IInvocationExpression as invocExpr
                            when Set.contains cfe.Id state.NodesToVisit 
                            ->
                            let invokedExpr = castToIRefExpr invocExpr.InvokedExpression
                            let methodName = invokedExpr.NameIdentifier.Name
                            let callTargetRefExpr = castToIRefExpr invokedExpr.QualifierExpression
                            let callTarget = callTargetRefExpr.NameIdentifier.Name
                            let label = "methodCall(target=" + callTarget + " name=" + methodName + ")"
                            let args = 
                                invocExpr.Arguments 
                                |> List.ofSeq
                                |> List.map (fun a -> a.Value)
                            let dependencies =
                                if isUpdateMethod methodName (callTargetRefExpr.Type())
                                then
                                    callTargetRefExpr :> ICSharpExpression :: args
                                else
                                    args
                            Some(label, dependencies, dependencies)
                        | :? IReferenceExpression as refExpr 
                            when Set.contains cfe.Id state.NodesToVisit 
                            ->
                            let curVarName = refExpr.NameIdentifier.Name
                            let label = "refExpr(" + curVarName + ")"
                            Some(label, [], [])
                        | :? IAdditiveExpression as addExpr 
                            when Set.contains cfe.Id state.NodesToVisit  
                            ->
                            let operands = addExpr.OperatorOperands |> List.ofSeq
                            Some("concat", operands, operands)
                        | _ -> None
                    let trackedVars', state' =
                        match optData with
                        | Some(label, newVars, newNodes) ->
                            processNode astNode label newVars newNodes varsSet state
                        | None -> varsSet, state
                    let entries = cfe.Entries |> List.ofSeq
                    entries, state', trackedVars'
            processEntries entries updVarsSet updState 

        let provider = NodeIdProviderFuncs.create
        let finalNodeId, provider = NodeIdProviderFuncs.getId provider varRef
        let varName = varRef.NameIdentifier.Name
        let finalNodeInfo = { Label = "varRef(" + varName + ")"; AstElem = varRef }
        let graphInfo = DDGBuildFuncs.create(finalNodeId, finalNodeInfo)
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
                    let text = kvp.Value.Label
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