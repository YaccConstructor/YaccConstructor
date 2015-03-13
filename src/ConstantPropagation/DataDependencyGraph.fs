module DataDependencyGraph

open QuickGraph

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.CSharp.Tree

open Utils
open Utils.StateMonad

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
    type DDGraphBuildInfo = {   
        Graph: AdjacencyGraph<int, Edge<int>>
        NodeIdInfoDict: Dictionary<int, DDNode>
        ConnectionNode: int
        FinalNodeId: int
        PrerootNodes: list<int>
        RootId: option<int> }

    module DDGBuildFuncs =
        // exception messages
        let private dstNodeIsNotSetMsg = 
            "cannot create edge - dst node is not set"
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
                PrerootNodes = []
                RootId = None }
            addNode ddGraph finalNodeId finalNodeInfo

        let addNodeAndEdge (ddGraph: DDGraphBuildInfo) srcNodeId srcNodeInfo =
            let ddg = addNode ddGraph srcNodeId srcNodeInfo
            addEdge ddg srcNodeId ddg.ConnectionNode

        let addPrerootAndEdge (ddg: DDGraphBuildInfo) srcNodeId srcNodeInfo =
            let ddg' = addNodeAndEdge ddg srcNodeId srcNodeInfo
            { ddg' with PrerootNodes = srcNodeId :: ddg'.PrerootNodes }

        let foldPreroots (ddg: DDGraphBuildInfo) rootId rootInfo =
            let ddg' = addNode ddg rootId rootInfo
            ddg'.PrerootNodes
            |> List.fold (fun accDdg id -> addEdge accDdg rootId id) ddg'
            |> fun ddg -> { ddg with PrerootNodes = [] }

        let merge (dstGraph: DDGraphBuildInfo) (srcGraph: DDGraphBuildInfo) =
            srcGraph.NodeIdInfoDict 
            |> List.ofSeq
            |> List.iter (fun entry -> dstGraph.NodeIdInfoDict.Add (entry.Key, entry.Value))
            dstGraph.Graph.AddEdgeRange srcGraph.Graph.Edges |> ignore
            addEdge dstGraph srcGraph.FinalNodeId dstGraph.ConnectionNode

    type NodeIdProvider = {   
        NextId: int
        GeneratedIds: Dictionary<ITreeNode, int> } 

    module NodeIdProviderFuncs =
        let create = { NextId = 0; GeneratedIds = new Dictionary<ITreeNode, int>() }

        let getId (provider: NodeIdProvider) (node: ITreeNode) =
            let idsDict = provider.GeneratedIds
            if idsDict.ContainsKey node
            then idsDict.[node], provider
            else
                do idsDict.Add (node, provider.NextId)
                provider.NextId, {provider with NextId = provider.NextId + 1 }

    type BuildState = {   
        GraphInfo: DDGraphBuildInfo
        Provider: NodeIdProvider }

    let rec buildForVar (varRef: IReferenceExpression) (astCfgMap: Dictionary<ITreeNode, IControlFlowElement>) = 
        let addNodeUsingFun treeNode label addFunc (state: BuildState) =
            let nodeId, provider' = NodeIdProviderFuncs.getId state.Provider treeNode
            let nodeInfo = { Label = label; AstElem = treeNode }
            let graph' = addFunc state.GraphInfo nodeId (InnerNode(nodeInfo))
            nodeId, {state with GraphInfo = graph'; Provider = provider'}

        let addNode treeNode label (state: BuildState) =
            let addFun = DDGBuildFuncs.addNodeAndEdge
            addNodeUsingFun treeNode label addFun state

        let addPreroot treeNode label (state: BuildState) =
            let addFun = DDGBuildFuncs.addPrerootAndEdge
            addNodeUsingFun treeNode label addFun state

        let setGraphConnectionNode nodeId (state: BuildState) =
            let graph' = { state.GraphInfo with ConnectionNode = nodeId }
            { state with GraphInfo = graph' }

        let addNodeAsConnectionNode treeNode label (state: BuildState) =
            let addedNode, state' = addNode treeNode label state
            setGraphConnectionNode addedNode state'

        let rec build (cfgNode: IControlFlowElement) (varName: string) (state: BuildState) =
            let processExpr (expr: ICSharpExpression) (state: BuildState) =
                match expr with
                | :? ICSharpLiteralExpression as literalExpr ->
                    let literalVal = literalExpr.Literal.GetText().Trim[|'\"'|]
                    let label = "literal(" + literalVal + ")"
                    snd <| addPreroot literalExpr label state
                // not implemented (but must precede IReferenceExpression case)
                | :? IInvocationExpression -> state
                | :? IReferenceExpression as refExpr ->
                    let curVarName = refExpr.NameIdentifier.Name
                    let label = "refExpr(" + curVarName + ")"
                    let addedNode, state' = addNode refExpr label state
                    if curVarName = varName
                    then 
                        let curCfgNode = astCfgMap.[refExpr]
                        let curConnectionNode = state'.GraphInfo.ConnectionNode
                        let state' = setGraphConnectionNode addedNode state'
                        let state' = build curCfgNode varName state'
                        setGraphConnectionNode curConnectionNode state'
                    else 
                        // let otherVarDdg = buildForVar refExpr astCfgMap
                        failwith ""
                    
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

            let processEntries (cfgNode: IControlFlowElement) (state: BuildState) =
                let curConnectionNode = state.GraphInfo.ConnectionNode
                cfgNode.Entries
                |> List.ofSeq
                |> List.choose (fun rib -> if rib.Source <> null then Some(rib.Source) else None) 
                |> List.fold 
                    (
                        fun accState src -> 
                            build src varName accState 
                            |> setGraphConnectionNode curConnectionNode
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

        let provider = NodeIdProviderFuncs.create
        let finalNodeId, provider = NodeIdProviderFuncs.getId provider varRef
        let varName = varRef.NameIdentifier.Name
        let finalNodeInfo = { Label = "varRef(" + varName + ")"; AstElem = varRef }
        let graphInfo = DDGBuildFuncs.create(finalNodeId, InnerNode(finalNodeInfo))
        let cfgNode = astCfgMap.[varRef]
        let initState = { GraphInfo = graphInfo; Provider = provider }
        let resState = build cfgNode varName initState
        let root = RootNode
        let rootId = resState.Provider.NextId
        let graphInfo' = DDGBuildFuncs.foldPreroots resState.GraphInfo rootId root
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