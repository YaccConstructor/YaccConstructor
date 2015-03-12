module DataDependencyGraph

open QuickGraph

open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree

open System.Collections.Generic
open System.IO

type NodeInfo = 
    { Label: string
      AstElem: ITreeNode }

type DDNode =
| RootNode
| InnerNode of NodeInfo

type DDGraph(finalNodeId: int, finalNodeInfo: DDNode) =
    let graph = new AdjacencyGraph<int, Edge<int>>()
    let nodeIdInfoDict = new Dictionary<int, DDNode>()
    let mutable currentConnectionNode = finalNodeId

    // exception messages
    let dstNodeIsNotSetMsg = "cannot create edge - dst node is not set"
    let noSuchNodeMsg = "cannot create edge - src or dst node is not added to graph"

    let addNode id info = 
        if nodeIdInfoDict.ContainsKey id
        then 
            nodeIdInfoDict.[id] <- info
        else
            nodeIdInfoDict.Add (id, info)
            graph.AddVertex id |> ignore

    do
        addNode finalNodeId finalNodeInfo

    let failIfNotExists id =
        if (not << nodeIdInfoDict.ContainsKey) id
        then failwith noSuchNodeMsg

    member private this.Graph with get() = graph
    member private this.NodeIdInfoDict with get() = nodeIdInfoDict
    member private this.FinalNodeId with get() = finalNodeId

    member private this.AddEdge src dst =
        failIfNotExists src
        failIfNotExists dst
        if (not << (graph.ContainsEdge : int * int -> bool)) (src, dst)
        then graph.AddEdge (new Edge<int>(src, dst)) |> ignore

    member private this.AddEdgeFrom srcNodeId =  
        this.AddEdge srcNodeId currentConnectionNode

    member this.CurrentConnectionNode 
        with get() = currentConnectionNode
        and set(id) = currentConnectionNode <- id

    member this.AddEdgeFrom (srcNodeId, srcNodeInfo) =
        addNode srcNodeId srcNodeInfo
        this.AddEdgeFrom srcNodeId

    member this.Merge (other: DDGraph) =
        other.NodeIdInfoDict 
        |> List.ofSeq
        |> List.iter (fun entry -> nodeIdInfoDict.Add (entry.Key, entry.Value))
        graph.AddEdgeRange other.Graph.Edges |> ignore
        this.AddEdgeFrom other.FinalNodeId

    member this.ToDot name path =
        use file = FileInfo(path).CreateText()
        file.WriteLine("digraph " + name + " {")
        this.NodeIdInfoDict
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
        this.Graph.Edges
        |> List.ofSeq
        |> List.map
            (
                fun edge -> edge.Source.ToString() + " -> " + edge.Target.ToString()
            )
        |> List.iter file.WriteLine
        file.WriteLine("}")

module NodeIdProvider = 
    type NodeIdProvider = 
        { NextId: int
          GeneratedIds: Dictionary<ITreeNode, int> }

    let create = {NextId = 0; GeneratedIds = new Dictionary<ITreeNode, int>()}

    let getId (provider: NodeIdProvider) (node: ITreeNode) =
        let idsDict = provider.GeneratedIds
        if idsDict.ContainsKey node
        then idsDict.[node], provider
        else
            do idsDict.Add (node, provider.NextId)
            provider.NextId, {provider with NextId = provider.NextId + 1 }

module DDGraphFuncs =
    open NodeIdProvider

    let buildForVar (varRef: IReferenceExpression) (astCfgMap: Dictionary<ITreeNode, IControlFlowElement>) =
        let rec build (cfgNode: IControlFlowElement) (varName: string) 
                      (graph: DDGraph) (provider: NodeIdProvider) =
            let updateGraph treeNode textInfo (provider: NodeIdProvider) =
                let nodeId, provider = NodeIdProvider.getId provider treeNode
                let nodeInfo = {
                    Label = textInfo
                    AstElem = treeNode
                }
                graph.AddEdgeFrom (nodeId, InnerNode(nodeInfo))
                nodeId, provider

            let processExpr (expr: ICSharpExpression) (provider: NodeIdProvider) =
                match expr with
                | :? ICSharpLiteralExpression as literalExpr ->
                    let literalVal = literalExpr.Literal.GetText().Trim[|'\"'|]
                    let _, provider =
                        updateGraph literalExpr ("literal(" + literalVal + ")") provider
                    provider
                // not implemented (but must precede IReferenceExpression case)
                | :? IInvocationExpression -> provider
                | :? IReferenceExpression as refExpr ->
                    let curVarName = refExpr.NameIdentifier.Name
                    let addedNodeId, provider = 
                        updateGraph refExpr ("refExpr(" + curVarName + ")") provider
                    if curVarName = varName
                    then 
                        let curConnectionNode = graph.CurrentConnectionNode
                        graph.CurrentConnectionNode <- addedNodeId
                        let curCfgNode = astCfgMap.[refExpr]
                        let provider = build curCfgNode varName graph provider
                        graph.CurrentConnectionNode <- curConnectionNode
                        provider
                    else 
                        failwith ("not implemented new ref case in processExpr")
                | _ -> failwith ("not implemented case in processExpr: " + expr.NodeType.ToString())

            let processAssignment (assignExpr: IAssignmentExpression) (provider: NodeIdProvider) = 
                let assingText, operands = 
                    if assignExpr.AssignmentType = AssignmentType.EQ
                    then 
                        "assignment", assignExpr.OperatorOperands |> List.ofSeq |> List.tail
                    else 
                        "plusAssignment", assignExpr.OperatorOperands |> List.ofSeq
                let addedNodeId, provider = 
                    updateGraph assignExpr (assingText + "(" + varName + ")") provider
                graph.CurrentConnectionNode <- addedNodeId
                operands |> List.fold (fun provider op -> processExpr op provider) provider

            let processInitializer (initializer: ITreeNode) (provider: NodeIdProvider) = 
                match initializer with
                | :? ICSharpExpression as expr ->
                    processExpr expr provider
                | :? IExpressionInitializer as exprInitializer ->
                    processExpr exprInitializer.Value provider
                | _ -> failwith ("not implemented case in processInitializer: " + initializer.NodeType.ToString())

            let processLocVarDecl (locVarDecl: ILocalVariableDeclaration) (provider: NodeIdProvider) =
                let addedNodeId, provider = 
                    updateGraph locVarDecl ("varDecl(" + varName + ")") provider
                graph.CurrentConnectionNode <- addedNodeId
                processInitializer locVarDecl.Initializer provider

            let processEntries (cfgNode: IControlFlowElement) (provider: NodeIdProvider): NodeIdProvider =
                let curConnectionNode = graph.CurrentConnectionNode
                cfgNode.Entries
                |> List.ofSeq
                |> List.choose (fun rib -> if rib.Source <> null then Some(rib.Source) else None) 
                |> List.fold 
                    (
                        fun provider src ->
                            graph.CurrentConnectionNode <- curConnectionNode
                            build src varName graph provider
                    ) 
                    provider

            let astNode = cfgNode.SourceElement
            match astNode with
            | :? IAssignmentExpression as assignExpr 
                when 
                    match assignExpr.Dest with 
                    | :? IReferenceExpression as dst 
                        when dst.NameIdentifier.Name = varName -> true
                    | _ -> false
                -> 
                    processAssignment assignExpr provider
            | :? ILocalVariableDeclaration as locVarDecl
                when locVarDecl.NameIdentifier.Name = varName 
                -> 
                    processLocVarDecl locVarDecl provider
            | _ ->
                processEntries cfgNode provider
        
        let provider = NodeIdProvider.create
        let finalNodeId, provider = NodeIdProvider.getId provider varRef
        let varName = varRef.NameIdentifier.Name
        let finalNodeInfo = {
            Label = "varRef(" + varName + ")"
            AstElem = varRef
        }
        let graph = DDGraph(finalNodeId, InnerNode(finalNodeInfo))
        let cfgNode = astCfgMap.[varRef]
        build cfgNode varName graph provider |> ignore
        graph