module DataDependencyGraph

open QuickGraph

open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree

open System.Collections.Generic
open System.IO

type NodeInfo = {
    Info: string
    AstElem: ITreeNode
}

type DDGraph(finalNodeId: int, finalNodeInfo: NodeInfo) = 
    let graph = new AdjacencyGraph<int, Edge<int>>()
    let nodeInfoDict = new Dictionary<int, NodeInfo>()
    let mutable curConnectionNodeId = finalNodeId

    // exception messages
    let dstNodeIsNotSetMsg = "cannot create edge - dst node is not set"
    let noSuchNodeMsg = "cannot create edge - src or dst node is not added to graph"

    let addNode id info = 
        if nodeInfoDict.ContainsKey id
        then 
            nodeInfoDict.[id] <- info
        else
            nodeInfoDict.Add (id, info)
            graph.AddVertex id |> ignore

    do
        addNode finalNodeId finalNodeInfo

    let failIfNotExists id =
        if (not << nodeInfoDict.ContainsKey) id
        then failwith noSuchNodeMsg

    member this.AddNode = addNode

    member this.AddEdge src dst =
        failIfNotExists src
        failIfNotExists dst
        if (not << (graph.ContainsEdge : int * int -> bool)) (src, dst)
        then graph.AddEdge (new Edge<int>(src, dst)) |> ignore

    member this.SetConnectionNode id = curConnectionNodeId <- id

    member this.CurrentConnectionNode = curConnectionNodeId

    member this.AddEdgeFrom srcNodeId = 
        failIfNotExists srcNodeId 
        this.AddEdge srcNodeId curConnectionNodeId |> ignore

    member this.AddEdgeFrom (srcNodeId, srcNodeInfo) =
        this.AddNode srcNodeId srcNodeInfo
        this.AddEdgeFrom srcNodeId

    member this.Edges = graph.Edges

    member this.GetNodes() = 
        nodeInfoDict 
        |> List.ofSeq 
        |> List.map (fun kvp -> kvp.Key, kvp.Value)

    member this.FinalNodeId = finalNodeId

    member this.Merge (other: DDGraph) =
        other.GetNodes() |> List.iter nodeInfoDict.Add
        graph.AddEdgeRange other.Edges |> ignore
        this.AddEdgeFrom other.FinalNodeId

    member this.ToDot name path =
        use file = FileInfo(path).CreateText()
        file.WriteLine("digraph " + name + " {")
        do this.GetNodes()
        |> List.ofSeq
        |> List.map 
            (
                fun (id, node) -> id.ToString() + " [label=\"" + node.Info + "\"]"
            )
        |> List.iter file.WriteLine
        do this.Edges
        |> List.ofSeq
        |> List.map
            (
                fun edge -> edge.Source.ToString() + " -> " + edge.Target.ToString()
            )
        |> List.iter file.WriteLine
        file.WriteLine("}")

module DDGraphFuncs =
    let buildForVar (varRef: IReferenceExpression) (astCfgMap: Dictionary<ITreeNode, IControlFlowElement>) =
        let rec build (cfgNode: IControlFlowElement) (varName: string) (graph: DDGraph) =
            let updateGraph treeNode textInfo =
                let nodeId = treeNode.GetHashCode()
                let nodeInfo = {
                    Info = textInfo
                    AstElem = treeNode
                }
                graph.AddEdgeFrom (nodeId, nodeInfo)
                nodeId

            let processExpr (expr: ICSharpExpression) =
                match expr with
                | :? ICSharpLiteralExpression as literalExpr ->
                    updateGraph literalExpr ("literal(" + literalExpr.Literal.GetText().Trim[|'\"'|] + ")") 
                    |> ignore
                // not implemented (but must precede IReferenceExpression case)
                | :? IInvocationExpression -> ()
                | :? IReferenceExpression as refExpr ->
                    let curVarName = refExpr.NameIdentifier.Name
                    let addedNodeId = updateGraph refExpr ("refExpr(" + curVarName + ")")
                    if curVarName = varName
                    then 
                        let curConnectionNode = graph.CurrentConnectionNode
                        graph.SetConnectionNode addedNodeId
                        let curCggNode = astCfgMap.[refExpr]
                        build curCggNode varName graph
                        graph.SetConnectionNode curConnectionNode
                    else 
                        failwith ("not implemented new ref case in processExpr")
                | _ -> failwith ("not implemented case in processExpr: " + expr.NodeType.ToString())
                    
            let processAssignOps (ops: list<ICSharpExpression>) = 
                ops |> List.iter processExpr

            let processAssignment (assignExpr: IAssignmentExpression) = 
                let assingText, operands = 
                    if assignExpr.AssignmentType = AssignmentType.EQ
                    then 
                        "assignment", assignExpr.OperatorOperands |> List.ofSeq |> List.tail
                    else 
                        "plusAssignment", assignExpr.OperatorOperands |> List.ofSeq
                let addedNodeId = updateGraph assignExpr (assingText + "(" + varName + ")")
                graph.SetConnectionNode addedNodeId
                processAssignOps operands

            let processInitializer (initializer: ITreeNode) = 
                match initializer with
                | :? ICSharpExpression as expr ->
                    processExpr expr
                | :? IExpressionInitializer as exprInitializer ->
                    processExpr exprInitializer.Value
                | _ -> failwith ("not implemented case in processInitializer: " + initializer.NodeType.ToString())

            let processLocVarDecl (locVarDecl: ILocalVariableDeclaration) =
                let addedNodeId = updateGraph locVarDecl ("varDecl(" + varName + ")")
                graph.SetConnectionNode addedNodeId
                processInitializer locVarDecl.Initializer

            let astNode = cfgNode.SourceElement
            match astNode with
            | :? IAssignmentExpression as assignExpr 
                when 
                    match assignExpr.Dest with 
                    | :? IReferenceExpression as dst 
                        when dst.NameIdentifier.Name = varName -> true
                    | _ -> false
                -> 
                    processAssignment assignExpr
            | :? ILocalVariableDeclaration as locVarDecl
                when locVarDecl.NameIdentifier.Name = varName 
                -> 
                    processLocVarDecl locVarDecl
            | _ ->
                let curConnectionNode = graph.CurrentConnectionNode
                cfgNode.Entries
                |> List.ofSeq
                |> List.choose (fun rib -> if rib.Source <> null then Some(rib.Source) else None) 
                |> List.iter 
                    (
                        fun src ->
                            graph.SetConnectionNode curConnectionNode
                            build src varName graph
                    )

        let finalNodeId = varRef.GetHashCode()
        let varName = varRef.NameIdentifier.Name
        let finalNodeInfo = {
            Info = "varRef(" + varName + ")"
            AstElem = varRef
        }
        let graph = DDGraph(finalNodeId, finalNodeInfo)
        let cfgNode = astCfgMap.[varRef]
        build cfgNode varName graph
        graph