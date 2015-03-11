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
        graph.AddEdge (new Edge<int>(src, dst)) |> ignore

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
        let rec build (cfgNode: IControlFlowElement) (varName: string) (graph: DDGraph)
            =
            let processExpr (expr: ICSharpExpression) =
    //            match expr with
    //            | :? ICSharpLiteralExpression as literalExpr ->
    //                ()
                1

            let processAssignOps ops = ()

            let processAssignment (assignExpr: IAssignmentExpression) = 
                let curNodeId = assignExpr.GetHashCode()
                let curNodeInfo = {
                    Info = ("assgnment(" + varName + ")")
                    AstElem = assignExpr
                }
                graph.AddEdgeFrom (curNodeId, curNodeInfo)
                graph.SetConnectionNode curNodeId
                let operands = 
                    if assignExpr.AssignmentType = AssignmentType.EQ
                    then assignExpr.OperatorOperands |> List.ofSeq |> List.tail
                    else assignExpr.OperatorOperands |> List.ofSeq
                processAssignOps operands

            let processInitializer initializer = ()

            let processLocVarDecl (locVarDecl: ILocalVariableDeclaration) =
                let curNodeId = locVarDecl.GetHashCode()
                let curNodeInfo = {
                    Info = ("varDecl(" + varName + ")")
                    AstElem = locVarDecl
                }
                graph.AddEdgeFrom (curNodeId, curNodeInfo)
                graph.SetConnectionNode curNodeId
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