module ControlFlowGraph.InnerGraph

open Microsoft.FSharp.Collections

open System.Collections.Generic

open QuickGraph

open ControlFlowGraph.Common
open ControlFlowGraph.CfgTokensGraph

type EdgeType<'TokenType> = 
| Simple of BlockType * CfgTokensGraph<'TokenType>
| Complicated of BlockType * CfgBlocksGraph<'TokenType>
| EmptyEdge
    
    static member ToString edge = 
        match edge with
        | EmptyEdge -> "Empty edge"
        | Simple (blockType, graph) -> BlockType.BlockTypeToString blockType// "Simple edge"
        | Complicated (blockType, _) -> BlockType.BlockTypeToString blockType

and BlockEdge<'TokenType>(source, target, tag) = 
    inherit TaggedEdge<int, EdgeType<'TokenType>>(source, target, tag)

/// <summary>
/// Recursive data structure. 
/// Each edge contains either graph or tokens array.
/// Looks like simplified cfg.
/// </summary>
and CfgBlocksGraph<'TokenType>() = 
    inherit AdjacencyGraph<int, BlockEdge<'TokenType>>()

    let isEpsilonEdge (edge : BlockEdge<_>) = 
        match edge.Tag with
        | EmptyEdge -> true
        | _ -> false

    member this.FirstVertex = 0
    
    //vertex that has't out edges
    member this.LastVertex = 
        
        let stockVertices = 
            this.Vertices
            |> Seq.filter(fun vertex -> this.OutDegree vertex = 0)
            |> List.ofSeq

        match stockVertices with
        | [head] -> head
        | _ -> failwith "Incorrect inner graph was built: only one stock vertices must be" 

    member this.AddEdgeForced (e : BlockEdge<'TokenType>) =
        this.AddVertex e.Source |> ignore
        this.AddVertex e.Target |> ignore
        this.AddEdge e |> ignore

    member this.RemoveDuplicateEpsilons() = 
        
        let isTheSame (edge1 : BlockEdge<_>) (edge2 : BlockEdge<_>) = 
            edge1.Source = edge2.Source && edge1.Target = edge2.Target

        let needRemove = ref []

        this.Edges
        |> Seq.filter isEpsilonEdge
        |> Seq.fold
            (
                fun acc edge -> 
                    if acc |> List.exists (isTheSame edge)
                    then 
                        needRemove := edge :: !needRemove
                        acc
                    else edge :: acc
            ) []
        |> ignore

        !needRemove
        |> List.iter (this.RemoveEdge >> ignore)

    member this.RemoveEpsilonLoopEdges() = 
        
        let needRemove = ref []

        this.Edges
        |> Seq.filter isEpsilonEdge
        |> Seq.filter (fun edge -> edge.Source = edge.Target)
        |> Seq.iter (fun edge -> needRemove := edge :: !needRemove)

        !needRemove
        |> List.iter (this.RemoveEdge >> ignore)

/// <summary>
/// Builds CfgBlocksGraph.
/// </summary>
type GraphConstructor<'TokenType> = 
    val Graph : CfgBlocksGraph<'TokenType>
    val mutable CurrentVertex : int
    val mutable LastVertex : int

    new (g, s, e) = {Graph = g; CurrentVertex = s; LastVertex = e}
    new (g : CfgBlocksGraph<_>) = new GraphConstructor<_>(g, g.FirstVertex, g.FirstVertex)
    new () = new GraphConstructor<_>(new CfgBlocksGraph<_>())

    /// <summary>
    /// Creates new vertex.
    /// </summary>
    member this.CreateNewVertex() = 
        this.LastVertex <- if this.Graph.VertexCount > 0 then this.Graph.VertexCount else 1
        this.LastVertex

    /// <summary>
    ///<para>Adds edge with edgeTag to graph.</para><br />
    ///<para>Source vertex is CurrentVertex.</para><br />
    ///<para>Target vertex is new created vertex.</para><br />
    /// </summary>
    member this.AddEdge edgeTag = 
        let target = this.CreateNewVertex()
        let edge = new BlockEdge<_>(this.CurrentVertex, target, edgeTag)
        this.Graph.AddEdgeForced edge

    /// <summary>
    ///<para>Adds edge with edgeTag from source vertex to target vertex</para><br />
    /// </summary>
    member this.AddEdgeFromTo edgeTag source target = 
        let edge = new BlockEdge<_>(source, target, edgeTag)
        this.Graph.AddEdgeForced edge

    /// <summary>
    /// CurrentVertex becomes equal to number of last created vertex.
    /// </summary>
    member this.UpdateVertex() = this.CurrentVertex <- this.LastVertex

    /// <summary>
    /// Does BFS from start vertex. Returns the vertex that has out degree = 0.
    /// If such vertices are few or none, then None will be returned.
    /// </summary>
    /// <param name="start">First vertex for BFS</param>
    member this.TryFindLastVertex start = 
        
        let queue = new Queue<_>()
        let addEdge = 
            let markedEdges = new ResizeArray<_>()
            fun edge -> 
                if not <| markedEdges.Contains edge
                then
                    queue.Enqueue edge
                    markedEdges.Add edge

        this.Graph.OutEdges start
        |> Seq.iter addEdge

        let mutable res = []

        while queue.Count > 0 do
            let edge = queue.Dequeue()
            if this.Graph.OutDegree edge.Target = 0
            then 
                res <- edge.Target :: res
            else 
                this.Graph.OutEdges(edge.Target)
                |> Seq.iter addEdge

        match res with
        | [ head ] -> Some head
        | _ -> None