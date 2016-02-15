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
        | Simple (blockType, _)
        | Complicated (blockType, _) -> blockType.ToString()

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

/// <summary>
/// Builds CfgBlocksGraph.
/// </summary>
type GraphConstructor<'TokenType> = 
    val Graph : CfgBlocksGraph<'TokenType>
    val mutable CurrentVertex : int
    val mutable NextVertex : int

    new (g, s, e) = {Graph = g; CurrentVertex = s; NextVertex = e}
    new (g : CfgBlocksGraph<_>) = new GraphConstructor<_>(g, g.FirstVertex, g.FirstVertex)
    new () = new GraphConstructor<_>(new CfgBlocksGraph<_>())

    /// <summary>
    /// Creates new vertex.
    /// </summary>
    member this.CreateNewVertex() = 
        this.NextVertex <- if this.Graph.VertexCount > 0 then this.Graph.VertexCount else 1
        this.NextVertex

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
    member this.UpdateVertex() = this.CurrentVertex <- this.NextVertex

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