module ControlFlowGraph.CfgTokensGraph

open QuickGraph
open System.Collections.Generic

type TokensEdge<'TokenType>(source, target, tag) = 
    inherit TaggedEdge<int, 'TokenType option>(source, target, tag)

/// <summary>
/// <para>Intermediate structure that is obtained after processing the AST node or family.</para><br />
/// Each edge is labelled token option.
/// </summary>
type CfgTokensGraph<'TokenType>() =
    inherit AdjacencyGraph<int, TokensEdge<'TokenType>>()
    
    let mutable currentVertex = 0
    let mutable nextVertex = 0
    
    member this.StartVertex = 0
    member this.CurrentVertex 
        with get() = currentVertex
        and set v = currentVertex <- v

    member this.NextVertex 
        with get() = nextVertex
        and set v = nextVertex <- v

    member this.AddEdgeForced (e : TokensEdge<'TokenType>) =
        this.AddVertex e.Source |> ignore
        this.AddVertex e.Target |> ignore
        this.AddEdge e |> ignore

    /// <summary>
    /// Creates new vertex.
    /// </summary>
    member this.CreateNewVertex() = 
        nextVertex <- if this.VertexCount > 0 then this.VertexCount else 1
        nextVertex

    /// <summary>
    ///<para>Adds edge with edgeTag to graph.</para><br />
    ///<para>Source vertex is CurrentVertex.</para><br />
    ///<para>Target vertex is new created vertex.</para><br />
    /// </summary>
    member this.AddEdge edgeTag = 
        let target = this.CreateNewVertex()
        this.AddEdgeFromTo edgeTag currentVertex target

    /// <summary>
    ///<para>Adds edge with edgeTag from source vertex to target vertex</para><br />
    /// </summary>
    member this.AddEdgeFromTo edgeTag source target = 
        let edge = new TokensEdge<'TokenType>(source, target, edgeTag)
        this.AddEdgeForced edge

    /// <summary>
    /// CurrentVertex becomes equal to number of last created vertex.
    /// </summary>
    member this.UpdateVertex() = currentVertex <- nextVertex

    member this.TryFindLastVertex start = 
        let queue = new Queue<_>()
        let addEdge = 
            let markedEdges = new ResizeArray<_>()
            fun edge -> 
                if not <| markedEdges.Contains edge
                then
                    queue.Enqueue edge
                    markedEdges.Add edge

        this.OutEdges start
        |> Seq.iter addEdge

        let mutable res = []

        while queue.Count > 0 do
            let edge = queue.Dequeue()
            if this.OutDegree edge.Target = 0
            then 
                res <- edge.Target :: res
            else 
                this.OutEdges(edge.Target)
                |> Seq.iter addEdge

        match res with
        | [ head ] -> Some head
        | _ -> None

    member this.GetAvailableTokens() = 
        this.Edges
        |> Seq.fold 
            (
                fun acc edge -> 
                    match edge.Tag with
                    | Some token -> token :: acc
                    | None -> acc
            ) []
        |> List.rev

