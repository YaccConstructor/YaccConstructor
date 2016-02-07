module ControlFlowGraph.CfgTokensGraph

open QuickGraph

type TokensEdge<'TokenType>(source, target, tag) = 
    inherit TaggedEdge<int, 'TokenType option>(source, target, tag)

/// <summary>
/// <para>Intermediate structure that is obtained after processing the AST node or family.</para><br />
/// Each edge is labelled token option.
/// </summary>
type CfgTokensGraph<'TokenType>() =
    inherit AdjacencyGraph<int, TokensEdge<'TokenType>>()
    
    let mutable currentVertex = 0
    let mutable lastVertex = 0
    
    member this.StartVertex = 0
    member this.CurrentVertex 
        with get() = currentVertex
        and set v = currentVertex <- v

    member this.LastVertex 
        with get() = lastVertex
        and set v = lastVertex <- v

    member this.AddEdgeForced (e : TokensEdge<'TokenType>) =
        this.AddVertex e.Source |> ignore
        this.AddVertex e.Target |> ignore
        this.AddEdge e |> ignore

    /// <summary>
    /// Creates new vertex.
    /// </summary>
    member this.CreateNewVertex() = 
        lastVertex <- lastVertex + 1
        lastVertex

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
    member this.UpdateVertex() = currentVertex <- lastVertex

    member this.GetAvailableTokens() = 
        this.Edges
        |> Seq.fold 
            (
                fun acc edge -> 
                    match edge.Tag with
                    | Some token -> token :: acc
                    | None -> acc
            ) []