module ControlFlowGraph.CfgTokensGraph

open QuickGraph

open ControlFlowGraph.GraphConstructor

type TokensEdge<'TokenType>(source, target, tag) = 
    inherit TaggedEdge<int, 'TokenType option>(source, target, tag)

/// <summary>
/// <para>Intermediate structure that is obtained after processing the AST node or family.</para><br />
/// Each edge is labelled token option.
/// </summary>
type CfgTokensGraph<'TokenType>() =
    inherit AdjacencyGraph<int, TokensEdge<'TokenType>>()
    
    member this.StartVertex = 0

    member this.GetAvailableTokens() = 
        
        this.Edges
        |> Seq.filter (fun edge -> edge.Tag |> Option.isSome)
        |> Seq.map (fun edge -> edge.Tag |> Option.get)

type TokensGraphBuilder<'TokenType>() = 
    
    let builder = new GraphConstructor<_, _>(new CfgTokensGraph<'TokenType>(), 0, 0)
    
    let createEdge source target tag = 
        new TokensEdge<'TokenType>(source, target, tag)

    member this.CurrentVertex 
        with get() = builder.CurrentVertex
        and set value = builder.CurrentVertex <- value
    
    member this.NextVertex 
        with get() = builder.NextVertex
        and set value = builder.NextVertex <- value    

    member this.CreateNewVertex() = 
        builder.CreateNewVertex()

    member this.AddEdge edgeTag = 
        builder.AddEdge createEdge edgeTag

    member this.AddEdgeFromTo edgeTag source target = 
        builder.AddEdgeFromTo createEdge edgeTag source target

    member this.UpdateVertex() =    
        builder.UpdateVertex()
    
    member this.TryFindLastVertex start = 
        builder.TryFindLastVertex start

    member this.Build() = 
        builder.Graph :?> CfgTokensGraph<_>