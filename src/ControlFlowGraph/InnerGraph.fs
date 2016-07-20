module ControlFlowGraph.InnerGraph

open Microsoft.FSharp.Collections

open QuickGraph

open ControlFlowGraph.Common
open ControlFlowGraph.CfgTokensGraph
open ControlFlowGraph.GraphConstructor

type EdgeType<'TokenType> = 
| Simple of BlockType * CfgTokensGraph<'TokenType>
//blockType * graph for id * graph for right part
| AssignmentEdge of BlockType * CfgTokensGraph<'TokenType> * CfgBlocksGraph<'TokenType> option
| Complicated of BlockType * CfgBlocksGraph<'TokenType>
| EmptyEdge
    
    override this.ToString() =
        match this with
        | EmptyEdge -> "Empty edge"
        | AssignmentEdge (blockType, _, _)
        | Simple (blockType, _)
        | Complicated (blockType, _) -> string blockType


and BlockEdge<'TokenType>(source, target, tag) = 
    inherit TaggedEdge<int, EdgeType<'TokenType>>(source, target, tag)

/// <summary>
/// Recursive data structure. 
/// Each edge contains either graph or tokens array.
/// Looks like simplified cfg.
/// </summary>
and CfgBlocksGraph<'TokenType>() = 
    inherit AdjacencyGraph<int, BlockEdge<'TokenType>>()

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
    
    member this.AddEdgeForced (edge : BlockEdge<_>) = 
        this.AddVertex edge.Source |> ignore
        this.AddVertex edge.Target |> ignore
        this.AddEdge edge |> ignore
    
type BlocksGraphBuilder<'TokenType>() = 
    let builder = new GraphConstructor<_, _>(new CfgBlocksGraph<'TokenType>(), 0, 0)
    
    let createEdge source target tag = 
        new BlockEdge<'TokenType>(source, target, tag)

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
        builder.Graph :?> CfgBlocksGraph<'TokenType>