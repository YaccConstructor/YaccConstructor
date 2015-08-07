module ControlFlowGraph.InnerGraph

open System.IO

open QuickGraph

open ControlFlowGraph.Common

type EdgeType<'TokenType> = 
| Simple of list<'TokenType>
| Complicated of BlockType * CfgBlocksGraph<'TokenType>
| EmptyEdge
    
    static member ToString edge = 
        match edge with
        | EmptyEdge -> "Empty edge"
        | Simple _ -> "Simple edge"
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

    member this.StartVertex = 0

    member this.AddEdgeForced (e : BlockEdge<'TokenType>) =
        this.AddVertex e.Source |> ignore
        this.AddVertex e.Target |> ignore
        this.AddEdge e |> ignore

    /// <summary>
    /// Prints graph to .dot file. 
    /// Each edge contains edgeType only
    /// </summary>
    /// <param name="name">Name of .dot file</param>
    member this.RelaxedPrintToDot (name : string) = 
        use out = new StreamWriter (name : string)
        out.WriteLine("digraph AST {")
        out.WriteLine "rankdir=LR"
        this.Vertices
        |> Seq.iter (fun i -> out.Write (sprintf "%d ;" i))
        out.WriteLine()

        this.Edges
        |> Seq.iter
            (
                fun edge ->
                    let tagName = EdgeType<_>.ToString edge.Tag
                    out.WriteLine (sprintf "%d -> %d [label=\" %s \"]" edge.Source edge.Target tagName)
            )
        out.WriteLine("}")
        out.Close()

/// <summary>
/// Builds CfgBlocksGraph
/// </summary>
type GraphConstructor<'TokenType> = 
    ///current graph
    val Graph : CfgBlocksGraph<'TokenType>
    ///existing vertex with the biggest number
    val mutable StartVertex : int
    ///next vertex will have this number
    val mutable EndVertex : int

    new (g, s, e) = {Graph = g; StartVertex = s; EndVertex = e}
    new (g : CfgBlocksGraph<_>) = new GraphConstructor<_>(g, g.StartVertex, g.StartVertex + 1)
    new () = new GraphConstructor<_>(new CfgBlocksGraph<_>())

    ///Adds edge with edgeTag.
    ///Source vertex is StartVertex.
    ///Target vertex is EndVertex.
    member this.AddEdge edgeTag = 
        let edge = new BlockEdge<_>(this.StartVertex, this.EndVertex, edgeTag)
        this.Graph.AddEdgeForced edge

    /// Adds edge with edgeTag from source vertex to target vertex
    member this.AddEdgeFromTo edgeTag source target = 
        let edge = new BlockEdge<_>(source, target, edgeTag)
        this.Graph.AddEdgeForced edge

    /// <summary>
    /// Updates StartVertex and EndVertex values.
    /// StartVertex becomes equal to EndVertex and
    /// EndVertex increments its value.
    /// </summary>
    member this.UpdateVertices() = 
        this.StartVertex <- this.EndVertex
        this.EndVertex <- this.EndVertex + 1