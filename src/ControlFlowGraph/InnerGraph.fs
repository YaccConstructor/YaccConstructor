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
        | Simple tokens -> tokens.ToString()// "Simple edge"
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
    member this.EndVertex = Seq.max this.Vertices

    member this.AddEdgeForced (e : BlockEdge<'TokenType>) =
        this.AddVertex e.Source |> ignore
        this.AddVertex e.Target |> ignore
        this.AddEdge e |> ignore

    /// <summary>
    /// Prints graph to .dot file. 
    /// Each edge contains edgeType only
    /// </summary>
    /// <param name="name">Name of .dot file</param>
    /// <param name="tokenToStringOpt">Token to string mapping option</param>
    member this.RelaxedPrintToDot (name : string) (tokenToStringOpt : _ option)= 
        use out = new StreamWriter (name : string)
        out.WriteLine("digraph AST {")
        out.WriteLine "rankdir=LR"
        this.Vertices
        |> Seq.iter (fun i -> out.Write (sprintf "%d ;" i))
        out.WriteLine()

        let getSuffix (tag : EdgeType<_>) = 
            if tokenToStringOpt.IsNone
            then 
                ""
            else
                let tok2String = tokenToStringOpt.Value
                match tag with
                | Complicated (_, graph) -> 
                    if graph.EdgeCount = 1 
                    then 
                        let edge = graph.Edges |> Seq.toList |> List.head 
                        match edge.Tag with
                        | Simple tokens -> 
                            tokens 
                            |> List.map tok2String
                            |> String.concat " "
                        | _ -> ""
                    else ""
                | _ -> ""

        this.Edges
        |> Seq.iter
            (
                fun edge ->
                    let prefix = EdgeType<_>.ToString edge.Tag
                    let suffix = getSuffix edge.Tag
                    let tagName = prefix + " " + suffix
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

    /// <summary>
    ///<para>Adds edge with edgeTag to graph.</para><br />
    ///<para>Source vertex is StartVertex.</para><br />
    ///<para>Target vertex is EndVertex.</para><br />
    /// </summary>
    member this.AddEdge edgeTag = 
        let edge = new BlockEdge<_>(this.StartVertex, this.EndVertex, edgeTag)
        this.Graph.AddEdgeForced edge

    /// <summary>
    ///<para>Adds edge with edgeTag from source vertex to target vertex</para><br />
    /// </summary>
    member this.AddEdgeFromTo edgeTag source target = 
        let edge = new BlockEdge<_>(source, target, edgeTag)
        this.Graph.AddEdgeForced edge

    /// <summary>
    /// Updates StartVertex and EndVertex values.
    /// <para>StartVertex becomes equal to EndVertex and</para><br />
    /// EndVertex increments its value.
    /// </summary>
    member this.UpdateVertices() = 
        this.StartVertex <- this.EndVertex
        this.EndVertex <- this.EndVertex + 1