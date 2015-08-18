module ControlFlowGraph.InnerGraph

open Microsoft.FSharp.Collections

open System.IO
open System.Collections.Generic

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

    let isEpsilonEdge (edge : BlockEdge<_>) = 
        match edge.Tag with
        | EmptyEdge -> true
        | _ -> false

    member this.StartVertex = 0
    member this.EndVertex = Seq.max this.Vertices

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
    /// Prints graph to .dot file. 
    /// Each complicated edge (except ASSIGNMENT) contains edgeType only
    /// </summary>
    /// <param name="name">Name of .dot file</param>
    /// <param name="tokenToStringOpt">Token to string mapping option</param>
    member this.RelaxedPrintToDot (name : string) (tokenToStringOpt : _ option)= 
        use out = new StreamWriter (name : string)
        out.WriteLine("digraph AST {")
        out.WriteLine "rankdir=LR"
        this.Vertices
        |> Seq.iter (sprintf "%d ;" >> string >> out.Write)
        out.WriteLine()

        let getSuffix (tag : EdgeType<_>) = 
            match tokenToStringOpt with
            | None -> ""
            | Some tok2String ->
                match tag with
                | Complicated (_, graph) -> 
                    if graph.EndVertex = 1 
                    then 
                        let edges = graph.Edges |> Seq.toList
                        edges 
                        |> List.map 
                            (
                                fun edge -> 
                                    match edge.Tag with
                                    | Simple tokens -> 
                                        tokens 
                                        |> List.map tok2String
                                        |> String.concat " "
                                    | _ -> ""
                            )
                        |> String.concat " or \n"
                    else "complicated"
                | _ -> ""

        this.Edges
        |> Seq.iter
            (
                fun edge ->
                    let prefix = EdgeType<_>.ToString edge.Tag
                    let suffix = getSuffix edge.Tag
                    let tagName = sprintf "%s [%s]" prefix suffix
                    out.WriteLine (sprintf "%d -> %d [label=\" %s \"]" edge.Source edge.Target tagName)
            )
        out.WriteLine("}")
        out.Close()

/// <summary>
/// Builds CfgBlocksGraph.
/// </summary>
type GraphConstructor<'TokenType> = 
    ///Current graph
    val Graph : CfgBlocksGraph<'TokenType>
    ///Existing vertex 
    val mutable StartVertex : int
    ///Next vertex will have this number
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

    member this.FindLastVertex start = 
        let markedEdges = new ResizeArray<_>()
        let queue = new Queue<_>()
        let graph = this.Graph

        let addEdge edge = 
            if not <| markedEdges.Contains edge
            then
                queue.Enqueue edge
                markedEdges.Add edge

        this.Graph.OutEdges start
        |> Seq.iter addEdge

        let mutable res = []

        while queue.Count > 0 do
            let edge = queue.Dequeue()
            if graph.OutDegree edge.Target = 0
            then res <- edge.Target :: res
            else 
                this.Graph.OutEdges(edge.Target)
                |> Seq.iter addEdge

        graph.RelaxedPrintToDot "`temp.dot" None
        if res.Length <> 1
        then None //failwith "OOOOPS"
        else Some <| res.Head