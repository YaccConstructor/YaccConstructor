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

    member this.FirstVertex = 0
    member this.LastVertex = Seq.max this.Vertices

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
            | Some tokenToString ->
                match tag with
                | Complicated (_, graph) -> 
                    if graph.LastVertex = 1 
                    then 
                        let edges = graph.Edges |> Seq.toList
                        edges 
                        |> List.map 
                            (
                                fun edge -> 
                                    match edge.Tag with
                                    | Simple tokens -> 
                                        tokens 
                                        |> List.map tokenToString
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
    val Graph : CfgBlocksGraph<'TokenType>
    val mutable CurrentVertex : int
    val mutable private LastCreatedVertex : int

    new (g, s, e) = {Graph = g; CurrentVertex = s; LastCreatedVertex = e}
    new (g : CfgBlocksGraph<_>) = new GraphConstructor<_>(g, g.FirstVertex, g.FirstVertex)
    new () = new GraphConstructor<_>(new CfgBlocksGraph<_>())

    /// <summary>
    /// Creates new vertex.
    /// </summary>
    member this.CreateNewVertex() = 
        this.LastCreatedVertex <- this.LastCreatedVertex + 1
        this.LastCreatedVertex

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
    member this.UpdateVertex() = this.CurrentVertex <- this.LastCreatedVertex

    /// <summary>
    /// Does BFS from start vertex. Returns the vertex that has out degree = 0.
    /// If such vertices are few or none, then None will be returned.
    /// </summary>
    /// <param name="start">First vertex for BFS</param>
    member this.FindLastVertex start = 
        
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

        if res.Length = 1
        then Some <| res.Head
        else None