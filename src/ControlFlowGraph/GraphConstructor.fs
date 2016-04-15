module ControlFlowGraph.GraphConstructor

open QuickGraph

type AdjacencyGraph<'TVertex, 'Edge when 'Edge :> IEdge<'TVertex>> with
    member this.AddEdgeForced (edge : #IEdge<_>) = 
        this.AddVertex edge.Source |> ignore
        this.AddVertex edge.Target |> ignore
        this.AddEdge edge |> ignore

type GraphConstructor<'TokenType, 'Edge when 'Edge :> IEdge<int>> = 
    val Graph : AdjacencyGraph<int, 'Edge>
    val mutable CurrentVertex : int
    val mutable NextVertex : int

    new (g, s, e) = {Graph = g; CurrentVertex = s; NextVertex = e}

    /// <summary>
    /// Creates new vertex.
    /// </summary>
    member this.CreateNewVertex() = 
        this.NextVertex <- 
            if this.Graph.VertexCount > 0 
            then this.Graph.VertexCount 
            else 1
        this.NextVertex

    /// <summary>
    ///<para>Adds an edge with edgeTag to graph.</para><br />
    ///<para>Source vertex is CurrentVertex.</para><br />
    ///<para>Target vertex is new created vertex.</para><br />
    /// </summary>
    member this.AddEdge createEdge edgeTag = 
        let target = this.CreateNewVertex()
        let edge = createEdge this.CurrentVertex target edgeTag
        this.Graph.AddEdgeForced edge

    /// <summary>
    ///<para>Adds an edge with edgeTag from source vertex to target vertex</para><br />
    /// </summary>
    member this.AddEdgeFromTo createEdge edgeTag source target = 
        let edge = createEdge source target edgeTag
        this.Graph.AddEdgeForced edge

    /// <summary>
    /// CurrentVertex becomes equal to number of NextVertex.
    /// </summary>
    member this.UpdateVertex() = this.CurrentVertex <- this.NextVertex

    /// <summary>
    /// Does BFS from start vertex. Returns the vertex that has out degree = 0.
    /// If such vertices are few or none, then None will be returned.
    /// </summary>
    /// <param name="start">First vertex for BFS</param>
    member this.TryFindLastVertex start = 
        
        let getOutVertices vertex = 
            this.Graph.OutEdges vertex
            |> Seq.map (fun edge -> edge.Target)
            |> Seq.toList

        let rec func acc processed queue = 
            
            match queue with
            | [] -> acc
            | head :: tail -> 
                
                if processed |> List.exists ((=) head)
                then 
                    func acc processed tail
                elif this.Graph.OutDegree head = 0
                then 
                    func (head :: acc) (head :: processed) tail
                else
                    let newQueue = 
                        tail
                        |> List.append <| getOutVertices head

                    func acc (head :: processed) newQueue

        let res = func [] [] <| getOutVertices start

        match res with
        | [head] -> Some head
        | _ -> None