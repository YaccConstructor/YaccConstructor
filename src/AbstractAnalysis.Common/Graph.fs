namespace AbstractAnalysis.Common

open QuickGraph

type LexerEdge<'l ,'br  when 'l: equality> (s,e,t) =
    inherit TaggedEdge<int,Option<'l*'br>>(s,e,t)
    let l,br =
        match t with
        | Some (l,br) -> Some l, Some br
        | None -> None, None

    member this.BackRef = br
    member this.Label = l

type DAG<'l,'br  when 'l: equality> () =
    inherit AdjacencyGraph<int, LexerEdge<'l,'br>>()
    let mutable startV = None

    member this.AddEdgeForsed (e:LexerEdge<'l,'br>) =        
        this.AddVertex e.Source |> ignore
        this.AddVertex e.Target |> ignore
        this.AddEdge e |> ignore

    member this.AddEdgesForsed (edges:#seq<LexerEdge<_,_>>) =
        Seq.iter this.AddEdgeForsed edges

    member this.StartVertex 
        with get () = match startV with Some v -> v | _ -> failwith "Start vertex is not defined!"
        and set (v:int) = startV <- Some v

type LexerInputGraph<'br> () =
    inherit DAG<string,'br>()    

    member this.PrintToDot name = 
        use out = new System.IO.StreamWriter (name : string)
        out.WriteLine("digraph AST {")
        out.WriteLine "rankdir=LR"
        for i=0 to this.VertexCount-1 do
            out.Write (i.ToString() + "; ")
        out.WriteLine()
        for i in this.Vertices do
            let edges = this.OutEdges i
            for e in edges do
                let tokenName = 
                    match e.Tag with
                    | Some pair -> fst pair
                    | None -> ""
                out.WriteLine (e.Source.ToString() + " -> " + e.Target.ToString() + "[label=\"" + tokenName + "\"]")
        out.WriteLine("}")
        out.Close()

type LexerInnerGraph<'br> (g:LexerInputGraph<'br>) as this =
    inherit DAG<char,'br>()
    let convert () =
        let counter = g.Vertices |> Seq.max |> ref
        let splitEdge (edg:LexerEdge<_,'br>) =
            let start = edg.Source
            let _end = edg.Target
            let str = edg.Label
            let br = edg.BackRef
            match str with
            | Some("") -> [|new LexerEdge<_,_> (start,_end,None)|]
            | None -> [|new LexerEdge<_,_> (start,_end,None)|]
            | Some(s) ->
                let l = s.Length
                let ss = s.ToCharArray()
                Array.init l 
                    (fun i ->
                        match i with
                        | 0 when (l = 1)     -> new LexerEdge<_,_>(start,_end, Some(ss.[i],br.Value))
                        | 0                  -> new LexerEdge<_,_>(start,(incr counter; !counter),Some(ss.[i],br.Value))
                        | i when (i = l - 1) -> new LexerEdge<_,_>(!counter,_end,Some(ss.[i],br.Value))
                        | i                  -> new LexerEdge<_,_>(!counter,(incr counter; !counter),Some(ss.[i],br.Value))
                    )
        let newEdges = g.Edges |> Array.ofSeq |> Array.collect splitEdge
        this.AddEdgesForsed(newEdges)
        this.StartVertex <- g.StartVertex

    do convert()

type ParserEdge<'token>(s, e, t)=
    inherit TaggedEdge<int, 'token>(s,e,t)
    
type ParserInputGraph<'token>(initialVertices : int[], finalVertices : int[]) = 
    inherit AdjacencyGraph<int,ParserEdge<'token>>()

    member val InitStates = initialVertices 
    member val FinalStates = finalVertices with get, set

    member this.PrintToDot name (tokenToString : 'token -> string) = 
        use out = new System.IO.StreamWriter (name : string)
        out.WriteLine("digraph AST {")
        out.WriteLine "rankdir=LR"
        for i=0 to this.VertexCount-1 do
            out.Write (i.ToString() + "; ")
        out.WriteLine()
        for i in this.Vertices do
            let edges = this.OutEdges i
            for e in edges do
                let tokenName = e.Tag |> tokenToString
                out.WriteLine (e.Source.ToString() + " -> " + e.Target.ToString() + "[label=\"" + tokenName + "\"]")
        out.WriteLine("}")
        out.Close()      

    new (initial : int, final : int) = 
        ParserInputGraph<_>([|initial|], [|final|])
 
type BioParserEdge(s : int, e : int, l : int, t : int[]) =
    member this.Start = s
    member this.End = e
    member this.RealLenght = l
    member this.Tokens = t 
      
type BioParserInputGraph(edges : BioParserEdge[]) =
    let 
    let edgs =  
    member val Edges = [] with get, private set
//    member this.MapToOriginalGraph = 
//    member this.InitialVertices = initialVertices
//    member this.FinalVertex = finalVertex
//    member this.ChainLength = chainLen
//    member this.EdgeCount = edges.Length
//    member this.VertexCount = vertexCount
//    member this.Shift = 


