namespace AbstractAnalysis.Common

open QuickGraph

[<Measure>] type token

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
        this.AddVerticesAndEdgeRange(newEdges) |> ignore
        this.StartVertex <- g.StartVertex

    do convert()

type ParserEdge<'token>(s, e, t)=
    inherit TaggedEdge<int, 'token>(s,e,t)
    
type ParserInputGraph<'token>(initialVertices : int[], finalVertices : int[]) = 
    inherit AdjacencyGraph<int,ParserEdge<'token>>()

    member val InitStates = initialVertices 
    member val FinalStates = finalVertices with get, set

    member this.PrintToDot name (numToString: 'token -> string) (*(tokenToString : 'token -> string) (numToToken : int -> 'token)*) = 
        use out = new System.IO.StreamWriter (name : string)
        out.WriteLine("digraph AST {")
        out.WriteLine "rankdir=LR"
        for i=0 to this.VertexCount-1 do
            out.Write (i.ToString() + "; ")
        out.WriteLine()
        for i in this.Vertices do
            let edges = this.OutEdges i
            for e in edges do
                let tokenName = e.Tag |> numToString(*numToToken |> tokenToString*)
                out.WriteLine (e.Source.ToString() + " -> " + e.Target.ToString() + "[label=\"" + tokenName + "\"]")
        out.WriteLine("}")
        out.Close()      

    new (initial : int, final : int) = 
        ParserInputGraph<_>([|initial|], [|final|])
 
type BioParserEdge(s : int, e : int, l : int, t : int[], id : int, startPos : int) =
    member this.Start = s
    member this.End = e
    member this.RealLength = l
    member this.Tokens = t 
    member this.SourceId = id
    member this.SourceStartPos = startPos
    override this.ToString () = (this.Start.ToString()) + "- "+ (this.Tokens.[0].ToString()) + " ->" + (this.End.ToString()) 
      
type BioParserInputGraph(edges : BioParserEdge[]) =
//    inherit AdjacencyGraph<int, TaggedEdge<int, int[]>>()
//    do
//        edges |> Array.map (fun e -> new TaggedEdge<_,_>(e.s, e.e, e.Tokens))
    let pack2to32 edge position =
        if (edge < 65536) && (position < 65536) then ((int position <<< 16) ||| int edge)
        else failwith "Edge or position is greater then 65535!!"
    let edgs = Array.zeroCreate edges.Length
    let shift = ref 0//-1
    let vertexCount = ref 0
    /// Length of each edge
    let chainLen = Array.zeroCreate edges.Length
    let initialVertices = new ResizeArray<_>()
    let finalVertex = ref 0
    do        
        let cnt = ref 0
        let vMap = new System.Collections.Generic.Dictionary<_,_>()
        let getV x = 
            let f,v = vMap.TryGetValue x
            if f 
            then v
            else       
                let newV = !cnt                
                vMap.Add(x, newV)
                incr cnt
                newV
        edges
        |> Array.iteri (fun i e -> 
            let edg = new BioParserEdge(getV e.Start, getV e.End, e.RealLength, e.Tokens, e.SourceId, e.SourceStartPos)
            edgs.[i] <- edg
            chainLen.[i] <- e.Tokens.Length
            //shift := 0//max !shift (e.Tokens.Length - e.RealLenght)
            for j in 0..e.Tokens.Length - 1 do
                initialVertices.Add(pack2to32 i (j - !shift)))
        vertexCount := vMap.Count
    
    member this.Edges  with get () = edgs
    member this.InitialVertices with get () = initialVertices.ToArray()
    member this.FinalVertex with get () = !finalVertex
    /// Lengths of edges.
    member this.ChainLength with get () = chainLen
    member this.EdgeCount with get () = edgs.Length
    member this.VertexCount with get () = !vertexCount
    member this.Shift with get () = !shift



