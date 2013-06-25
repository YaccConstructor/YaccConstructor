namespace AbstractLexer.Common

open QuickGraph

type AEdge<'l,'br> (s,e,t) =
    inherit TaggedEdge<int,Option<'l>*Option<'br>>(s,e,t)
    member this.BackRef = snd t
    member this.Label = fst t

type DAG<'l,'br> () =
    inherit AdjacencyGraph<int, AEdge<'l,'br>>()
    let mutable startV = None

    member this.AddEdgeForsed (e:AEdge<_,_>) =        
        this.AddVertex e.Source |> ignore
        this.AddVertex e.Target |> ignore
        this.AddEdge e |> ignore

    member this.AddEdgesForsed (edges:#seq<AEdge<_,_>>) =
        Seq.iter this.AddEdgeForsed edges

    member this.StartVertex 
        with get () = match startV with Some v -> v | _ -> failwith "Start vertex is not defined!"
        and set (v:int) = startV <- Some v

type LexerInputGraph<'br> () =
    inherit DAG<string,'br>()    

type LexerInnerGraph<'br> (g:LexerInputGraph<'br>) as this =
    inherit DAG<char,'br>()
    let convert () =
        let edges = g.Edges |> Array.ofSeq
        let counter = g.Vertices |> Seq.max |> ref
        let splitEdge (edg:AEdge<_,_>) =
            let start = edg.Source
            let _end = edg.Target
            let str = edg.Label
            let br = edg.BackRef
            match str with
            | Some("") -> [|new AEdge<_,_> (start,_end,(None,None))|]
            | None -> [|new AEdge<_,_> (start,_end,(None,None))|]
            | Some(s) ->
                let l = s.Length
                s |> Seq.mapi (fun i ch ->
                        match i with
                        | 0 when (l = 1)     -> new AEdge<_,_>(start,_end,(Some ch,br))
                        | 0                  -> new AEdge<_,_>(start,(incr counter; !counter),(Some ch,br))
                        | i when (i = l - 1) -> new AEdge<_,_>(!counter,_end,(Some ch,br))
                        | i                  -> new AEdge<_,_>(!counter,(incr counter; !counter),(Some ch,br))
                        ) 
                |> Array.ofSeq
        let newEdges = edges |> Array.collect splitEdge
        this.AddEdgesForsed(newEdges)
        //if res <> newEdges.Length then failfith ""
        this.StartVertex <- g.StartVertex

    do convert()