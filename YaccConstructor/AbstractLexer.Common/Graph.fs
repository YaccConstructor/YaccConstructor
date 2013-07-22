namespace AbstractLexer.Common

open QuickGraph

type AEdge<'l ,'br  when 'l: equality> (s,e,t) =
    inherit TaggedEdge<int,Option<'l>*Option<'br>>(s,e,t)
    member this.BackRef = snd t
    member this.Label = fst t

type DAG<'l,'br  when 'l: equality> () =
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
                let ss = s.ToCharArray()
                Array.init l 
                    (fun i ->
                        match i with
                        | 0 when (l = 1)     -> new AEdge<_,_>(start,_end,(Some ss.[i],br))
                        | 0                  -> new AEdge<_,_>(start,(incr counter; !counter),(Some ss.[i],br))
                        | i when (i = l - 1) -> new AEdge<_,_>(!counter,_end,(Some ss.[i],br))
                        | i                  -> new AEdge<_,_>(!counter,(incr counter; !counter),(Some ss.[i],br))
                    )
        let newEdges = g.Edges |> Array.ofSeq |> Array.collect splitEdge
        this.AddEdgesForsed(newEdges)
        this.StartVertex <- g.StartVertex

    do convert()