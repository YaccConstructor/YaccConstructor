namespace AbstractParsing.Common

open QuickGraph

[<Struct>]
type EdgeLabel<'token,'br>=
    val Token : 'token
    val BackRefs : array<'br>

    new (token,brs) = {Token = token; BackRefs = brs}

type AEdge<'token,'br>(s,e,t)=
    inherit TaggedEdge<int,EdgeLabel<'token,'br>>(s,e,t)

type ParserInputGraph<'token,'br>() =
    inherit AdjacencyGraph<int,AEdge<'token,'br>>()

    member this.AddEdgeForsed (e:AEdge<_,_>) =
        this.AddVertex e.Source |> ignore
        this.AddVertex e.Target |> ignore
        this.AddEdge e |> ignore
