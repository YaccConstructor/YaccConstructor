namespace AbstractParsing.Common

open QuickGraph


type ParserEdge<'token>(s,e,t)=
    inherit TaggedEdge<int, 'token>(s,e,t)

type ParserInputGraph<'token>() =
    inherit AdjacencyGraph<int,ParserEdge<'token>>()
