module AbstractLexer.Core.Test

open AbstractParsing.Common


type Class1() = 
    let graph = new ParserInputGraph<_>()
    let init () =
        [
//          new ParserEdge<_>(2,0,None)
//          new ParserEdge<_>(0,1,None)
//          new ParserEdge<_>(0,1,Some 3)
//          new ParserEdge<_>(0,1,Some 2)
            new ParserEdge<_>(0,7,Some '1')
            new ParserEdge<_>(7,8,Some '+')
            new ParserEdge<_>(8,9,Some '(')
            new ParserEdge<_>(9,1,Some '2')
            new ParserEdge<_>(1,2,Some '*')
            new ParserEdge<_>(2,4,Some '4')
            new ParserEdge<_>(4,5,Some ')')
            new ParserEdge<_>(0,10,Some '1')
            new ParserEdge<_>(10,11,Some '+')
            new ParserEdge<_>(11,12,Some '(')
            new ParserEdge<_>(12,6,Some '2')
            new ParserEdge<_>(6,3,Some '/')
            new ParserEdge<_>(3,4,Some '5')
        ]
        |> graph.AddVerticesAndEdgeRange
    member this.X () = 
        init()|>ignore
        EpsClosure.NfaToDfa graph
        
let c = new Class1()
let r = c.X()
printfn "%A" (r.Edges |> Seq.toList)
