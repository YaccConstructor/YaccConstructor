module AbstractLexer.Core.Test

open AbstractParsing.Common


type Class1() = 
    let graph = new ParserInputGraph<_>()
    let init () =
        [
          new ParserEdge<_>(0,1,None)
          new ParserEdge<_>(0,1,Some 3)
          new ParserEdge<_>(0,1,Some 2)
        ]
        |> graph.AddVerticesAndEdgeRange
    member this.X () = 
        init()
        EpsClosure.NfaToDfa graph
        
let c = new Class1()
let r = c.X()
printfn "%A" r.Edges