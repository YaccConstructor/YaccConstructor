module YC.SNN

open Microsoft.FSharp.Collections

type SnnNode (f:float->float, g:float->float->float) =
    let mutable outVal = None
    member this.OutVal = outVal
    member this.Input =
        new ResizeArray<_>()
    member this.Activation x = f x
    member this.Folder x y = g x y
    member this.Process () =
        this.Input
        |> ResizeArray.fold (fun state (x:SnnNode,w) -> Option.bind(fun y -> Option.map(fun v -> this.Folder y (w*v)) x.OutVal) state) None
        |> fun x -> outVal <- x


type SNN(grammar, width) =
    do
     let getParserSource =         
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let gen = new Yard.Generators.GLL.GLL()
        YC.API.generate 
                    grammar
                    fe 
                    gen 
                    None
                    Seq.empty
                    [|""|]
                    [] :?> Yard.Generators.GLL.ParserCommon.ParserSourceGLL

     let gll = new Yard.Generators.GLL.GLL()
     let input = new AbstractAnalysis.Common.SimpleInputGraph<_>([|0|],[|0|],id)
     let batch i j = [for t in getParserSource.TerminalNums -> new AbstractAnalysis.Common.ParserEdge<_>(i, j, int t)]
     //for i in 0 .. width do input.AddVerticesAndEdgeRange(batch i (i+1)) |> ignore
     input.AddVerticesAndEdgeRange(batch 0 0) |> ignore
     
     let ast = Yard.Generators.GLL.AbstractParser.buildAst getParserSource input
     ast.CountCounters |> printfn "%A"
     ()

    let layers = new ResizeArray<_>()
    let inputLayer = 0
    let run inputSeq =
       1 

do 
    let start = System.DateTime.Now
    let t = new SNN(@"C:\gsv\projects\YC\YaccConstructor\ttt",180)
    printfn "%A" <| System.DateTime.Now - start