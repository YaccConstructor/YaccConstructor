open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common
open Yard.Frontends.YardFrontend
open YC.API
open AbstractParser

let loadGraph graphFile tokenizer =
    let data = System.IO.File.ReadAllLines(graphFile)
    let startVrts = data.[0].Split ' ' |> Array.map int
    let edges = 
        data.[1..] |> Array.map (fun s -> s.Split ' ' |> fun a -> new ParserEdge<_>(int a.[0], int a.[2], a.[1]))
    let graph = new SimpleInputGraph<_>(startVrts, [||], tokenizer)
    graph.AddVerticesAndEdgeRange edges |> ignore
    graph

let loadGrammar grammarFile = ""

[<EntryPoint>]
let main argv =
    let grpahFile = argv.[0] 
    let grammarFile = argv.[1]
    
    let grammar = loadGrammar grammarFile    

    let parserSource =
        let fe = new YardFrontend()
        let gen = new GLL()
        GenerateFromStrToObj grammar fe gen None Seq.empty [||] :?> ParserSourceGLL
    
    let tokenizer str =
        str |> parserSource.StringToToken |> int

    let inputGraph = loadGraph grpahFile tokenizer

    let ranges = getAllRangesForStartState parserSource inputGraph

    let outputFile = argv.[2]
    
    0 // return an integer exit code
