open System.IO
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.Common.ASTGLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon
open YC.API
open Yard.Frontends.YardFrontend
open Yard.Generators.GLL
open Yard.Core.Conversions.ExpandMeta

open System.Collections.Generic
open System.Linq


let getParserSource grammarFile conv = 
    let fe = new YardFrontend()
    let gen = new GLL()
    generate ((*grammarsDir + *)grammarFile)
             fe gen 
             None
             conv
             [|""|]
             [] :?> ParserSourceGLL

[<EntryPoint>]
let main argv = 
    let parserSources = 
        let conv = [new ExpandMeta()]
        [|for fileNumber in 1..98 -> getParserSource (sprintf "../../grammars/BaselineDomain-%i.yrd" fileNumber ) conv |]

    
    let inputs =
        [| for fileNumber in 1..98 -> 
            let input = File.ReadAllLines(sprintf "../../data/Observations-%i.txt" fileNumber) |> Array.map (fun (x : string) -> x.Split([| |]).[1])
            [| for ps in parserSources ->
                   let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
                   shuffleInput.AddTokens(input)
                   shuffleInput |] 
        |]
    
    let times = new List<_>()

    for inputN in 0..97 do
        let startTime = System.DateTime.Now
        for grammarN in 0..97 do
            //let iterStartTime = System.DateTime.Now
            try
                let parser = new Parser(parserSources.[grammarN])
                parser.BuildAst inputs.[inputN].[grammarN] |> ignore
            with
                | _ -> printf "err in input %i grammar %i" inputN grammarN
            //tree.AstToDot("qwe.dot")
            //printfn "Iter %i time %A" i (System.DateTime.Now - iterStartTime)
        times.Add((System.DateTime.Now - startTime).TotalMilliseconds)
    
    printfn "Avg time %A" (Seq.average times)

//    let tree = buildAst parserSources.[0] inputs.[0]
//    tree.AstToDot("qwe.dot")
    0 // return an integer exit code
