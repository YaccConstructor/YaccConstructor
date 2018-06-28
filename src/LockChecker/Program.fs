open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common
open Yard.Frontends.YardFrontend
open YC.API
open AbstractParser
open System.Collections.Generic

open ResultProcessing
open InputLoading

let printAllPaths parserSource inputGraph outputFile = 
    let roots = getAllSPPFRootsAsINodes parserSource inputGraph
    if roots.Length < 1
    then 
        printfn "doesn't parsed"
    else
        let result = 
            roots
            |> Seq.collect(fun root -> (allPathsForRoot root parserSource.IntToString))
            //|> Array.map(fun x -> System.String.Join("; ", x))
            //|> Array.distinct

        let croppedRes = 
            result 
            |> Seq.map (fun s -> 
                let p = s.LastIndexOf 'A'
                let p2 = s.IndexOf(' ', p)
                s.Substring(0,p2).Trim())
        System.IO.File.WriteAllLines(outputFile, croppedRes)
        //System.IO.File.WriteAllLines(outputFile, result)  

let printAllBadAsserts parserSource inputGraph outputFile = 
    let roots = getAllSPPFRootsAsINodes parserSource inputGraph
    if roots.Length < 1
    then 
        printfn "doesn't parsed"
    else
        let result = 
            roots
            |> Array.collect(fun root -> getBadAsserts root parserSource.IntToString)
            |> Array.map(fun x -> System.String.Join("; ", x))
            |> Array.distinct

        System.IO.File.WriteAllLines(outputFile, result)

let printGraph (graph : SimpleInputGraph<_>) (file : string) = 
    graph.PrintToDot file id

[<EntryPoint>]
let main argv =
    //let graph = ".\\..\\..\\graph"
    let graphFile = argv.[0]
    /// CALLS, LOCKS, ASSERTS
    
    let parserSource, inputGraph = loadInput graphFile   
    
    //System.IO.File.WriteAllText("resultGrammar.yrd", grammar)

    printGraph inputGraph "inputGraph.dot"
    let outputFile = argv.[1]

    printAllPaths parserSource inputGraph outputFile
    //printAllBadAsserts parserSource inputGraph outputFile
    0 // return an integer exit code