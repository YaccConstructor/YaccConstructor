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
            let res = new HashSet<_>()
            roots
            |> Array.map (fun x -> allPathsForRoot x parserSource.IntToString)
            |> Array.iter (fun s -> res.UnionWith s)
            res

        let croppedRes = 
            result 
            |> Seq.map (fun s -> 
                let p = s.LastIndexOf 'A'
                let p2 = s.IndexOf(' ', p)
                s.Substring(0,p2).Trim())

        let filteredRes = croppedRes |> Seq.filter (fun x -> not <| x.Contains "RT")

        System.IO.File.WriteAllLines(outputFile, croppedRes)

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
    let graphFile = argv.[0]
    /// CALLS, LOCKS, ASSERTS
    
    let parserSource, inputGraph = loadInput graphFile   

    printGraph inputGraph "inputGraph.dot"
    let outputFile = argv.[1]
    let start = System.DateTime.Now
    printAllPaths parserSource inputGraph outputFile
    //printAllBadAsserts parserSource inputGraph outputFile
    printfn "Processing time: %A" (System.DateTime.Now - start)
    0 // return an integer exit code