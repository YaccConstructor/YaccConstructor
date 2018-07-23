module Lockchecker.Main

open Argu
open Yard.Generators.GLL
open AbstractAnalysis.Common
open AbstractParser
open System.Collections.Generic
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon

open ResultProcessing
open InputLoading
open System.IO

let parseGraph parserSource inputGraph =
    getAllSPPFRootsAsINodes parserSource inputGraph

let printAllPaths (roots: INode []) (parserSource: ParserSourceGLL) outputFile = 
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

        System.IO.File.WriteAllLines(outputFile, result)

let printAllBadAsserts (roots: INode []) (parserSource: ParserSourceGLL) outputFile = 
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

type optionsSet = {
    graphFile: string;
    verbose: bool;
    printPaths: bool;
    pathsOutput: string;
    printAsserts: bool;
    assertsOutput: string;
    drawGraph: bool;
    graphOutput: string}
 
let startExecution options = 
    let log message =
        if options.verbose then
            printfn "%s" message

    let parserSource, inputGraph = loadInput options.graphFile log

    if options.drawGraph then
        printGraph inputGraph options.graphOutput

    let start = System.DateTime.Now

    let roots = parseGraph parserSource inputGraph

    log (sprintf "Parsing time: %A" (System.DateTime.Now - start))

    let start = System.DateTime.Now

    if options.printPaths then
        printAllPaths roots parserSource options.pathsOutput

    if options.printAsserts then
        printAllBadAsserts roots parserSource options.assertsOutput

    log (sprintf "Processing time: %A" (System.DateTime.Now - start))

type CLIArguments =
    | [<Unique; AltCommandLine("-v")>] Verbose 
    | Print_Paths of path:string
    | Print_Asserts of path:string
    | Draw_Graph of path:string
    | [<MainCommand; ExactlyOnce; Last>] Graph_File of path:string
with
    interface IArgParserTemplate with   
        member s.Usage =
            match s with
            | Verbose -> "Print additional information, especially time measurements"
            | Print_Paths path -> "Print all found paths to a specified file"
            | Print_Asserts path -> "Print all found asserts to a specified file"
            | Draw_Graph path -> "Print source graph in the .dot format"
            | Graph_File path -> "Path to graph that should be parsed"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "LockChecker")

    try
        let results = parser.Parse argv

        let options = 
            {
                verbose = results.Contains Verbose
                printPaths = results.Contains Print_Paths
                printAsserts = results.Contains Print_Asserts
                drawGraph = results.Contains Draw_Graph
                graphFile = results.GetResult Graph_File
                pathsOutput = results.GetResult (Print_Paths, defaultValue = "")
                assertsOutput = results.GetResult (Print_Asserts, defaultValue = "")
                graphOutput = results.GetResult (Draw_Graph, defaultValue = "")
            }

        startExecution options
    with e ->
        printfn "%s" e.Message
    0