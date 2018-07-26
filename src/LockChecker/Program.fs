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
open System

let parseGraph parserSource inputGraph =
    getAllSPPFRootsAsINodes parserSource inputGraph

let printAllPaths (roots: INode []) (parserSource: ParserSourceGLL) (outputStream: TextWriter) = 
    if roots.Length < 1
    then 
        printfn "doesn't parsed"
        outputStream.WriteLine ""
    else
        let result = 
            let res = new HashSet<_>()
            roots
            |> Array.map (fun x -> allPathsForRoot x parserSource.IntToString)
            |> Array.iter (fun s -> res.UnionWith s)
            res

        result |> Seq.iter (outputStream.WriteLine)

let printAllBadAsserts (roots: INode []) (parserSource: ParserSourceGLL) outputFile = 
    if roots.Length < 1
    then 
        printfn "doesn't parsed"
        System.IO.File.WriteAllLines(outputFile, [""])
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
    printStages: bool;
    useStdin: bool;
    useStdout: bool;
    pathsOutput: string;
    drawGraph: bool;
    graphOutput: string}
 
let startExecution options = 
    let log message =
        if options.verbose then
            printfn "%s" message
     
    let stage name = 
        if options.printStages then
            printfn "stage: %s" name
    
    let inputStream = 
        if (options.useStdin) then
            Console.In
        else 
            new StreamReader (options.graphFile) :> TextReader

    let parserSource, inputGraph = loadInput inputStream log stage

    (*if options.drawGraph then
        printGraph inputGraph options.graphOutput*)

    let start = System.DateTime.Now
    stage "Parsing"

    let roots = parseGraph parserSource inputGraph

    log (sprintf "Parsing time: %A" (System.DateTime.Now - start))

    let start = System.DateTime.Now
    stage "Processing"

    let outputStream =
        if options.useStdout then
            Console.Out
        else
            new StreamWriter (options.pathsOutput) :> TextWriter

    printAllPaths roots parserSource outputStream
    outputStream.Close()

    log (sprintf "Processing time: %A" (System.DateTime.Now - start))

type CLIArguments =
    | [<Unique; AltCommandLine("-v")>] Verbose 
    | Output_Path of path: string
    | Draw_Graph of path: string
    | Print_Stages
    | [<MainCommand; Last>] Graph_File of path:string
with
    interface IArgParserTemplate with   
        member s.Usage =
            match s with
            | Verbose -> "Print additional information, especially time measurements"
            | Output_Path path -> "Specify path to file where results should be stored"
            | Draw_Graph path -> "Print source graph in the .dot format"
            | Print_Stages -> "Print stage name when each of them starts"
            | Graph_File path -> "Path to graph that should be parsed"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "LockChecker")

    try
        let results = parser.Parse argv

        let options = 
            {
                verbose = results.Contains Verbose
                useStdin = not (results.Contains Graph_File)
                useStdout = not (results.Contains Output_Path)
                printStages = results.Contains Print_Stages
                drawGraph = results.Contains Draw_Graph
                graphFile = results.GetResult (Graph_File, defaultValue = "")
                pathsOutput = results.GetResult (Output_Path, defaultValue = "")
                graphOutput = results.GetResult (Draw_Graph, defaultValue = "")
            }

        startExecution options
    with e ->
        printfn "%s" e.Message
    0