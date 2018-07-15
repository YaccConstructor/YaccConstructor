module Lockchecker.Main

open Yard.Generators.GLL
open AbstractAnalysis.Common
open AbstractParser
open System.Collections.Generic
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon

open ResultProcessing
open InputLoading

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
    mutable graphFile: string;
    mutable verbose: bool;
    mutable printPaths: bool;
    mutable pathsOutput: string;
    mutable printAsserts: bool;
    mutable assertsOutput: string;
    mutable drawGraph: bool;
    mutable graphOutput: string}
 
let options = {
    graphFile = "graph";
    verbose = false;
    printPaths = false;
    pathsOutput = "paths";
    printAsserts = false;
    assertsOutput = "asserts";
    drawGraph = false;
    graphOutput = "graph.dot"}
 
let rec parseArgs (args: string list) =
    match args with
    | "-v" :: rest ->
        options.verbose <- true
        parseArgs rest
    | "--print-paths" :: name :: rest ->
        options.printPaths <- true
        options.pathsOutput <- name
        parseArgs rest
    | "--print-asserts" :: name :: rest ->
        options.printAsserts <- true
        options.assertsOutput <- name
        parseArgs rest
    | "--draw-graph" :: name :: rest ->
        options.drawGraph <- true
        options.graphOutput <- name
        parseArgs rest
    | name :: [] ->
        if (name.[0] = '-') then
            failwith ("Invalid option: " + name)
        options.graphFile <- name
    | [] -> ()
    | any :: _ -> failwith ("Invalid option: " + any)

[<EntryPoint>]
let main argv =
    parseArgs (Array.toList argv)

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
    0