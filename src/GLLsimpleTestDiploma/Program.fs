// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open AbstractAnalysis.Common

open Yard.Generators.GLL
//open Yard.Generators.GLL.Parser 
open Yard.Generators
open Microsoft.FSharp.Collections

open Yard.Generators.GLL.ParserCommon
open System.Collections.Generic
open Yard.Generators.GLL.AbstractParser
open System

let printRes what count n = 
    System.IO.File.AppendAllText(sprintf "../../results/%s_noEbnf.txt" what, sprintf " (%i,%s)" n count)
let test (parser : ParserSourceGLL) inputString = 
    let arr = inputString |> Array.map (fun (x : string) -> parser.StringToToken x)
    let input  = new LinearInput(arr)
    buildAstTest parser input
    //tree.AstToDot(parser.IntToString) (sprintf "res%i.dot" inputString.Length)

let genInput length =
    [|for i in 1..length do yield "A"|]
[<EntryPoint>]
let main argv = 
//    for num in [101..119] do
//        let i = num
    for parser in [GLL.longK_noEBNF.parserSource; GLL.longK.parserSource] do
        for num in [1..14] do
            let i = num * 10
            GC.Collect()
            GC.WaitForPendingFinalizers()
            GC.Collect()
            let startTime = System.DateTime.Now
            let tree, gssEdgesCount, gssNodesCount, sppfNodesCount, totalBytesOfMemoryUsed = 
                test (parser) (genInput i)
            let duration = System.DateTime.Now - startTime
            printRes "GSSEdges" (gssEdgesCount.ToString()) i
            printRes "GSSNodes" (gssNodesCount.ToString()) i
            printRes "SPPF nodes" (sppfNodesCount.ToString()) i
            printRes "time" (duration.ToString()) i
            printRes "memory" ((totalBytesOfMemoryUsed / (int64 (1024 * 1024))).ToString()) i
            printfn "Time of %i: %A" i duration

    0 // return an integer exit code
