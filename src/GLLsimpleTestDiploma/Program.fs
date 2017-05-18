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
    System.IO.File.AppendAllText(sprintf "../../results/%s.txt" what, sprintf " (%i,%s)" n count)
let test (parser : ParserSourceGLL) inputString = 
    let arr = inputString |> Array.map (fun (x : string) -> parser.StringToToken x)
    let input  = new LinearInput(arr)
    buildAstTest parser input
   
let cHash a b = 
    let mutable h = 23
    h <- h * 31 + a
    h <- h * 31 + b
    h

let genInput length =
    [|for i in 1..length do yield "A"|]
[<EntryPoint>]
let main argv = 
//    for num in [101..119] do
//        let i = num
    for parser in [GLL.longK_noEBNF.parserSource; GLL.longK.parserSource] do
        for num in [1..15] do
            let i = num * 30
            GC.Collect()
            GC.WaitForPendingFinalizers()
            GC.Collect()
            let startTime = System.DateTime.Now
            let tree, gssEdgesCount, gssNodesCount, sppfNodesCount, totalBytesOfMemoryUsed, descr = 
                test (parser) (genInput i)
            let duration = System.DateTime.Now - startTime
            //tree.AstToDot(parser.IntToString) (sprintf "res%i.dot" i)
            printRes "descr" (descr.ToString()) i
            printRes "GSSEdges" (gssEdgesCount.ToString()) i
            printRes "GSSNodes" (gssNodesCount.ToString()) i
            printRes "SPPF nodes" (sppfNodesCount.ToString()) i
            printRes "time" (duration.ToString()) i
            printRes "memory" ((totalBytesOfMemoryUsed / (int64 (1024 * 1024))).ToString()) i
            printfn "Time of %i: %A" i duration

    0 // return an integer exit code
