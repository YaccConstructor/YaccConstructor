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

let test (parser : ParserSourceGLL) inputString = 
    let arr = inputString |> Array.map (fun (x : string) -> parser.StringToToken x)
    let input  = new LinearInput(arr)
    let tree = buildAst parser input
    tree.AstToDot(parser.IntToString) (sprintf "res%i.dot" inputString.Length)
    ()

let genInput length =
    [|for i in 1..length do yield "A"|]
[<EntryPoint>]
let main argv = 
    for i in [10;20;30;40;50;60] do
        let startTime = System.DateTime.Now
        test (GLL.longK.parserSource) (genInput i)
        let duration1 = System.DateTime.Now - startTime
        printfn "Time of %i: %A" i duration1

    0 // return an integer exit code
