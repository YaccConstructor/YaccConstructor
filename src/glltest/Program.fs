open AbstractAnalysis.Common
open System
open System.Collections.Generic
open GLL.test
open Yard.Generators.GLL.ParserCommon
open QuickGraph

let dirFiles = @"C:/Code/YaccConstructor/src/glltest/"
let dir = @"C:/Code/YaccConstructor/tests/data/GLL/"
let outDir = @"../../../src/GLLParser.SimpleTest/"

let isParsed parserSource input = 
    Yard.Generators.GLL.AbstractParserWithoutTree.isParsed parserSource input

let shouldBeTrue res = 
    if res then failwith "Mismatch"

let getTokens path =
    "NUM STAR NUM"
        .Split([|' '|])
        |> Array.filter ((<>) "")

let getLinearInput path (tokenToInt : Dictionary<string,int>) = 
    new LinearInput(
            getTokens path
            |> Array.map (fun x -> tokenToInt.[x] * 1<token>))
[<EntryPoint>]
let main argv =
    let parser = YaccConstructor.API.generate (dirFiles + "test.yrd")
                                                "YardFrontend" "GLLGenerator" 
                                                (None) 
                                                ["ExpandMeta"]
                                                [] :?> ParserSourceGLL
    let input  = getLinearInput "BBB.txt" GLL.test.stringToNumber
    let res = isParsed parser input

    printf "%A" res

    0
 