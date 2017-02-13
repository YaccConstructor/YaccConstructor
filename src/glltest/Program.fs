open AbstractAnalysis.Common
open System
open System.Collections.Generic
open Yard.Generators.GLL.ParserCommon
open QuickGraph

let dirFiles = @"../../"
let dir = @"../../"

let getTokens path =
    System.IO.File.ReadAllText(dir + path)
        .Split([|' '|])
        |> Array.filter ((<>) "")

let getLinearInput path (stringToToken : string -> int<token>) = 
    new LinearInput(
            getTokens path
            |> Array.map stringToToken
            )

let isParsed parserSource input = 
    Yard.Generators.GLL.AbstractParserWithoutTree.isParsed parserSource input
     
let getParserSource grammarFile =    
    YaccConstructor.API.generate (dirFiles + grammarFile)
                                 "YardFrontend" "GLLGenerator" 
                                 None
                                 ["ExpandMeta"]
                                 [] :?> ParserSourceGLL

let runTest grammarFile inputFile =
    let parser = getParserSource grammarFile
    let input  = getLinearInput inputFile parser.StringToToken
    let res = isParsed parser input
    printfn "%A" res

[<EntryPoint>]
let main argv =
    runTest "List.yrd" "List.txt"

    0
 