module YC.Bio.RNA.Search.Structs

open Argu
open QuickGraph
open YC.Bio.GraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParser
open System
open System.Collections.Generic

let grammarsDir = @"../../src/YC.GrammarZOO/Bio/16s/"
//open Yard.Generators.GLL.AbstractParserWithoutTree
type WhatShouldISearch = 
    | R16S_1_18
    | R16S_19_27

type CLIArguments = 
    | [<NoAppSettings; Mandatory; AltCommandLine("-i")>] Input of string
    | Agents of int
    interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | Input _ -> "Specify a graph for processing."
            | Agents _ -> "Specify a number of agents for parallel processing."

type msg = 
    | Data of int * EdgeCompressedGraphInput
    | Die of AsyncReplyChannel<unit>

[<Struct>]
type SearchConfig = 
    val ParserSource : ParserSourceGLL
    val Tokenizer : char -> int<token>
    val HighLengthLimit : int
    val LowLengthLimit : int
    val NumToString : Dictionary<int,string>
    val LengthOfBeinning : int
    val OutFileName : string
    new(parserSource, getSmb, lowLengthLimit, highLengthLimit, lengthOfBeinning, numToString, outFileName) = 
        { ParserSource = parserSource
          Tokenizer = getSmb
          HighLengthLimit = highLengthLimit
          LowLengthLimit = lowLengthLimit
          NumToString = numToString
          LengthOfBeinning = lengthOfBeinning
          OutFileName = outFileName }

type toPostProcessMSG =
    | PData of EdgeCompressedGraphInput * SearchConfig * array<int<positionInInput>*int<positionInInput>*uint16>
    | PDie of AsyncReplyChannel<unit>


let getParserSource grammarFile =    
    YaccConstructor.API.generate (grammarsDir + grammarFile)
                                 "YardFrontend" "GLLGenerator" 
                                 None
                                 ["ExpandMeta"]
                                 [] :?> ParserSourceGLL

let FSA_R16S_1_18_SearchConfig = 
    let parserSource = getParserSource "R16S_1_18.yrd"
    let tokenizer (ch : char) =
        let ch = 
            let ch = Char.ToUpper(ch)
            if ch = 'T'
            then 'U'
            elif Array.contains ch [|'A';'C';'G';'U';|]
            then ch
            else 'G'
        parserSource.StringToToken.[ch.ToString()]
        |> (fun x -> x * 1<token>)
    let nTs = new Dictionary<_,_>()
    parserSource.StringToToken
    |> Seq.iter (fun kvp -> nTs.Add(kvp.Value,kvp.Key))
    new SearchConfig(parserSource, tokenizer, 535, 545, 20, nTs, "R16S_1_18_result.fa")


let FSA_R16S_19_27_SearchConfig = 
    let parserSource = getParserSource "R16S_19_27.yrd"
    let tokenizer (ch : char) =
        let ch = 
            let ch = Char.ToUpper(ch)
            if ch = 'T'
            then 'U'
            elif Array.contains ch [|'A';'C';'G';'U';|]
            then ch
            else 'G'
        parserSource.StringToToken.[ch.ToString()]
        |> (fun x -> x * 1<token>)
    let nTs = new Dictionary<_,_>()
    parserSource.StringToToken
    |> Seq.iter (fun kvp -> nTs.Add(kvp.Value,kvp.Key))
    new SearchConfig(parserSource, tokenizer, 300, 370, 0, nTs, "R16S_19_27_result.fa")

