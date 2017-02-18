module YC.Bio.RNA.Search.Structs

open Argu
open QuickGraph
open YC.Bio.GraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParser
open System
open System.Collections.Generic

let grammarsDir = @"../../../src/YC.GrammarZOO/Bio/16s/"
//open Yard.Generators.GLL.AbstractParserWithoutTree
type WhatShouldISearch = 
    | R16S_1_18
    | R16S_19_27

type CLIArguments = 
    | [<NoAppSettings; Mandatory; AltCommandLine("-i")>] Input of string
    | Agents of int
    | TmpDir of string
    interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | Input _ -> "Specify a graph for processing."
            | Agents _ -> "Specify a number of agents for parallel processing."
            | TmpDir _ -> "Specify a directory for temp output files."

type msg = 
    | Data of int * EdgeCompressedGraphInput
    | Die of AsyncReplyChannel<unit>

[<Struct>]
type SearchConfig = 
    val ParserSource : ParserSourceGLL
    val Tokenizer : char -> int<token>
    val HighLengthLimit : int
    val LowLengthLimit : int
    val LengthOfBeinning : int
    val OutFileName : string
    new(parserSource, getSmb, lowLengthLimit, highLengthLimit, lengthOfBeinning, outFileName) = 
        { ParserSource = parserSource
          Tokenizer = getSmb
          HighLengthLimit = highLengthLimit
          LowLengthLimit = lowLengthLimit
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

let parserSourceHead = getParserSource "R16S_1_18.yrd"
let parserSourceMiddle = getParserSource "R16S_19_27.yrd"
let parserSourceTail = getParserSource "R16S_1_18.yrd"

let FSA_R16S_1_18_SearchConfig = 
    
    let tokenizer (ch : char) =
        let ch = 
            let ch = Char.ToUpper(ch)
            if ch = 'T'
            then 'U'
            elif Array.contains ch [|'A';'C';'G';'U';|]
            then ch
            else 'G'
        parserSourceHead.StringToToken (ch.ToString())
    new SearchConfig(parserSourceMiddle, tokenizer, 360, 390, 20, "R16S_1_18_result.fa")


let FSA_R16S_tail_SearchConfig = 
    //let parserSource = getParserSource "R16S_1_18.yrd"
    let tokenizer (ch : char) =
        let ch = 
            let ch = Char.ToUpper(ch)
            if ch = 'T'
            then 'U'
            elif Array.contains ch [|'A';'C';'G';'U';|]
            then ch
            else 'G'
        parserSourceMiddle.StringToToken (ch.ToString())
    new SearchConfig(parserSourceMiddle, tokenizer, 360, 390, 20, "R16S_tail_result.fa")


let FSA_R16S_19_27_SearchConfig = 
    //let parserSource = getParserSource "R16S_19_27.yrd"
    let tokenizer (ch : char) =
        let ch = 
            let ch = Char.ToUpper(ch)
            if ch = 'T'
            then 'U'
            elif Array.contains ch [|'A';'C';'G';'U';|]
            then ch
            else 'G'
        parserSourceMiddle.StringToToken(ch.ToString())
    //D = 30
    new SearchConfig(parserSourceMiddle, tokenizer, 300, 370, 0, "R16S_19_27_result.fa")

