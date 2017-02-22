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

let getParserSource grammarFile =    
    YaccConstructor.API.generate (grammarsDir + grammarFile)
                                 "YardFrontend" "GLLGenerator" 
                                 None
                                 []
                                 [] :?> ParserSourceGLL

let FSA_R16S_1_18_SearchConfig = 
    let parserSource = GLL.R16S_1_18_noEBNF.parserSource//getParserSource "R16S_1_18.yrd"
    let tokenizer =
        (fun x -> 
            match Char.ToUpper(x).ToString() with              
            | "U" | "T" -> "U"
            | x -> x)
        >> (parserSource.StringToToken)
    new SearchConfig(parserSource, tokenizer, 535, 545, 20, parserSource.IntToString, 
                     "R16S_1_18_result.fa")


let FSA_R16S_19_27_SearchConfig = 
    let parserSource = getParserSource "R16S_19_27.yrd"
    let tokenizer =
        (fun x -> 
            match Char.ToUpper(x).ToString() with              
            | "U" | "T" -> "U"
            | x -> x)
        >> (parserSource.StringToToken)
    new SearchConfig(parserSource, tokenizer, 318, 370, 0, parserSource.IntToString, 
                     "R16S_19_27_result.fa")
