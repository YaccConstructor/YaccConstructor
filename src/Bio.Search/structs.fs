module YC.Bio.RNA.Search.Structs

open Argu
open QuickGraph
open YC.Bio.GraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParserWithoutTree
open System
open System.Collections.Generic

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
    | Data of int * AdjacencyGraph<int,TaggedEdge<int,BioGraphEdgeLbl>>
    | Die of AsyncReplyChannel<unit>

[<Struct>]
type SearchConfig = 
    val ParserSource : ParserSourceGLL
    val Tokenizer : char -> int<token>
    val HighLengthLimit : int
    val LowLengthLimit : int
    val NumToString : Dictionary<int<positionInGrammar>,string>
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

let FSA_R16S_1_18_SearchConfig = 
    let tokenizer (ch : char) = 
        GLL.R16S_1_18.stringToNumber.[Char.ToUpper(ch).ToString()]
        |> (fun x -> x * 1<token>)
    new SearchConfig(GLL.R16S_1_18.parserSource, tokenizer, 535, 545, 20, GLL.R16S_1_18.stateToNontermName, 
                     "R16S_1_18_result.fa")

let FSA_R16S_19_27_SearchConfig = 
    let tokenizer ch =
        GLL.R16S_19_27.stringToNumber.[Char.ToUpper(ch).ToString()]
        |> (fun x -> x * 1<token>)
    new SearchConfig(GLL.R16S_19_27.parserSource, tokenizer, 318, 370, 0, GLL.R16S_19_27.stateToNontermName, 
                     "R16S_19_27_result.fa")
