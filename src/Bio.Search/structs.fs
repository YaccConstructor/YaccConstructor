module YC.Bio.RNA.Search.Structs

open Argu
open QuickGraph
open YC.Bio.GraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParserWithoutTree

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
    val NumToString : int -> string
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
    let tokenizer ch = 
        match ch with
        | 'A' | 'a' -> GLL.R16S_1_18.A()
        | 'U' | 'u' | 'T' | 't' -> GLL.R16S_1_18.U()
        | 'C' | 'c' -> GLL.R16S_1_18.C()
        | 'G' | 'g' -> GLL.R16S_1_18.G()
        | _ -> GLL.R16S_1_18.U()
        |> GLL.R16S_1_18.tokenToNumber 
        |> (fun x -> x * 1<token>)
    new SearchConfig(GLL.R16S_1_18.parserSource, tokenizer, 535, 545, 20, GLL.R16S_1_18.stateToNontermName, 
                     "R16S_1_18_result.fa")

let FSA_R16S_19_27_SearchConfig = 
    let tokenizer ch = 
        match ch with
        | 'A' | 'a' -> GLL.R16S_19_27.A()
        | 'U' | 'u' | 'T' | 't' -> GLL.R16S_19_27.U()
        | 'C' | 'c' -> GLL.R16S_19_27.C()
        | 'G' | 'g' -> GLL.R16S_19_27.G()
        | _ -> GLL.R16S_19_27.U()
        |> GLL.R16S_19_27.tokenToNumber
        |> (fun x -> x * 1<token>)
    new SearchConfig(GLL.R16S_19_27.parserSource, tokenizer, 318, 370, 0, GLL.R16S_19_27.stateToNontermName, 
                     "R16S_19_27_result.fa")
