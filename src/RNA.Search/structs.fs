module YC.Bio.RNA.Search.Structs

open Argu
open YC.BIO.BioGraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput

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
    | Data of int * BioParserInputGraph
    | Die of AsyncReplyChannel<unit>

[<Struct>]
type SearchConfig = 
    val SearchWithoutSPPF : BioParserInputGraph -> ParseResult<ResultStruct>
    val Tokenizer : char -> int
    val HighLengthLimit : int
    val LowLengthLimit : int
    val NumToString : int -> string
    val LengthOfBeinning : int
    val OutFileName : string
    new(withoutSppf, getSmb, lowLengthLimit, highLengthLimit, lengthOfBeinning, numToString, outFileName) = 
        { SearchWithoutSPPF = withoutSppf
          Tokenizer = getSmb
          HighLengthLimit = highLengthLimit
          LowLengthLimit = lowLengthLimit
          NumToString = numToString
          LengthOfBeinning = lengthOfBeinning
          OutFileName = outFileName }

let FSA_R16S_1_18_SearchConfig = 
    let tokenizer ch = 
        match ch with
        | 'A' | 'a' -> GLLFSA.R16S_1_18.A()
        | 'U' | 'u' | 'T' | 't' -> GLLFSA.R16S_1_18.U()
        | 'C' | 'c' -> GLLFSA.R16S_1_18.C()
        | 'G' | 'g' -> GLLFSA.R16S_1_18.G()
        | _ -> GLLFSA.R16S_1_18.U()
        |> GLLFSA.R16S_1_18.tokenToNumber
    new SearchConfig(GLLFSA.R16S_1_18.buildAbstract, tokenizer, 535, 545, 20, GLLFSA.R16S_1_18.stateToNontermName, 
                     "R16S_1_18_result.fa")

let FSA_R16S_19_27_SearchConfig = 
    let tokenizer ch = 
        match ch with
        | 'A' | 'a' -> GLLFSA.R16S_19_27.A()
        | 'U' | 'u' | 'T' | 't' -> GLLFSA.R16S_19_27.U()
        | 'C' | 'c' -> GLLFSA.R16S_19_27.C()
        | 'G' | 'g' -> GLLFSA.R16S_19_27.G()
        | _ -> GLLFSA.R16S_19_27.U()
        |> GLLFSA.R16S_19_27.tokenToNumber
    new SearchConfig(GLLFSA.R16S_19_27.buildAbstract, tokenizer, 318, 370, 0, GLLFSA.R16S_19_27.stateToNontermName, 
                     "R16S_19_27_result.fa")
