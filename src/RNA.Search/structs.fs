module YC.Bio.RNA.Search.Structs

open Argu

open YC.BIO.BioGraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput
//open Yard.Generators.GLL.AbstractParserWithoutTree

type WhatShouldISearch =
    | TRNA
    | R16S_H22_H23
    | Shift_problem

type CLIArguments =
    | [<NoAppSettings>][<Mandatory>][<AltCommandLine("-i")>] Input of string
    | Agents of int
    //| WhatShouldISearch of WhatShouldISearch
    
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "Specify a graph for processing." 
            | Agents _ -> "Specify a number of agents for parallel processing." 
            //| 

type msg =
    | Data of int*BioParserInputGraph    
    | Die of AsyncReplyChannel<unit>

[<Struct>]
type SearchConfig =
    val SearchWithoutSPPF: BioParserInputGraph -> (*int -> *)ParseResult<ResultStruct>
    //val SearchWithSPPF: ParserInputGraph -> ParseResult<int>
    val Tokenizer: char -> int
    val HighLengthLimit: int
    val LowLengthLimit: int
    val StartNonterm: int
    val NumToString : int -> string

    new(withoutSppf, (*withSppf,*) getSmb, lowLengthLimit, highLengthLimit, startNonterm, numToString) = 
        {
            SearchWithoutSPPF = withoutSppf
            //SearchWithSPPF = withSppf
            Tokenizer = getSmb
            HighLengthLimit = highLengthLimit
            LowLengthLimit = lowLengthLimit
            StartNonterm = startNonterm
            NumToString = numToString
        }

let FSA_r16s_H22_H23_SearchConfig =

    let getSmb =
        let cnt = ref 0
        fun ch ->
            let i = incr cnt; !cnt 
            match ch with
            | 'A' | 'a' -> GLLFSA.r16s.H22_H23.A ()                
            | 'U' | 'u' 
            | 'T' | 't' -> GLLFSA.r16s.H22_H23.U ()
            | 'C' | 'c' -> GLLFSA.r16s.H22_H23.C ()
            | 'G' | 'g' -> GLLFSA.r16s.H22_H23.G ()
            | _ -> GLLFSA.r16s.H22_H23.U ()
            | x ->   failwithf "Strange symbol in input: %A" x
            (*match ch with
            | 'A' | 'a' -> GLL.r16s.H22_H23.A ()                
            | 'K' | 'k' -> GLL.r16s.H22_H23.K ()
            | 'B' | 'b' -> GLL.r16s.H22_H23.B ()
            | 'C' | 'c' -> GLL.r16s.H22_H23.C ()
            | 'F' | 'f' -> GLL.r16s.H22_H23.F ()
            | 'L' | 'l' -> GLL.r16s.H22_H23.L ()
            | 'M' | 'm' -> GLL.r16s.H22_H23.M ()
            | 'U' | 'u' -> GLL.r16s.H22_H23.U ()
            | 'E' | 'e' -> GLL.r16s.H22_H23.E ()
            | x ->   failwithf "Strange symbol in input: %A" x*)
            |> GLLFSA.r16s.H22_H23.tokenToNumber
    
    new SearchConfig(GLLFSA.r16s.H22_H23.buildAbstract(*, GLL.r16s.H22_H23.buildAbstractAst*), getSmb, 
                        //300, 550, 14, GLL.r16s.H22_H23.numToString)
                     318, 370, 14, GLLFSA.r16s.H22_H23.numToString)
                        //0 , 10, 14, GLL.r16s.H22_H23.numToString)
                        //400, 550, 14, GLL.r16s.H22_H23.numToString)
                        //800, 910, 14, GLL.r16s.H22_H23.numToString)
(*
let r16s_H22_H23_SearchConfig =

    let getSmb =
        let cnt = ref 0
        fun ch ->
            let i = incr cnt; !cnt 
            match ch with
            | 'A' | 'a' -> GLL.r16s.H22_H23.A 1                
            | 'U' | 'u' 
            | 'T' | 't' -> GLL.r16s.H22_H23.U 1
            | 'C' | 'c' -> GLL.r16s.H22_H23.C 1
            | 'G' | 'g' -> GLL.r16s.H22_H23.G 1
            | _ -> GLL.r16s.H22_H23.U 1
            | x ->   failwithf "Strange symbol in input: %A" x
            (*match ch with
            | 'A' | 'a' -> GLL.r16s.H22_H23.A ()                
            | 'K' | 'k' -> GLL.r16s.H22_H23.K ()
            | 'B' | 'b' -> GLL.r16s.H22_H23.B ()
            | 'C' | 'c' -> GLL.r16s.H22_H23.C ()
            | 'F' | 'f' -> GLL.r16s.H22_H23.F ()
            | 'L' | 'l' -> GLL.r16s.H22_H23.L ()
            | 'M' | 'm' -> GLL.r16s.H22_H23.M ()
            | 'U' | 'u' -> GLL.r16s.H22_H23.U ()
            | 'E' | 'e' -> GLL.r16s.H22_H23.E ()
            | x ->   failwithf "Strange symbol in input: %A" x*)
            |> GLL.r16s.H22_H23.tokenToNumber
    
    new SearchConfig(GLL.r16s.H22_H23.buildAbstract(*, GLL.r16s.H22_H23.buildAbstractAst*), getSmb, 
                        //300, 550, 14, GLL.r16s.H22_H23.numToString)
                     318, 370, 14, GLL.r16s.H22_H23.numToString)
                        //0 , 10, 14, GLL.r16s.H22_H23.numToString)
                        //400, 550, 14, GLL.r16s.H22_H23.numToString)
                        //800, 910, 14, GLL.r16s.H22_H23.numToString)*)