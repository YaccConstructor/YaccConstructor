module YC.Bio.RNA.Search.Configuration

open Argu
open QuickGraph
open YC.Bio.GraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParser
open System
open System.Collections.Generic

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

[<Struct>]
type SearchConfig = 
    val ParserSource : ParserSourceGLL
    val Tokenizer : char -> int<token>
    val HighLengthLimit : int
    val LowLengthLimit : int
    val OutFileName : string
    new(parserSource, getSmb, lowLengthLimit, highLengthLimit, outFileName) = 
        { ParserSource = parserSource
          Tokenizer = getSmb
          HighLengthLimit = highLengthLimit
          LowLengthLimit = lowLengthLimit
          OutFileName = outFileName }


type Config (argv) =
    let argParser = ArgumentParser.Create<CLIArguments>()
    let args = argParser.Parse argv
    let agentsCount = args.GetResult(<@ Agents @>, defaultValue = 1)
    let tmpDir = args.GetResult(<@ TmpDir @>, defaultValue = "../../BioSearchOut")
    let inputGraphPath = 
        args.GetResult <@ Input @> 
        |> (fun s -> 
        System.IO.Path.Combine(System.IO.Path.GetDirectoryName s, System.IO.Path.GetFileNameWithoutExtension s))
    
    let grammarsDir = @"../../../src/YC.GrammarZOO/Bio/16s/"

    let getParserSource grammarFile =    
        YaccConstructor.API.generate (System.IO.Path.Combine(grammarsDir, grammarFile))
                                     "YardFrontend" "GLLGenerator" 
                                     None
                                     ["ExpandMeta"]
                                     [] :?> ParserSourceGLL

    let parserSourceHead = getParserSource "R16S_1_18.yrd"
    let parserSourceMiddle = getParserSource "R16S_19_27.yrd"
    let parserSourceTail = getParserSource "R16S_tails.yrd"
    
    let mkTokenizer strToToken =
        fun (ch:char) ->
            let ch = 
                let ch = Char.ToUpper(ch)
                if ch = 'T'
                then 'U'
                elif Array.contains ch [|'A';'C';'G';'U';|]
                then ch
                else 'G'
            strToToken (ch.ToString())

    let R16SHeadSearchConfig = 
        new SearchConfig(parserSourceMiddle, mkTokenizer parserSourceMiddle.StringToToken, 360, 390, "R16S_1_18_result.fa")

    let R16STailSearchConfig = 
        new SearchConfig(parserSourceTail, mkTokenizer parserSourceTail.StringToToken , 460, 490, "R16S_tail_result.fa")

    let R16SMiddleSearchConfig = 
        //D = 30
        new SearchConfig(parserSourceMiddle, mkTokenizer parserSourceMiddle.StringToToken, 300, 370, "R16S_19_27_result.fa")

    do
        if System.IO.Directory.Exists tmpDir |> not
        then System.IO.Directory.CreateDirectory tmpDir |> ignore

    member val TempDirectory = tmpDir with get
    member val AgentsCount = agentsCount with get
    member val InputGraphPath = inputGraphPath with get
    member val HeadSearchConfig = R16SHeadSearchConfig with get
    member val MiddleSearchConfig = R16SMiddleSearchConfig with get
    member val TailSearchConfig = R16STailSearchConfig with get

    member val OriginalEdges:array<TaggedEdge<int<vNumInOriginalGraph>,BioGraphEdgeLbl<char>>> = [||] with get, set
    member val LongEdges:array<TaggedEdge<int<vNumInOriginalGraph>,BioGraphEdgeLbl<char>>> = [||] with get, set

type msg<'data> = 
    | Data of 'data
    | Die of AsyncReplyChannel<unit>



