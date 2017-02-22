module YC.Bio.RNA.Search.Configuration

open Argu
open QuickGraph
open YC.Bio.GraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParser
open System
open System.Collections.Generic
open Microsoft.FSharp.Collections

type CLIArguments = 
    | [<NoAppSettings; Mandatory; AltCommandLine("-i")>] Input of string
    | Agents of int
    | TmpDir of string
    | [<AltCommandLine("-fb")>]FinalBias of float
    | [<AltCommandLine("-hb")>]HeadsBias of float
    | [<AltCommandLine("-mb")>]MiddlesBias of float
    | [<AltCommandLine("-tb")>]TailsBias of float
    | [<AltCommandLine("-hmb")>]HeadsMiddlesBias of float
    interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | Input _ -> "Specify a graph for processing."
            | Agents _ -> "Specify a number of agents for parallel processing."
            | TmpDir _ -> "Specify a directory for temp output files."
            | FinalBias _ -> "Specify a hight limit of bias for final filtering."
            | HeadsBias _ -> "Specify a hight limit of bias for heads filtering."
            | TailsBias _ -> "Specify a hight limit of bias for tailss filtering."
            | HeadsMiddlesBias _ -> "Specify a hight limit of bias for concatenated heads-middles  filtering."
            | MiddlesBias _ -> "Specify a hight limit of bias for middless filtering."

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

    let finalBias = args.GetResult(<@ FinalBias @>, defaultValue = 4.0)
    let headsBias = args.GetResult(<@ HeadsBias @>, defaultValue = 0.5)
    let middlesBias = args.GetResult(<@ MiddlesBias @>, defaultValue = 4.0) 
    let tailsBias = args.GetResult(<@ TailsBias @>, defaultValue = 1.0)
    let headMiddleBias = args.GetResult(<@ HeadsMiddlesBias @>, defaultValue = 4.0)
    let tmpDir = args.GetResult(<@ TmpDir @>, defaultValue = "BioSearchOut")
    let inputGraphPath = 
        args.GetResult <@ Input @> 
        |> (fun s -> 
        System.IO.Path.Combine(System.IO.Path.GetDirectoryName s, System.IO.Path.GetFileNameWithoutExtension s))
    
    let grammarsDir = @"../../../src/YC.GrammarZOO/Bio/16s/"

    let fileInTmpDir file = System.IO.Path.Combine(tmpDir, file)

    let startTime = ref System.DateTime.Now

    let timing = new ResizeArray<_>()

    let lap name =
        let last = 
            if timing.Count = 0
            then !startTime
            else 
                let (name,delta,abs) = timing.[timing.Count - 1]
                abs
        let cur = System.DateTime.Now
        printfn "Step: %A. Duration: %A. Finished: %A" name (cur - last) cur
        timing.Add((name, cur - last, cur))

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
        new SearchConfig(parserSourceMiddle, mkTokenizer parserSourceMiddle.StringToToken, 360, 390, fileInTmpDir "R16S_1_18_result.fa")

    let R16STailSearchConfig = 
        new SearchConfig(
            parserSourceTail
            , mkTokenizer parserSourceTail.StringToToken
            , //h29
              //390, 420
              //h30
              270, 300
              //any 80 + h30
              //350, 380
              (*460, 490,*)
            , fileInTmpDir "R16S_tail_result.fa")

    let R16SMiddleSearchConfig = 
        //D = 30
        new SearchConfig(parserSourceMiddle, mkTokenizer parserSourceMiddle.StringToToken, 300, 370, fileInTmpDir "R16S_19_27_result.fa")

    do
        if System.IO.Directory.Exists tmpDir |> not
        then System.IO.Directory.CreateDirectory tmpDir |> ignore
        startTime := System.DateTime.Now
    
    member val FinalBias = finalBias with get
    member val HedsBias = headsBias with get
    member val MiddlesBias = middlesBias with get
    member val TailsBias = tailsBias with get
    member val HeadsMiddlesBias = headMiddleBias with get
    member val TempDirectory = tmpDir with get
    member val AgentsCount = agentsCount with get
    member val InputGraphPath = inputGraphPath with get
    member val HeadSearchConfig = R16SHeadSearchConfig with get
    member val MiddleSearchConfig = R16SMiddleSearchConfig with get
    member val TailSearchConfig = R16STailSearchConfig with get
    member val FileForHeadAndMiddles = fileInTmpDir "HeadMiddle.fa" with get
    member val FileForFull = fileInTmpDir "Full.fa" with get
    member val FileForScoredFull = fileInTmpDir "SFull.fa" with get
    member this.GetTiming () = timing
    member this.PrintTiming () =
        timing |> ResizeArray.iter (fun (name,delta,abs) -> printfn "Step: %A. Duartion: %A. Finished: %A" name delta abs)
        try
            let (_,_,startTime) = timing.[0]
            printfn "Total: %A" (System.DateTime.Now - startTime)
        with _ -> ()
    member this.Lap name = lap name

    member val OriginalEdges:array<TaggedEdge<int<vNumInOriginalGraph>,BioGraphEdgeLbl<char>>> = [||] with get, set
    member val LongEdges:array<TaggedEdge<int<vNumInOriginalGraph>,BioGraphEdgeLbl<char>>> = [||] with get, set

type msg<'data> = 
    | Data of 'data
    | Die of AsyncReplyChannel<unit>