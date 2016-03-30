module YC.Bio.RNA.Search

open Argu

open YC.BIO.BioGraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParserWithoutTree

type CLIArguments =
    | [<NoAppSettings>][<Mandatory>][<AltCommandLine("-i")>] Input of string
    | Agents of int
    
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "Specify a graph for processing." 
            | Agents _ -> "Specify a number of agents for parallel processing." 

type msg =
    | Data of int*BioParserInputGraph    
    | Die of AsyncReplyChannel<unit>

let filterRnaParsingResult lengthLimit res  =
    match res:ParseResult<ResultStruct> with
    | Success ast -> 
        failwith "Result is success but it is unrxpectrd success"
    | Success1 x ->
        let ranges = new ResizeArray<_>()
        let curLeft = ref 0
        let curRight = ref 0                  
        let x = 
            let onOneEdg, other =
                x
                |> Array.filter (fun s -> s.rpos - s.lpos >= lengthLimit || s.le <> s.re)
                |> Array.partition (fun s -> s.le = s.re)
                    
            let curEdg = ref 0
            //printfn ""
            onOneEdg
            |> Array.iter(fun s ->
                curEdg := s.le
                if !curRight < s.lpos
                then 
                    ranges.Add ((s.le,!curLeft),(s.re,!curRight))
                    curLeft := s.lpos
                    curRight := s.rpos                        
                else
                    curLeft := min !curLeft s.lpos
                    curRight := max !curRight s.rpos
                    )
            ranges.Add((!curEdg,!curLeft),(!curEdg,!curRight))
            other
            |> Seq.groupBy(fun s -> s.le,s.re)
            |> Array.ofSeq
            |> Array.map snd
            |> Array.iter(fun s ->
                let s = s |> Array.ofSeq
                let left = s |> Array.minBy (fun s -> s.lpos)
                let right = s |> Array.maxBy (fun s -> s.rpos)                        
                ranges.Add((left.le,left.lpos),(right.re,right.rpos)))        
        ranges     
        
    | Error e -> 
        failwith "Input parsing failed: %A" e
        


let search graphs agentsCount =
    let start = System.DateTime.Now
    let processRanges (ranges:ResizeArray<_>) =
        ranges.Count |> printfn "Total ranges: %A"
    let agent name  =
        MailboxProcessor.Start(fun inbox ->
            let rec loop n =
                async {
                        let! msg = inbox.Receive()
                        match msg with
                        | Data (i,graph) ->
                            printfn "%A: %A" name i
                            try
                                GLL.tRNA.buildAbstract graph 4                                
                                |> filterRnaParsingResult 60
                                |> processRanges
                            with
                            | e -> printfn "ERROR! %A" e.Message
                            return! loop n         
                        | Die ch -> ch.Reply()
                        }
            loop 0)

    let agents = Array.init agentsCount (fun i -> agent (sprintf "searchAgent%A" i))
    graphs
    |> Array.ofSeq  
    |> fun a -> a.[10000..10100]
    |> Array.iteri 
        (fun i graph -> 
            Data (i, graph) 
            |> agents.[i % agentsCount].Post
        )
    agents |> Array.iter (fun a -> a.PostAndReply Die)
    printfn "Total time = %A" (System.DateTime.Now - start)
    0

let searchTRNA path agentsCount =
    let lengthLimit = 120

    let getSmb =
        let cnt = ref 0
        fun ch ->
            let i = incr cnt; !cnt 
            match ch with
            | 'A' -> GLL.tRNA.A i                
            | 'U' -> GLL.tRNA.U i
            | 'T' -> GLL.tRNA.U i
            | 'C' -> GLL.tRNA.C i
            | 'G' -> GLL.tRNA.G i                
            | x ->   failwithf "Strange symbol in input: %A" x
            |> GLL.tRNA.tokenToNumber

    let graphs, longEdges = loadGraphFormFileToBioParserInputGraph path lengthLimit getSmb (GLL.tRNA.RNGLR_EOF 0)
    search graphs agentsCount
    |> printfn "%A"
    ()

[<EntryPoint>]
let main argv = 
    let parser = ArgumentParser.Create<CLIArguments>()
    let args = parser.Parse argv
    let appSetting = parser.ParseAppSettings (System.Reflection.Assembly.GetExecutingAssembly())
    let agentsCount =
        args.GetResult(<@Agents@>, defaultValue = appSetting.GetResult(<@Agents@>, defaultValue = 1))        
    let inputGraphPath = 
        args.GetResult <@Input@>
        |> (fun s ->
                System.IO.Path.Combine(System.IO.Path.GetDirectoryName s, System.IO.Path.GetFileNameWithoutExtension s))
    searchTRNA inputGraphPath agentsCount
    0
