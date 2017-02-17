module YC.Bio.RNA.SearchFSA

open QuickGraph
open Argu
open System
open YC.Bio.GraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParser
open YC.Bio.RNA.Search.Structs
open Microsoft.FSharp.Collections
open System.IO
open System.Collections.Generic
open System.Diagnostics

type AssemblyOf16s<'v> (id:int, edges:ResizeArray<TaggedEdge<'v,BioGraphEdgeLbl>>, ?infernalData:YC.Bio.InfernalInteraction.InfernalData, ?head, ?middle, ?tail) =
    member val Id = id with get
    member val InfernalData = infernalData with get, set
    member val Edges = edges with get, set
    member val Middle = middle.IsSome && middle.Value with get, set
    member val Head = head.IsSome && head.Value with get, set
    member val Tail = tail.IsSome && tail.Value with get, set
    member this.Full with get() = this.Head && this.Middle && this.Tail

    member this.ConvertToString (longEdges:array<TaggedEdge<_,BioGraphEdgeLbl>>, searchCfg:SearchConfig) =
        let data =
            let getStr (e:TaggedEdge<_,BioGraphEdgeLbl>) =
                let longE = longEdges |> Array.tryFind (fun e' -> e'.Tag.id = Math.Abs e.Tag.id)
                match longE with
                | None -> e.Tag.str
                | Some e' -> 
                    let bound = 
                        match this.InfernalData with
                        | None -> 800
                        | Some d -> if e.Tag.id < 0  then d.ModelFrom else 1600 - d.ModelTo
                    if e.Tag.id < 0 
                    then e'.Tag.str.[max 0 e'.Tag.str.Length - bound ..]
                    else e'.Tag.str.[0 .. min bound (e'.Tag.str.Length - 1)]
                 
            this.Edges
            |> ResizeArray.map (fun e -> e.Tag.str |> Array.map (fun i -> searchCfg.NumToString.[int i]) |> String.concat "")
            |> String.concat ""

        let metadata =
            edges
            |> ResizeArray.map(fun e -> e.Tag.id |> string )
            |> String.concat "; "
        "> " + string this.Id + " " + metadata + "\n" + data + "\n"
        
let getPaths (graph:AdjacencyGraph<_,TaggedEdge<_,BioGraphEdgeLbl>>) isForward s condToStop maxLength = 
    let rec dfs start curLength = 
        let toReturn = new List<List<TaggedEdge<_,_>>>()
        let edges =
            if isForward
            then graph.OutEdges start
            else graph.Edges |> Seq.filter (fun e -> e.Target = start)
        for edge in edges do
            let newLength = curLength + edge.Tag.length
            if condToStop edge newLength
            then new List<_>( [edge]) |> toReturn.Add
            if newLength < maxLength then 
                let paths = dfs (if isForward then edge.Target else edge.Source) newLength
                paths |> ResizeArray.iter (fun (x : List<_>) -> x.Add edge)
                toReturn.AddRange paths
        toReturn
        
    let paths = dfs s 0 |> ResizeArray.map ResizeArray.rev    
    paths

let filterRnaParsingResult (graph : EdgeCompressedGraphInput) (searchCfg : SearchConfig) res = 
    let result = new ResizeArray<_>()

    res
    |> Array.filter (fun (f,t,l) -> int l >= searchCfg.LowLengthLimit && int l <= searchCfg.HighLengthLimit)
    |> Array.groupBy (fun (f,t,l) -> 
            let start =
                match graph.GetEdgeFromPackedPod f with
                | Some v -> v.Target, Some v
                | None -> graph.VerticesBackMap.[int (graph.GetPosOnEdgeFromPackedPod f)], None
            let _end =
                match graph.GetEdgeFromPackedPod t with
                | Some v -> v.Source, Some v
                | None -> graph.VerticesBackMap.[int (graph.GetPosOnEdgeFromPackedPod t)], None
            start, _end)
    |> Array.iter (fun ((eFrom, eTo), a) ->
        let lengths =
            a |> Array.map (fun (f,t,l) -> 
                let prefixLength = 
                    match snd eFrom with
                    | Some v -> v.Tag.str.Length - (int <| graph.GetPosOnEdgeFromPackedPod f)
                    | _ -> 0
                let suffixLEngth = 
                    match snd eTo with
                    | Some v -> (int <| graph.GetPosOnEdgeFromPackedPod t)
                    | None -> 0
                int l - prefixLength - suffixLEngth)
            |> fun a -> new HashSet<_>(a)

        let midlePaths = 
            let e = fst eTo
            let maxLength = lengths |> Seq.max
            let res = getPaths graph true (fst eFrom) (fun (curE:TaggedEdge<_,_>) curL -> curE.Target = e && lengths.Contains curL) maxLength
            if lengths.Contains 0 then new List<_>() |> res.Add
            res
        
        midlePaths
        |> ResizeArray.map (fun a -> 
            let res = new ResizeArray<_>()
            match snd eFrom with
            | Some v -> res.Add v
            | None -> ()
            res.AddRange a
            match snd eTo with
            | Some v -> res.Add v
            | None -> ()
            res)
        |> fun r -> result.AddRange r
        )

    result

let parsingResultsProcessor (assembliesOf16s:ResizeArray<_>) longEdges =
    let edgesGlobalCounter = ref 0
    MailboxProcessor.Start(fun inbox -> 
        let rec loop n = 
            async { 
                let! msg = inbox.Receive()
                match msg with
                | PData(graph, cfg, res) -> 
                    try 
                        let pathToPrint = filterRnaParsingResult graph cfg res
                        pathToPrint
                        |> ResizeArray.map (fun e -> 
                            incr edgesGlobalCounter
                            let a = new AssemblyOf16s<_>(!edgesGlobalCounter, e, middle = true)
                            assembliesOf16s.Add a
                            a.ConvertToString (longEdges, cfg))
                        |> fun strs -> System.IO.File.AppendAllLines(cfg.OutFileName, strs)
                    with e -> printfn "ERROR in parsing results postprocessing! %A" e.Message
                    return! loop n
                | PDie ch -> ch.Reply()
            }
        loop 0)
    
let searchInBioGraphs (searchCfg : SearchConfig) (graphs : EdgeCompressedGraphInput[]) assembliesOf16s agentsCount longEdges =
    printfn "Total graph to porcess: %A" graphs.Length 
    let start = System.DateTime.Now
    let postprocessor = parsingResultsProcessor assembliesOf16s longEdges
    let agent name = 
        MailboxProcessor.Start(fun inbox -> 
            let rec loop n = 
                async { 
                    let! msg = inbox.Receive()
                    match msg with
                    | Data(i, graph) -> 
                        try 
                            printfn "\nSearch in agent %A. Graph %A." name i
                            printfn "Vertices: %A Edges: %A" graph.VertexCount graph.EdgeCount
                            let parseResult = 
                                getAllRangesForStartStateWithLength searchCfg.ParserSource graph
                                |> Array.ofSeq
                                
                            if parseResult.Length = 0 
                            then failwith "Input parsing failed."
                            else 
                                printfn "SearchWithoutSPPF succeed. Count = %A" parseResult.Length
                                postprocessor.Post(PData(graph, searchCfg, parseResult))

                        with e -> printfn "ERROR in bio graph parsing! %A" e.Message
                        return! loop n
                    | Die ch -> ch.Reply()
                }
            loop 0)
    
    let agents = Array.init agentsCount (sprintf "searchAgent%A" >> agent)
    graphs 
    |> Array.iteri (fun i graph -> 
        let agentId = 
            if agentsCount > 1 && graph.VertexCount < 100
            then 1 + i % (agentsCount - 1)
            else 0
        Data(i, graph) |> agents.[agentId].Post)
    agents |> Array.iter (fun a -> a.PostAndReply Die)
    postprocessor.PostAndReply PDie

    printfn "Total time = %A" (System.DateTime.Now - start)

let printLongEdges path edges = 
    let toPrint = 
        edges
        |> Array.mapi (fun i x -> 
               [| ">Long" + i.ToString()
                  x |])
        |> Array.collect id
    File.AppendAllLines(path, toPrint)

let searchMiddle (searchCfg:SearchConfig) agentsCount assembliesOf16s graphs longEdges = 
    let graphs =
        graphs
        |> Array.filter (fun (g:EdgeCompressedGraphInput) -> g.Edges |> Seq.sumBy (fun e -> e.Tag.str.Length) > int (float searchCfg.LowLengthLimit / 1.8))    
    searchInBioGraphs searchCfg graphs assembliesOf16s agentsCount longEdges

let score (searchCfg : SearchConfig) (assembliesOf16s:ResizeArray<AssemblyOf16s<_>>) =
    let scoredByInfernal = 
        YC.Bio.InfernalInteraction.getScores searchCfg.OutFileName
        |> Array.ofSeq
        |> Array.iter (fun d -> 
            let a = assembliesOf16s |> ResizeArray.tryFind (fun a -> try a.Id = int d.TargetName with _ -> try a.Id = int d.QueryName with _ -> false) 
            match a with
            | Some a -> a.InfernalData <- Some d 
            | None -> printfn "assembly with id = %A not found" d.QueryName)
        assembliesOf16s
        |> ResizeArray.filter (fun a -> a.InfernalData.IsSome && (a.InfernalData.Value.ModelFrom < 3 || a.InfernalData.Value.SeqFrom < 3))
    scoredByInfernal

let searchMain path agentsCount =     
    let assembliesOf16s = new ResizeArray<_>()
    let searchCfg = FSA_R16S_19_27_SearchConfig
    let sourceGraph, graphs, longEdges = loadInitialGraph path searchCfg.HighLengthLimit searchCfg.Tokenizer    
    searchMiddle searchCfg agentsCount assembliesOf16s graphs longEdges
    
    let assembliesOf16s = score searchCfg assembliesOf16s

//    let midleEdgesScordByInfernal = 
//        middleScoredByInfernal
//        |> Array.map (fun d -> 
//            let edsIds = d.DescriptionOfTarget.Value.Split([|' ';';'|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int
//            d, edsIds |> Array.map (fun id -> sourceGraph |> Array.find (fun e -> e.Tag.id = id))
//            )
//    
//    let headMidle = new ResizeArray<_>()
//
//    let assembliesOf16s = 
//        let r = new ResizeArray<_>()
//        midleEdgesScordByInfernal
//        |> Array.iter(fun (d,e) -> new AssemblyOf16s<_>(d,e,middle=true) |> r.Add)
//        r

    assembliesOf16s
    |> ResizeArray.iter (fun a -> 
        if a.InfernalData.Value.ModelFrom < 5 then a.Head <- true
        if a.Edges.[0].Tag.id < 0 then a.Head <- true // First edge is a part of long edges
        if a.InfernalData.Value.ModelTo > 1500 then a.Tail <- true
        if longEdges |> Array.exists( fun e -> e.Tag.id = a.Edges.[a.Edges.Count - 1].Tag.id) then a.Tail <- true
        )
    
    assembliesOf16s |> ResizeArray.map (fun a -> a.ConvertToString (longEdges, searchCfg)) |> (fun f -> System.IO.File.WriteAllLines ("AAAA.fa", f))

    let midleEdgesToProcess = assembliesOf16s |> ResizeArray.filter (fun a -> not a.Full) |> Array.ofSeq

    let g = sourceGraph.ToAdjacencyGraph(true)
    let headsAndTails =
        midleEdgesToProcess
        |> Array.map (fun a -> 
            let headsFinalV = a.Edges.[0].Source
            let headsLengthLim = a.InfernalData.Value.ModelFrom + 10
            let heads = 
                let paths = getPaths g false headsFinalV (fun (curE:TaggedEdge<_,_>) curL -> curL >= headsLengthLim) headsLengthLim
                paths
                |> ResizeArray.concat
                |> Array.ofSeq

            let tailsStartV = a.Edges.[a.Edges.Count - 1].Target
            let tailsLengthLim = 1600 - a.InfernalData.Value.ModelTo 
            let tails = 
                getPaths g true tailsStartV (fun (curE:TaggedEdge<_,_>) curL -> curL >= tailsLengthLim) tailsLengthLim
                |> ResizeArray.concat
                |> Array.ofSeq
            heads, a, tails
            )

    headsAndTails |> Array.iter (fun (h,m,t) -> printfn "%A - %A" h.Length t.Length)

    let edgesForHeadsSearch =
        headsAndTails
        |> Array.collect (fun (h,a,t) -> [|h; [|a.Edges.[0]|]|])
        |> Array.concat
        |> fun a -> new HashSet<_>(a)
//    
//    let edgesForTailsSearch =
//        headsAndTails
//        |> Array.collect (fun (h,(_,m),t) -> [|[|m.[m.Length-1]|];t|])
//        |> Array.concat
//        |> fun a -> new HashSet<_>(a)
//
    let graphs = splitToConnectedSubgraphs edgesForHeadsSearch
    let assembliesOf16sHeads = new ResizeArray<_>()
    let searchCfg = FSA_R16S_1_18_SearchConfig
    searchInBioGraphs searchCfg graphs assembliesOf16sHeads 1 longEdges
    let assembliesOf16sHeads = score searchCfg assembliesOf16sHeads
    ()

let printPairs path pairs = 
    let x = pairs |> Array.collect (fun (x, y) -> [| x; y |])
    File.AppendAllLines(path, x)

[<EntryPoint>]
let main argv = 
    let argParser = ArgumentParser.Create<CLIArguments>()
    let args = argParser.Parse argv
    //let appSetting = argParser.ParseConfiguration (ConfigurationReader.FromAppSettingsFile("C:\YCInfernal\src\RNA.Search\App.config"))
    let agentsCount = args.GetResult(<@ Agents @>, defaultValue = 1) //appSetting.GetResult(<@Agents@>, defaultValue = 1))
    let inputGraphPath = 
        args.GetResult <@ Input @> 
        |> (fun s -> 
        System.IO.Path.Combine(System.IO.Path.GetDirectoryName s, System.IO.Path.GetFileNameWithoutExtension s))
    searchMain inputGraphPath agentsCount
    0
