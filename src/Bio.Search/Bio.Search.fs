module YC.Bio.RNA.SearchFSA

open QuickGraph
open Argu
open System
open YC.Bio.GraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParser
open YC.Bio.RNA.Search.Configuration
open Microsoft.FSharp.Collections
open System.IO
open System.Collections.Generic
open System.Diagnostics

type AssemblyOf16s<'v> (id:int, edges:ResizeArray<TaggedEdge<'v,BioGraphEdgeLbl<char>>>, ?infernalData:YC.Bio.InfernalInteraction.InfernalData, ?head, ?middle, ?tail) =
    member val Id = id with get
    member val InfernalData = infernalData with get, set
    member val Edges = edges with get, set
    member val Middle = middle.IsSome && middle.Value with get, set
    member val Head = head.IsSome && head.Value with get, set
    member val Tail = tail.IsSome && tail.Value with get, set
    member this.Full with get() = this.Head && this.Middle && this.Tail

    member this.EqualsPath (path:ResizeArray<TaggedEdge<'v,BioGraphEdgeLbl<char>>>) =
        path.Count = this.Edges.Count
        && (path |> Seq.compareWith (fun x y -> if x = y then 0 else 1) this.Edges |> ((=)0))

    member this.ConvertToString (longEdges:array<TaggedEdge<_,BioGraphEdgeLbl<_>>>, ?trancate) =
        let data =
            let getStr (e:TaggedEdge<_,BioGraphEdgeLbl<_>>) =
                let longE = longEdges |> Array.tryFind (fun e' -> e'.Tag.id = Math.Abs e.Tag.id)
                match longE with
                | None -> e.Tag.str
                | Some e' -> 
                    let bound = 
                        match this.InfernalData with
                        | None -> 800
                        | Some d -> if e.Tag.id < 0  then d.ModelFrom else 1500 - d.ModelTo
                    if e.Tag.id < 0 
                    then e'.Tag.str.[max 0 e'.Tag.str.Length - bound ..]
                    else e'.Tag.str.[0 .. min bound (e'.Tag.str.Length - 1)]
                 
            this.Edges
            |> ResizeArray.map (fun e -> new String(e.Tag.str))
            |> String.concat ""
            |> fun s ->
                if trancate.IsSome && trancate.Value
                then
                    let startPos = 
                        match this.InfernalData with
                        | Some d -> max 0 (d.SeqFrom - d.ModelFrom)
                        | _ -> 0
                    let lenght =
                        match this.InfernalData with
                        | Some d -> d.SeqTo - startPos
                        | _ -> s.Length - startPos 
                    s.Substring(startPos, lenght)
                else s


        let metadata =
            edges
            |> ResizeArray.map(fun e -> e.Tag.id |> string )
            |> String.concat "; "
        "> " + string this.Id + " " + metadata + "\n" + data + "\n"
        
let getPaths (graph:AdjacencyGraph<_,TaggedEdge<_,BioGraphEdgeLbl<_>>>) isForward s condToStop maxLength = 
    let rec dfs start curLength = 
        let toReturn = new List<List<TaggedEdge<_,_>>>()
        let edges =
            if isForward
            then graph.OutEdges start
            else graph.Edges |> Seq.filter (fun e -> e.Target = start)
        for edge in edges do
            let newLength = curLength + edge.Tag.length
            if condToStop edge newLength
            then new List<_>([edge]) |> toReturn.Add
            if newLength < maxLength then 
                let paths = dfs (if isForward then edge.Target else edge.Source) newLength
                paths |> ResizeArray.iter (fun (x : List<_>) -> x.Add edge)
                toReturn.AddRange 
                    paths
                    //(if toReturn.Count > 500 then (*((*printf "@@@" ; *)paths.ToArray().[0..10])*)[||] else paths.ToArray())
        toReturn
        
    let paths = dfs s 0 |> ResizeArray.map ResizeArray.rev    
    paths

let filterRnaParsingResult (graph : EdgeCompressedGraphInput) (searchCfg : SearchConfig) res = 
    let result = new ResizeArray<_>()

    let x1 = 
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
    x1
    |> Array.iter (fun ((eFrom, eTo), a) ->
        let lengths =
            a 
            |> fun a -> 
                printfn "A Length = %A" a.Length
                let r = new HashSet<_>(a) 
                printfn "R Length = %A" r.Count
                r
            |> Array.ofSeq
            |> Array.map (fun (f,t,l) -> 
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

let parsingResultsProcessor (config:Config) (assembliesOf16s:ResizeArray<_>) =
    let edgesGlobalCounter = ref 0
    MailboxProcessor.Start(fun inbox -> 
        let rec loop n = 
            async { 
                let! msg = inbox.Receive()
                match msg with
                | Data(graph, cfg, res) -> 
                    try 
                        let pathToPrint = filterRnaParsingResult graph cfg res
                        printfn "Path to print: %A. Queue: %A" pathToPrint.Count inbox.CurrentQueueLength
                        pathToPrint
                        |> ResizeArray.map (fun e -> 
                            incr edgesGlobalCounter
                            let a = 
                                new AssemblyOf16s<_>(
                                    !edgesGlobalCounter
                                    , e |> ResizeArray.map (fun e -> config.OriginalEdges |> Array.find (fun e' -> e'.Tag.id = e.Tag.id))
                                    , middle = true)
                            assembliesOf16s.Add a
                            a.ConvertToString (config.LongEdges))
                        |> fun strs -> System.IO.File.AppendAllLines(cfg.OutFileName, strs)
                    with e -> printfn "ERROR in parsing results postprocessing! %A" e.Message
                    return! loop n
                | Die ch -> 
                    printfn "Posprocessor finished!"
                    ch.Reply()
            }
        loop 0)

let GSSedgeCount = ref 0L
let GSSnodeCount = ref 0L
let sppfNodeCount = ref 0L
let MemoryUsed = ref 0L
let descrCount = ref 0L 
    
let printRes what count = 
    System.IO.File.AppendAllText(sprintf "./results.txt" , sprintf "%s : %s\n" what count)

let printResults() =
    printRes "descr" (descrCount.Value.ToString())
    printRes "GSSEdges" (GSSedgeCount.Value.ToString())
    printRes "GSSNodes" (GSSnodeCount.Value.ToString())
    printRes "SPPF nodes" (sppfNodeCount.Value.ToString())
    //printRes "time" (duration.Value.ToString()) i
    printRes "memory" ((MemoryUsed.Value / (1024L * 1024L)).ToString())

let searchInBioGraphs (searchCfg : SearchConfig) (config:Config) (graphs : EdgeCompressedGraphInput[]) assembliesOf16s =
    printfn "Total graph to porcess: %A" graphs.Length 
    let start = System.DateTime.Now
    let postprocessor = parsingResultsProcessor config assembliesOf16s
    let agent name = 
        MailboxProcessor.Start(fun inbox -> 
            let rec loop n = 
                async { 
                    let! msg = inbox.Receive()
                    match msg with
                    | Data(i, graph:EdgeCompressedGraphInput) -> 
                        try 
                            printfn "\nSearch in agent %A. Graph %A." name i
                            printfn "Vertices: %A Edges: %A" graph.VertexCount graph.EdgeCount
                            let (*parseResult*) edgeCount, vertexCount, sppfNodes, totalBytesOfMemoryUsed, descr = 
                                if (true)
                                then
                                    GLL.Parse.test.buildAbstract graph 2
                                else
                                    getAllRangesForStartStateWithLength searchCfg.ParserSource graph
                            
                            GSSedgeCount := !GSSedgeCount + (int64 edgeCount)
                            GSSnodeCount := !GSSnodeCount + (int64 vertexCount)
                            sppfNodeCount := !sppfNodeCount + (int64 sppfNodes)
                            MemoryUsed := !MemoryUsed + totalBytesOfMemoryUsed
                            descrCount := !descrCount + (int64 descr)
                            printfn "Mem: %i" !MemoryUsed
//                            let parseResult = parseResult |> Array.ofSeq                                   
//                            if parseResult.Length = 0 
//                            then failwith "Input parsing failed."
//                            else 
//                                printfn "SearchWithoutSPPF succeed. Count = %A" parseResult.Length
//                                postprocessor.Post(Data(graph, searchCfg, parseResult))

                        with e -> printfn "ERROR in bio graph parsing! %A" e.Message
                        return! loop n
                    | Die ch -> 
                        printfn "Graph parser agent %A finished!" name
                        ch.Reply()
                }
            loop 0)
    
    let agents = Array.init config.AgentsCount (sprintf "searchAgent%A" >> agent)
    let qToProcess = Queue<_>(graphs.[4..] |> Array.mapi (fun i x -> (i, x)))
    while qToProcess.Count > 0 do
        agents
        |> Array.iter (fun a ->
            if a.CurrentQueueLength < 10
            then 
                for i in 0..9 do
                    if qToProcess.Count > 0 
                    then
                        let (i,graph) = qToProcess.Dequeue()
                        Data(i, graph) |> a.Post)            
        
    agents |> Array.iter (fun a -> a.PostAndReply Die)
    postprocessor.PostAndReply Die

    printfn "Total time = %A" (System.DateTime.Now - start)

let score file bias longEdges (assembliesOf16s:ResizeArray<AssemblyOf16s<_>>) =
    assembliesOf16s |> ResizeArray.map (fun a -> a.ConvertToString(longEdges)) |> fun s -> System.IO.File.WriteAllLines(file,s)
    assembliesOf16s |> ResizeArray.iter (fun a -> a.InfernalData <- None)
    let scoredByInfernal = 
        YC.Bio.InfernalInteraction.getScores file
        |> Array.ofSeq
        |> Array.iter (fun d -> 
            let a = assembliesOf16s |> ResizeArray.tryFind (fun a -> try a.Id = int d.TargetName with _ -> try a.Id = int d.QueryName with _ -> false) 
            match a with
            | Some a -> 
                if a.InfernalData.IsNone 
                   || (a.InfernalData.IsSome && a.InfernalData.Value.Bias > d.Bias) 
                then a.InfernalData <- Some d 
            | None -> printfn "assembly with id = %A not found" d.TargetName)
        assembliesOf16s
        |> ResizeArray.filter 
            (fun a -> 
                  a.InfernalData.IsSome 
               //&& (a.InfernalData.Value.ModelFrom < 3 || a.InfernalData.Value.SeqFrom < 3) 
               //&& (a.InfernalData.Value.Bias < bias)
               //&& (a.InfernalData.Value.E_value < 1.0e-100) 
               //&& (a.InfernalData.Value.ModelTo >= 1420 || a.InfernalData.Value.SeqTo >= (a.Edges |> ResizeArray.fold (fun b e -> b + e.Tag.str.Length) 0) - 10 )
            )

    scoredByInfernal |> ResizeArray.map (fun a -> a.ConvertToString(longEdges)) |> fun s -> System.IO.File.WriteAllLines(extendFileName file "filtered", s)
    scoredByInfernal

let mergeAssemblies 
        (left:ResizeArray<AssemblyOf16s<_>>) 
        (right:ResizeArray<AssemblyOf16s<_>>) 
        (result:ResizeArray<AssemblyOf16s<_>>) 
        (gaps:Dictionary<_,ResizeArray<TaggedEdge<_,BioGraphEdgeLbl<_>>>>) 
        newAssenbly =
    let used = new HashSet<_>()
    let rec addTails (h:ResizeArray<TaggedEdge<_,_>>) repeat = 
        let tails =
            right
            |> ResizeArray.filter (fun t -> t.Edges.[0].Source = h.[h.Count - 1].Target || t.Edges.[0] = h.[h.Count - 1])
        if tails.Count > 0
        then
            tails
            |> ResizeArray.iter (fun t ->
                used.Add t |> ignore
                let newPath = new ResizeArray<_>(h)
                newPath.AddRange (if t.Edges.[0] = h.[h.Count - 1] then t.Edges.ToArray().[1..] else t.Edges.ToArray())
                result.Add(newAssenbly newPath))
        else
            let flg, gag = gaps.TryGetValue h.[h.Count - 1].Target
            if flg
            then  
                let newHead = new ResizeArray<_>(h)
                newHead.AddRange gag
                addTails newHead false
            else result.Add(newAssenbly h)
    left
    |> ResizeArray.iter (fun h -> addTails h.Edges true )
    right |> ResizeArray.iter (fun a -> if used.Contains a |> not then result.Add a)
    let copy = new ResizeArray<AssemblyOf16s<_>>()
    result
    |> ResizeArray.iter (fun a -> 
        if copy.Exists(fun b -> a.EqualsPath b.Edges) |> not
        then copy.Add a)
    result.Clear()
    result.AddRange copy

let printLongEdges path edges = 
    let toPrint = 
        edges
        |> Array.mapi (fun i x -> [| ">" + i.ToString(); x |])
        |> Array.collect id
    File.AppendAllLines(path, toPrint)

let searchMain (config:Config) =     
    let assembliesOf16s = new ResizeArray<_>()
    let sourceGraph, (graphs, edges), longEdges = loadInitialGraph config.InputGraphPath config.MiddleSearchConfig.HighLengthLimit GLL.Parse.test.tokenizer//config.MiddleSearchConfig.Tokenizer    
    config.OriginalEdges <- sourceGraph
    config.LongEdges <- longEdges
    let graphs =
        graphs
        |> Array.filter (fun (g:EdgeCompressedGraphInput) -> g.Edges |> Seq.sumBy (fun e -> e.Tag.str.Length) > int (float config.MiddleSearchConfig.LowLengthLimit / 1.8))
        //|> Array.partition (fun (g:EdgeCompressedGraphInput) -> g.EdgeCount > 1)

    config.Lap "Data preparing"

    searchInBioGraphs config.MiddleSearchConfig config graphs assembliesOf16s 
    
    printResults()

    config.Lap "Middles parsing"

    let globalResultCount = ref assembliesOf16s.Count

    let assembliesOf16s = score config.MiddleSearchConfig.OutFileName config.MiddlesBias longEdges assembliesOf16s

    config.Lap "Middles scoring with Infernal"

    let assembiesOfLongEdges = 
        Array.concat [|longEdges; edges|]
        |> Array.collect (fun e -> 
            e.Tag.str
            |> Array.split 2000
            |> Array.map (fun a -> new TaggedEdge<_,_>(-1, -1, new BioGraphEdgeLbl<_>(a, e.Tag.length, e.Tag.id, e.Tag.sourceStartPos)))
            |> Array.map (fun a -> incr globalResultCount; new AssemblyOf16s<_>(!globalResultCount, new ResizeArray<_>([a]) ,head=true,middle=true,tail=true))
            )
        |> ResizeArray<_>

    let assembiesOfLongEdges = score config.FileForLongEdges config.MiddlesBias [||] assembiesOfLongEdges

    assembliesOf16s
    |> ResizeArray.iter (fun a -> 
        if a.InfernalData.Value.ModelFrom < 5 then a.Head <- true
        if a.Edges.[0].Tag.id < 0 then a.Head <- true // First edge is a part of long edges
        if a.InfernalData.Value.ModelTo >= 1420 then a.Tail <- true
        if longEdges |> Array.exists (fun e -> e.Tag.id = a.Edges.[a.Edges.Count - 1].Tag.id) then a.Tail <- true
        )
    
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
                |> Array.ofSeq

            let tailsStartV = a.Edges.[a.Edges.Count - 1].Target
            let tailsLengthLim = 1500 - a.InfernalData.Value.ModelTo 
            let tails = 
                getPaths g true tailsStartV (fun (curE:TaggedEdge<_,_>) curL -> curL >= tailsLengthLim) tailsLengthLim
                |> Array.ofSeq
            heads, a, tails
            )

    let searchCfg = config.HeadSearchConfig

    let assembliesOf16sHeads = new ResizeArray<AssemblyOf16s<_>>()    

    let tailsCount = ref 0

    headsAndTails 
    |> Array.iter (fun (h,_,t) -> 
        h |> Array.iter (fun h ->
            let h = ResizeArray.rev h
            if h.Count > 0 && assembliesOf16sHeads |> ResizeArray.exists(fun a -> a.EqualsPath h) |> not
            then assembliesOf16sHeads.Add (new AssemblyOf16s<_>(!globalResultCount, h, head = true))
            tailsCount := !tailsCount + t.Length
            incr globalResultCount)
        )

    config.Lap (sprintf "Heads and tails preparing. Tails %A" !tailsCount)

    let assembliesOf16sHeads = score searchCfg.OutFileName config.HedsBias longEdges assembliesOf16sHeads

    config.Lap "Heads scoring with Infernal"

    let assembliesOf16sHeadsMiddles = new ResizeArray<AssemblyOf16s<_>>()

    mergeAssemblies
        assembliesOf16sHeads
        assembliesOf16s
        assembliesOf16sHeadsMiddles
        (new Dictionary<_,_>())
        (fun edges -> 
            incr globalResultCount
            new AssemblyOf16s<_>(!globalResultCount, edges, head = true, middle = true))

    //assembliesOf16sHeadsMiddles |> ResizeArray.map (fun a -> a.ConvertToString(longEdges)) |> fun s -> System.IO.File.WriteAllLines(config.FileForHeadAndMiddles,s)
    let assembliesOf16sHeadsMiddles = score config.FileForHeadAndMiddles config.HeadsMiddlesBias longEdges assembliesOf16sHeadsMiddles
    
    config.Lap "Heads and middles combining"
    
//    let edgesForTailsSearch = new HashSet<_> ()
//    
//    headsAndTails
//    |> Array.iter 
//        (fun (h, m, t) ->
//            edgesForTailsSearch.Add m.Edges.[m.Edges.Count - 1] |> ignore
//            t |> Array.iter (fun a -> a |> ResizeArray.iter (fun x -> edgesForTailsSearch.Add x |> ignore)))
//
//    let graphs = splitToConnectedSubgraphs edgesForTailsSearch config.TailSearchConfig.Tokenizer
//    let assembliesOf16sTails = new ResizeArray<AssemblyOf16s<_>>()
//
//    searchInBioGraphs config.TailSearchConfig config graphs assembliesOf16sTails
//
//    config.Lap "Tails parsing"
//
//    printfn "Tails Length = %A" assembliesOf16sTails.Count
//    let assembliesOf16sTails = score config.TailSearchConfig.OutFileName config.TailsBias longEdges assembliesOf16sTails
//
////    let gaps =
////        let startEndVertices = new HashSet<_>()
////        for h in assembliesOf16sHeadsMiddles do
////            for t in assembliesOf16sTails do
////                if t.InfernalData.Value.ModelFrom - h.InfernalData.Value.ModelTo > 10
////                then startEndVertices.Add (h.Edges.[h.Edges.Count-1].Target, t.Edges.[0].Source) |> ignore
////        let gags = new Dictionary<_,_>()
////        for (s,e) in startEndVertices do
////            getPaths g true s (fun (curE:TaggedEdge<_,_>) curL -> curE.Target = e || curL <= 100) 100
////            |> ResizeArray.filter (fun edgs -> edgs.Count > 0 && edgs.[edgs.Count - 1 ].Target = e)
////            |> fun r -> 
////                if r.Count > 0
////                then 
////                    r
////                    |> Seq.minBy (fun edgs -> edgs.Count) 
////                    |> fun p -> gags.Add(s,(p,e))
////        gags
//
//    let assembliesOf16sFull = new ResizeArray<_>()
//
//    mergeAssemblies
//        assembliesOf16sHeadsMiddles
//        assembliesOf16sTails
//        assembliesOf16sFull
//        //gaps
//        (new Dictionary<_,_>())
//        (fun edges -> 
//            incr globalResultCount
//            new AssemblyOf16s<_>(!globalResultCount, edges, head = true, middle = true, tail = true))
//
//    //assembliesOf16sFull |> ResizeArray.map (fun a -> a.ConvertToString(longEdges)) |> fun s -> System.IO.File.WriteAllLines(config.FileForFull, s)
//
//    let final = score config.FileForFull config.FinalBias longEdges assembliesOf16sFull
//    
//    printfn "Total before filtering: %A" final.Count
//
//    final
//    |> ResizeArray.filter 
//        (fun a ->
//            match a.InfernalData with
//            | Some d -> 
//                d.ModelTo - d.ModelFrom > 1000
//            | _ -> false
//            )
//    |> ResizeArray.map (fun a -> a.ConvertToString(longEdges, true)) |> fun s -> System.IO.File.WriteAllLines(config.FileForScoredFull, s)

    config.Lap "Final tails processing"
    config.PrintTiming ()
    ()

[<EntryPoint>]
let main argv = 
    let config = new Config(argv)    
    searchMain config
    0
