module YC.Bio.RNA.SearchFSA

open QuickGraph
open Argu
open System
open YC.Bio.GraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParser
open YC.Bio.RNA.Search.Structs
open YC.Bio.RNA.IO
open Microsoft.FSharp.Collections
open System.IO
open System.Collections.Generic
open System.Diagnostics
open InfernalApi

let filterRnaParsingResult (graph : EdgeCompressedGraphInput) (searchCfg : SearchConfig) res = 
    let getPaths s e (lengths : HashSet<_>) = 
        let maxLength = 
            lengths
            |> Seq.max
    
        let rec dfs start curLength = 
            let toReturn = new List<List<TaggedEdge<_,_>>>()
            for edge in graph.OutEdges start do
                let newLength = curLength + edge.Tag.length
                if edge.Target = e && newLength
                                   |> lengths.Contains
                then new List<_>([ edge ]) |> toReturn.Add
                if newLength < maxLength then 
                    let paths = dfs edge.Target newLength
                    paths |> ResizeArray.iter (fun (x : List<_>) -> x.Add edge)
                    toReturn.AddRange paths
            toReturn
        
        let paths = dfs s 0 |> ResizeArray.map ResizeArray.rev
        if lengths.Contains 0 then new List<_>() |> paths.Add
        paths
    
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
        let letgths =
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
        let midlePaths = getPaths (fst eFrom) (fst eTo) letgths
        
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

let pathToString (searchCfg : SearchConfig) id (edges: ResizeArray<TaggedEdge<_,BioGraphEdgeLbl>>) =
    let data =
        edges
        |> ResizeArray.map (fun e -> e.Tag.str |> Array.map (fun i -> searchCfg.NumToString.[int i]) |> String.concat "")
        |> String.concat ""

    let metadata =
        edges
        |> ResizeArray.map(fun e -> e.Tag.id |> string )
        |> String.concat "; "
    "> " + id + " | " + metadata + "\n" + data + "\n"

let parsingResultsProcessor () =
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
                            pathToString cfg (string !edgesGlobalCounter) e)
                        |> fun strs -> System.IO.File.AppendAllLines(cfg.OutFileName, strs)
                    with e -> printfn "ERROR in parsing results postprocessing! %A" e.Message
                    return! loop n
                | PDie ch -> ch.Reply()
            }
        loop 0)
    
let searchInBioGraphs (searchCfg : SearchConfig) (graphs : EdgeCompressedGraphInput[]) agentsCount = 
    let start = System.DateTime.Now
    let postprocessor = parsingResultsProcessor ()
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
            if graph.VertexCount < 100
            then 1 + i % (agentsCount - 1)
            else 0
        Data(i, graph) |> agents.[agentId].Post)
    agents |> Array.iter (fun a -> a.PostAndReply Die)
    postprocessor.PostAndReply PDie
    printfn "Total time = %A" (System.DateTime.Now - start)
    0

let printLongEdges path edges = 
    let toPrint = 
        edges
        |> Array.mapi (fun i x -> 
               [| ">Long" + i.ToString()
                  x |])
        |> Array.collect id
    File.AppendAllLines(path, toPrint)

let getSubgraphs (edges : TaggedEdge<_,BioGraphEdgeLbl> []) (startEdges : Dictionary<int,int>) (lengthLimit : int) (lengthOfBeginning : int) = 
    let inEdges = new Dictionary<int, ResizeArray<int>>()
    //let setOfStartEdges = Set.ofArray (startEdges)
    let startEdgesNumbers = new Dictionary<_,_>()
    
    edges |> Array.iteri (fun i edge -> 
        let cond, value = inEdges.TryGetValue(edge.Target)
        if cond
        then value.Add(i)
        else inEdges.Add(edge.Target, new ResizeArray<_>([i]))
                 
        if (edge.Tag.id |> startEdges.ContainsKey)
        then startEdgesNumbers.Add(i, startEdges.[edge.Tag.id]))
    
    let getSubGraph (edge : TaggedEdge<_,BioGraphEdgeLbl>) (posOnEdge : int) = 
        let result = new HashSet<_>()
        let edgesToBegin = new HashSet<_>()
        
        /// Returns true if path was added.
        let rec dfs start curLength : bool = 
            let cond, value = inEdges.TryGetValue(start)
            if cond
            then
                let addBranch = ref false

                for edgeNum in value do
                    let edge = edges.[edgeNum]
                    let newLength = curLength + edge.Tag.length
                    
                    if newLength < lengthLimit
                    then
                        let pathWasAdded = dfs edge.Source newLength
                        if newLength > lengthLimit - lengthOfBeginning
                        then
                            addBranch := true
                            result.Add edge |> ignore
                            edgesToBegin.Add(edge.Tag.id) |> ignore
                        elif pathWasAdded
                        then
                            addBranch := true
                            result.Add edge |> ignore    
                    else
                        addBranch := true
                        result.Add edge |> ignore
                        edgesToBegin.Add(edge.Tag.id) |> ignore

                !addBranch
            else
                false
        
        if posOnEdge + 1 >= lengthLimit || dfs edge.Source (posOnEdge + 1)
        then
            result.Add edge |> ignore
            if posOnEdge + 1 > lengthLimit - lengthOfBeginning
            then
                edgesToBegin.Add edge.Tag.id |> ignore

        result, edgesToBegin

    let res = 
        startEdgesNumbers
        |> Seq.map (fun kvp -> 
               let edge = edges.[kvp.Key]
               getSubGraph edge kvp.Value)
        |> Array.ofSeq
        |> Array.filter (fun (allEdges, startEdges) ->
            if allEdges.Count > 0 && startEdges.Count = 0
            then
                failwith "No start positions!"
            
            allEdges.Count > 0 && startEdges.Count > 0
            )
    res

let searchMiddle path agentsCount = 
    let searchCfg = FSA_R16S_19_27_SearchConfig
    let sourceGraph, graphs, longEdges = 
        loadGraph path searchCfg.HighLengthLimit searchCfg.Tokenizer
    //printLongEdges @"C:\CM\long_edges.fa" longEdges
    let graphs =
        graphs.[0..]
        |> Array.filter (fun g -> g.Edges |> Seq.sumBy (fun e -> e.Tag.str.Length) > int (float searchCfg.LowLengthLimit / 1.8))

//    let strange = graphs |> Array.filter (fun g -> g.VertexCount = 4 && g.EdgeCount = 4)
//    
//    printfn "%A" strange

    printfn "Total graph to porcess: %A" graphs.Length
    searchInBioGraphs searchCfg graphs agentsCount |> printfn "%A"
    sourceGraph

//let searchBeginning agentsCount sourceGraph = 
//    let searchCfg = FSA_R16S_1_18_SearchConfig
//    
//    let convert (g : HashSet<TaggedEdge<_,BioGraphEdgeLbl>> * HashSet<int>) = 
//        let startEdges = new ResizeArray<_>()
//        
//        let edges = 
//            let eList = new ResizeArray<_>()
//            let edges, edgesToBegin = g
//            edges|> Seq.iteri (fun i edge -> 
//                         let tag = edge.Tag.str.ToCharArray() |> Array.map searchCfg.Tokenizer
//                         let newEdge = 
//                             new BioParserEdge(edge.Source, edge.Target, edge.Tag.length, tag, edge.Tag.id, 
//                                               edge.Tag.sourceStartPos)
//                         eList.Add(newEdge)
//                         if edgesToBegin.Contains(edge.Tag.id) then startEdges.Add(i))
//            eList.ToArray()
//        new BioParserInputGraph(edges, startEdges |> Set)
//    
//    let startEdges = filterResult "R16S_19_27_result.fa"
//    let subgraphs = 
//        getSubgraphs sourceGraph startEdges searchCfg.HighLengthLimit searchCfg.LengthOfBeinning
//    
//    let subgraphs = 
//        subgraphs
//        |> Array.map convert
//    printfn "\nSubgraphs count: %A.\n" subgraphs.Length
//    searchInBioGraphs searchCfg subgraphs agentsCount |> printfn "%A"

let searchMain path agentsCount = 
    let sourceGraph = searchMiddle path agentsCount
    //searchBeginning agentsCount sourceGraph
    ()

/// Divides input on edges 1 to 10 symbols
let toSmallEdges path = 
    let inputExt = ".txt"
    let lblsExt = ".sqn"
    let graphStrauctureExt = ".grp"
    let rnd = new System.Random()
    
    let splitLine (line1 : string) = 
        let rec inner (line : string) = 
            let subLineLength = rnd.Next(1, 11)
            if line.Length <= subLineLength then [ line ]
            else (line.Substring(0, subLineLength)) :: (inner (line.Substring subLineLength))
        inner line1 |> List.toArray
    
    let strings = 
        File.ReadAllLines(path + inputExt)
        |> Array.collect splitLine
        |> List.ofArray
    
    let toPrint = strings |> List.mapi (fun i line -> ">" + (i + 1).ToString() + "\n" + line)
    
    let rec getVertexList i = 
        if i = 0 then []
        else getVertexList (i - 1) @ [ ("Vertex " + i.ToString() + " ~ 0 .") ]
    
    let rec getEdgeList i = 
        if i = 0 then []
        else 
            getEdgeList (i - 1) 
            @ [ ("Edge " + i.ToString() + " : " + i.ToString() + " -> " + (i + 1).ToString() + ", l = " 
                 + (strings.[i - 1].Length).ToString() + " ~ 0 .") ]
    
    File.WriteAllLines(path + lblsExt, toPrint)
    File.WriteAllLines
        (path + graphStrauctureExt, getVertexList (strings.Length + 1) @ [ "\n" ] @ getEdgeList strings.Length)

let filterRes path (names : HashSet<_>) = 
    let lines = File.ReadAllLines(path)
    let buf = lines |> Array.pairwise
    buf |> Array.filter (fun (name, _) -> names.Contains(name.Substring(1)))

let filterLongEdges path (names : Dictionary<_, _>) = 
    let lines = File.ReadAllLines(path)
    let buf = lines |> Array.pairwise
    buf |> Array.collect (fun (name, str) -> 
               let cond, value = names.TryGetValue(name.Substring(1))
               if cond then 
                   [| name
                      str.Substring(fst value, snd value - fst value) |]
               else [||])

let printPairs path pairs = 
    let x = pairs |> Array.collect (fun (x, y) -> [| x; y |])
    File.AppendAllLines(path, x)

[<EntryPoint>]
let main argv = 
    //let r = getTbl @"C:\CM\log.txt"
    //let filtered = filterRes @"C:\CM\result.fa" (new HashSet<_>(r))
    //printPairs @"C:\CM\filteredResult.fa" filtered
    (*
    let r = getTbl @"C:\CM\long_log.txt"
    let dict = new Dictionary<_,_>()
    r
    |> Array.groupBy (fun (x, y, z) -> x)
    |> Array.map (fun (name,x) ->
        let firstest = ref 1000000
        let lastest = ref -1
        x
        |> Array.iter (fun (_, s, e) ->
            let start = min s e
            let ending = max s e
            if start < !firstest then firstest := start
            if ending > !lastest then lastest := ending
            )
        name, !firstest, !lastest
        )
    |> Array.iter (fun (name, s, e) -> dict.Add(name, (s,e)))
    let filtered = filterLongEdges @"C:\CM\long_edges.fa" (dict)
    File.AppendAllLines(@"C:\CM\filteredLongEdges.fa", filtered)
    *)
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
