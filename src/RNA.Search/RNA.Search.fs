module YC.Bio.RNA.Search

open Argu

open YC.BIO.BioGraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParserWithoutTree

open Microsoft.FSharp.Collections

open MBrace.Azure
open MBrace.Core
open MBrace.Core.Builders
open MBrace.Core.CloudOperators
open MBrace.Runtime
//open MBrace.Azure.Management

type WhatShouldISearch =
    | TRNA
    | R16S_H22_H23

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
type SearchConfig(*<'Token>*) =
    val SearchWithoutSPPF: BioParserInputGraph -> int -> ParseResult<ResultStruct>
    //val SearchWithSPPF: ParserInputGraph<'Token> -> ParseResult<'Token>
    val Tokenizer: char -> int
    val HightLengthLimit: int
    val LowLengthLimit: int
    val StartNonterm: int

    new(withoutSppf(*, withSppf*), getSmb, lowLengthLimit, hightLengthLimit, startNonterm) = 
        {
            SearchWithoutSPPF = withoutSppf
            //SearchWithSPPF = withSppf
            Tokenizer = getSmb
            HightLengthLimit = hightLengthLimit
            LowLengthLimit = lowLengthLimit
            StartNonterm = startNonterm
        }

let filterRnaParsingResult (graph:BioParserInputGraph) lengthLimit res  =
    let hihtLenghtLimit = 100.0
    match res:ParseResult<ResultStruct> with
    | Success ast -> 
        failwith "Result is success but it is unrxpectrd success"
    | Success1 x ->        
        let weightLimit = 10000
        let filteredByLength = x |> Array.filter (fun i -> i.length >= byte lengthLimit)
        let qgEdgFromBio (e:BioParserEdge) = new QuickGraph.TaggedEquatableEdge<_,_>(e.Start, e.End, float e.RealLenght) 
        let subgraphsMemoization =
            let x = new System.Collections.Generic.Dictionary<_,_>()
            fun s e ->
                let flg,v = x.TryGetValue((s,e))
                if flg
                then true
                else 
                    x.Add((s,e),0)
                    false

        let findSubgraph s e = 
            let qg = new QuickGraph.AdjacencyGraph<_,_>()
            let _ = qg.AddVerticesAndEdgeRange(graph.Edges |> Array.map qgEdgFromBio)
            let yen = new QuickGraph.Algorithms.ShortestPath.Yen.YenShortestPathsAlgorithm<_>(qg, s, e, 1000)
            let paths = yen.Execute() 
                        |> Seq.filter (fun p -> p |> Seq.sumBy (fun e -> e.Tag) < hihtLenghtLimit)
            paths
            |> Seq.concat
            |> System.Collections.Generic.HashSet<_>
            |> fun s -> s, (s |> Seq.sumBy (fun x -> x.Tag |> int))
        
        let subgraphs =
            filteredByLength
            |> Array.choose (fun r -> 
                let newStartEdge = qgEdgFromBio graph.Edges.[r.le]
                let newEndEdge = qgEdgFromBio graph.Edges.[r.re]
                let additionalWeight = newStartEdge.Tag + newEndEdge.Tag |> int
                if r.le = r.re || graph.Edges.[r.le].End = graph.Edges.[r.re].Start
                then (new System.Collections.Generic.HashSet<_> [|newStartEdge; newEndEdge|], additionalWeight)
                     |> Some
                elif not <| subgraphsMemoization graph.Edges.[r.le].End graph.Edges.[r.re].Start
                then 
                    let s,w = findSubgraph graph.Edges.[r.le].End graph.Edges.[r.re].Start
                    s.Add newStartEdge |> ignore
                    s.Add newEndEdge |> ignore
                    (s, w + additionalWeight)
                    |> Some
                else None
                )
        
        let mergedSubgraphs = new ResizeArray<_>()

        subgraphs
        |> Array.iter
            (fun (g1, w1) ->
                if w1 > weightLimit || mergedSubgraphs.Count = 0
                then mergedSubgraphs.Add (g1, ref w1)
                else 
                    let merged = ref false
                    mergedSubgraphs 
                    |> ResizeArray.iter (
                        fun (g2, w2) -> 
                            if !w2 < weightLimit && g2.Overlaps g1 
                            then 
                                g2.UnionWith g1
                                w2 := g2 |> Seq.sumBy (fun x -> x.Tag |> int)
                                merged := true
                                )
                    if not !merged
                    then mergedSubgraphs.Add(g1, ref w1)
            )

        let fromStartVtoEdg = new SysDict<_,ResizeArray<_>>()
        graph.Edges
        |> Array.iter (
            fun e -> 
                let flg, v = fromStartVtoEdg.TryGetValue(e.Start)
                if flg
                then v.Add e
                else fromStartVtoEdg.Add(e.Start, new ResizeArray<_>([|e|]))
                )
        
        mergedSubgraphs
        |> ResizeArray.map (fun (g1,n) -> 
            printfn "Subgraph. smb = %A edges = %A" !n g1.Count
            g1 |> Seq.collect (fun e -> fromStartVtoEdg.[e.Source]))
        
    | Error e -> 
        failwithf "Input parsing failed: %A" e

let convertToParserInputGraph (edges : ResizeArray<seq<BioParserEdge>>) maxVertex (startEdges : ResizeArray<ResizeArray<BioParserEdge>>) (finalEdges : ResizeArray<ResizeArray<BioParserEdge>>)  = 
    let result = new ResizeArray<_>()
    let mutable curB = 0
    let mutable curE = 0 
    let index = ref 0
    let edg f t l = new ParserEdge<_>(f,t,l)
    let addEdge (arr : ResizeArray<_>) b e t =
        arr.Add(edg b e t)
        curB <- curE
        curE <- curE + 1 

    let createStartFinalSets (set : ResizeArray<BioParserEdge>) (vMap: System.Collections.Generic.Dictionary<_, _>) (edgesSet : ResizeArray<_>)= 
        curB <- !index
        curE <- !index
        let res = new ResizeArray<_>()
        for edge in set do
            vMap.Add(edge.Start, !index)
            res.Add(!index)
            edgesSet.Add(edg !index (!index + 1) edge.Tokens.[0])
            if edge.Tokens.Length >1 
            then
                for i = 1 to edge.Tokens.Length - 2 do
                    addEdge edgesSet curB curE edge.Tokens.[i]   
                vMap.Add(curE,edge.End)
                addEdge edgesSet curB curE edge.Tokens.[edge.Tokens.Length - 1]
                index := curE + 1
            else
                vMap.Add(curE, edge.End)
        res
    for g = 0 to edges.Count - 1 do
        let vertexMap = new System.Collections.Generic.Dictionary<_, _>()
        let edgesRes = new ResizeArray<_>()
        let startV = createStartFinalSets startEdges.[g] vertexMap edgesRes
        let finalV = createStartFinalSets finalEdges.[g] vertexMap edgesRes
      
        for e in edges.[g] do
            addEdge edgesRes e.Start !index e.Tokens.[0]
            vertexMap.Add(e.Start, !index)
            curB <- !index + 1
            curE <- !index + 2
            if e.Tokens.Length > 1 then
                for i = 1 to e.Tokens.Length - 2 do
                    addEdge edgesRes curE curB e.Tokens.[i]
                addEdge edgesRes curE e.End e.Tokens.[e.Tokens.Length - 1]
                vertexMap.Add(e.End, curE) 
        let resGraph = new ParserInputGraph<_>(startV.ToArray(), finalV.ToArray())
        resGraph.AddEdgeRange edgesRes |> ignore
        result.Add(resGraph)
    result
        
let searchInCloud graphs =
    let start = System.DateTime.Now

    let myStorageConnectionString = @"DefaultEndpointsProtocol=https;AccountName=mbracec3bb1560;AccountKey=G5GcN2Ne1JyP2u46EuAsCKZANM/xPSilqbwBk0z7zAncPStQax3SpYhxMb+8fwMSyXHhqhacsSwmHg3ZXZG/0A=="
    let myServiceBusConnectionString = @"EndPoint=sb://mbrace085d90e9.servicebus.windows.net/;SharedAccessKeyName=RootManageSharedAccessKey;SharedAccessKey=+9y8h6pLDSZFeSr5KyYPslxpoI6zkAz0ryHYvNTe2KY="
    let config = new Configuration(myStorageConnectionString, myServiceBusConnectionString)    
    let cluster = 
        AzureCluster.Connect(config, 
                                       logger = ConsoleLogger(true), 
                                       logLevel = LogLevel.Info)

    cluster.Reset(true,true,true,true,true,true,true)
    cluster.ClearAllProcesses()
    cluster.ShowWorkers()
   
    let cloudComputations = 
        cloud { 
            try
                let processGraph graph = 
                    try
                        GLL.tRNA.buildAbstract graph 3                                
                        |> filterRnaParsingResult graph 60
                        |> Some
                    with
                    | e -> None 
                let! result = Cloud.Parallel [for g in graphs -> cloud {return Some (string g)}]
                return  result |> Array.choose id
            with
            | e -> return [|e.Message|]
            }
        |> cluster.CreateProcess
        
    cloudComputations.Status |> printfn "%A"
    let r = cloudComputations.Result
    printfn "time = %A" (System.DateTime.Now - start)
    printfn "%A" r
    r

let searchInBioGraphs (searchCfg:SearchConfig) graphs agentsCount =
    let start = System.DateTime.Now
    let processSubgraphs (subgraphs:ResizeArray<_>) =
//        let parseWithSPPF graph =
//            let sppf = GLL.tRNA.buildAbstractAst graph
//            match sppf with
//            | Success sppf -> ()
//            | _ -> ()
        ()
    ()

    let agent name  =
        MailboxProcessor.Start(fun inbox ->
            let rec loop n =
                async {
                        let! msg = inbox.Receive()
                        match msg with
                        | Data (i,graph) ->
                            try
                                searchCfg.SearchWithoutSPPF graph searchCfg.StartNonterm                                
                                |> filterRnaParsingResult graph searchCfg.LowLengthLimit
                                |> processSubgraphs
                            with
                            | e -> printfn "ERROR in bio graph parsing! %A" e.Message
                            return! loop n         
                        | Die ch -> ch.Reply()
                        }
            loop 0)

    let agents = Array.init agentsCount (fun i -> agent (sprintf "searchAgent%A" i))
    graphs
    |> Array.iteri 
        (fun i graph -> 
            Data (i, graph) 
            |> agents.[i % agentsCount].Post
        )
    agents |> Array.iter (fun a -> a.PostAndReply Die)
    printfn "Total time = %A" (System.DateTime.Now - start)
    0

let tRNASearchConfig =

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
    
    new SearchConfig(GLL.tRNA.buildAbstract(*, GLL.tRNA.buildAbstractAst*), getSmb, 120, 60, 4)

let r16s_H22_H23_SearchConfig =

    let getSmb =
        let cnt = ref 0
        fun ch ->
            let i = incr cnt; !cnt 
            match ch with
            | 'A' -> GLL.r16s.H22_H23.A i                
            | 'U' -> GLL.r16s.H22_H23.U i
            | 'T' -> GLL.r16s.H22_H23.U i
            | 'C' -> GLL.r16s.H22_H23.C i
            | 'G' -> GLL.r16s.H22_H23.G i
            | _ -> GLL.r16s.H22_H23.U i
            | x ->   failwithf "Strange symbol in input: %A" x
            |> GLL.r16s.H22_H23.tokenToNumber
    
    new SearchConfig(GLL.r16s.H22_H23.buildAbstract(*, GLL.r16s.H22_H23.buildAbstractAst*), getSmb, 180, 340, 12)

let searchMain path what agentsCount =
    let searchCfg = 
        match what with
        | TRNA -> tRNASearchConfig
        | R16S_H22_H23 -> r16s_H22_H23_SearchConfig

    let graphs, longEdges = loadGraphFormFileToBioParserInputGraph path searchCfg.HightLengthLimit searchCfg.Tokenizer (GLL.tRNA.RNGLR_EOF 0)

    let gs = graphs.[100..150]
    searchInBioGraphs searchCfg gs agentsCount
    |> printfn "%A"
    //searchInCloud graphs
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
    searchMain inputGraphPath R16S_H22_H23 agentsCount
    0
