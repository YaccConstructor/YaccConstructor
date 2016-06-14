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
open GLL.shift_problem
//open MBrace.Azure.Management

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
type SearchConfig<'Token> =
    val SearchWithoutSPPF: BioParserInputGraph -> int -> ParseResult<ResultStruct>
    val SearchWithSPPF: ParserInputGraph<'Token> -> ParseResult<'Token>
    val Tokenizer: char -> int
    val HightLengthLimit: int
    val LowLengthLimit: int
    val StartNonterm: int

    new(withoutSppf, withSppf, getSmb, lowLengthLimit, hightLengthLimit, startNonterm) = 
        {
            SearchWithoutSPPF = withoutSppf
            SearchWithSPPF = withSppf
            Tokenizer = getSmb
            HightLengthLimit = hightLengthLimit
            LowLengthLimit = lowLengthLimit
            StartNonterm = startNonterm
        }

let filterRnaParsingResult (graph:BioParserInputGraph) lengthLimit res  =
    let hihtLenghtLimit = 100.0
    match res:ParseResult<ResultStruct> with
    | Success ast -> 
        failwith "Result is success but it is unexpected success"
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
        let startEdges = new ResizeArray<_>()
        let finalEdges = new ResizeArray<_>()
        let subgraphs =
            filteredByLength
            |> Array.choose (fun r -> 
                let newStartEdge = qgEdgFromBio graph.Edges.[r.le]
                startEdges.Add graph.Edges.[r.le]
                let newEndEdge = qgEdgFromBio graph.Edges.[r.re]
                finalEdges.Add graph.Edges.[r.re]
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
        let mergedStart = Array.zeroCreate subgraphs.Length
        let mergedFinal = Array.zeroCreate subgraphs.Length
        let addStartFinal value =
            let r = new ResizeArray<_>()
            r.Add value
            r
            
        let callSF i j =
            mergedStart.[i] <- addStartFinal startEdges.[j]
            mergedFinal.[i] <- addStartFinal finalEdges.[j]
        subgraphs |> Array.iteri (fun i x -> ())
        subgraphs
        |> Array.iteri
            (fun i (g1, w1) ->
                if w1 > weightLimit || mergedSubgraphs.Count = 0
                then                     
                    mergedSubgraphs.Add (g1, ref w1)
                    callSF (mergedSubgraphs.Count - 1) i
                else 
                    let merged = ref false
                    mergedSubgraphs 
                    |> ResizeArray.iteri (
                        fun j (g2, w2) -> 
                            if !w2 < weightLimit && g2.Overlaps g1 
                            then 
                                g2.UnionWith g1
                                mergedStart.[j].Add startEdges.[i]
                                mergedFinal.[j].Add finalEdges.[i]
                                w2 := g2 |> Seq.sumBy (fun x -> x.Tag |> int)
                                merged := true
                                )
                    if not !merged
                    then 
                        mergedSubgraphs.Add(g1, ref w1)
                        callSF (mergedSubgraphs.Count - 1) i
                        
            )
        let mergedStart = Array.sub mergedStart 0 mergedSubgraphs.Count
        let mergedFinal = Array.sub mergedFinal 0 mergedSubgraphs.Count
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
            g1 |> Seq.collect (fun e -> fromStartVtoEdg.[e.Source])), mergedStart, mergedFinal
        
        
    | Error e -> 
        failwithf "Input parsing failed: %A" e

let convertToParserInputGraph (edges : ResizeArray<seq<BioParserEdge>>) (startEdges : ResizeArray<BioParserEdge>[]) (finalEdges : ResizeArray<BioParserEdge>[])  = 
    let result = new ResizeArray<_>()
    let index = ref 0
    let edg f t l = new ParserEdge<_>(f,t,l)
    let changeIndex i = 
        index := !index + i
    let addEdge (arr : ResizeArray<_>) b e t i =
        arr.Add(edg b e t)
        changeIndex (i + 1)
    
    let processEdge (set : System.Collections.Generic.IEnumerable<BioParserEdge>) f (vMap : System.Collections.Generic.Dictionary<_, _>) (edgesSet : ResizeArray<_>) =
        let checkAndAdd k v t =
            vMap.Add(k, v)
            f !index
            addEdge edgesSet !index (!index + 1) t 0
        for e in set do
            if e.Tokens.Length > 1 then
                let cond, v = vMap.TryGetValue e.Start
                if not cond 
                then
                    checkAndAdd e.Start !index e.Tokens.[0]    
                else
                    addEdge edgesSet v (!index) e.Tokens.[0] -1  
                for i = 1 to e.Tokens.Length - 2 do
                    f !index
                    addEdge edgesSet !index (!index + 1) e.Tokens.[i] 0
                let cond, v = vMap.TryGetValue e.End
                if not cond 
                then
                    checkAndAdd e.End (!index + 1) e.Tokens.[e.Tokens.Length - 1]
                    index := !index + 1
                else
                    f !index
                    addEdge edgesSet !index v e.Tokens.[e.Tokens.Length - 1] 0
            else
                let c1, v1 = vMap.TryGetValue e.Start
                let c2, v2 = vMap.TryGetValue e.End
                if not c1
                then
                    vMap.Add(e.Start, !index)
                    f !index
                    if not c2
                    then
                        vMap.Add(e.End, !index + 1)
                        addEdge edgesSet !index (!index + 1) e.Tokens.[0] 1
                    else
                        addEdge edgesSet !index v2 e.Tokens.[0] 0
                else
                    if not c2
                    then
                        vMap.Add(e.End, !index)
                        addEdge edgesSet v1 !index e.Tokens.[0] 0
                    else
                        addEdge edgesSet v1 v2 e.Tokens.[0] -1
        ()
        
    let createStartFinalSets (set : ResizeArray<BioParserEdge>) (vMap: System.Collections.Generic.Dictionary<_, _>) (edgesSet : ResizeArray<_>)= 
        let res = new ResizeArray<_>()
        let f i = res.Add(i)
        processEdge set f vMap edgesSet
        res

    for g = 0 to edges.Count - 1 do
        index := 0
        let vertexMap = new System.Collections.Generic.Dictionary<_, _>()
        let edgesRes = new ResizeArray<_>()
        let startV = createStartFinalSets startEdges.[g] vertexMap edgesRes
        let finalV = createStartFinalSets finalEdges.[g] vertexMap edgesRes
        for i = 0 to finalV.Count - 1 do
            finalV.[i] <- finalV.[i] + 1 
        let f i = ()
        processEdge edges.[g] f vertexMap edgesRes 
        let resGraph = new ParserInputGraph<_>(startV.ToArray(), finalV.ToArray())
        let tmp = resGraph.AddVerticesAndEdgeRange edgesRes 
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

let searchInBioGraphs (searchCfg : SearchConfig<_>) graphs agentsCount =
    let start = System.DateTime.Now
    let processSubgraphs (subgraphs:ResizeArray<_>) (startEdges : ResizeArray<_>[]) (finalEdges : ResizeArray<_>[])=
        let parserInputGraphs = convertToParserInputGraph subgraphs startEdges finalEdges
        parserInputGraphs
        |> ResizeArray.iteri
            (fun i g ->
                searchCfg.SearchWithSPPF g |> ignore
            )
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
                                let g, s ,f = searchCfg.SearchWithoutSPPF graph searchCfg.StartNonterm |> filterRnaParsingResult graph searchCfg.LowLengthLimit
                                processSubgraphs g s f 
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
    
    new SearchConfig<_>(GLL.tRNA.buildAbstract, GLL.tRNA.buildAbstractAst, getSmb, 120, 60, 4)

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
    
    new SearchConfig<_>(GLL.r16s.H22_H23.buildAbstract, GLL.r16s.H22_H23.buildAbstractAst, getSmb, 180, 340, 12)

let shift_problem_SearchConfig =

    let getSmb =
        let cnt = ref 0
        fun ch ->
            let i = incr cnt; !cnt 
            match ch with
            | 'A' -> GLL.shift_problem.A i                
            | 'C' -> GLL.shift_problem.C i
            | 'G' -> GLL.shift_problem.G i
            | x ->   failwithf "Strange symbol in input: %A" x
            |> GLL.shift_problem.tokenToNumber
    
    new SearchConfig<_>(GLL.shift_problem.buildAbstract, GLL.shift_problem.buildAbstractAst, getSmb, 100, 0, 1)

let searchMain path what agentsCount =
    let searchCfg = 
        match what with
        | TRNA -> tRNASearchConfig
        | R16S_H22_H23 -> r16s_H22_H23_SearchConfig
        | Shift_problem -> shift_problem_SearchConfig

    let graphs, longEdges = loadGraphFormFileToBioParserInputGraph path searchCfg.HightLengthLimit searchCfg.Tokenizer (GLL.tRNA.RNGLR_EOF 0)

    //let gs = graphs.[100..150]
    searchInBioGraphs searchCfg graphs agentsCount
    |> printfn "%A"
    //searchInCloud graphs
    ()

[<EntryPoint>]
let main argv = 
//    let parser = ArgumentParser.Create<CLIArguments>()
//    let args = parser.Parse argv
//    let appSetting = parser.ParseAppSettings (System.Reflection.Assembly.GetExecutingAssembly())
//    let agentsCount =
//        args.GetResult(<@Agents@>, defaultValue = appSetting.GetResult(<@Agents@>, defaultValue = 1))        
//    let inputGraphPath = 
//        args.GetResult <@Input@>
//        |> (fun s ->
//                System.IO.Path.Combine(System.IO.Path.GetDirectoryName s, System.IO.Path.GetFileNameWithoutExtension s))
//    searchTRNA inputGraphPath agentsCount
//    searchMain inputGraphPath R16S_H22_H23 agentsCount
//    let e1 = new BioParserEdge(0, 1, 2, [|0; 1|])
//    let e2 = new BioParserEdge(1, 2, 2, [|3;4|])
//    let e3 = new BioParserEdge(2, 3, 2, [|1; 6|])
//    let e4 = new BioParserEdge(3, 4, 2, [|7; 8|])
//    let e5 = new BioParserEdge(2, 5, 1, [|9|])
//    let e6 = new BioParserEdge(1, 6, 2, [|4; 5|])
//    let e7 = new BioParserEdge(9, 3, 2, [|6; 7|])
//    let e8 = new BioParserEdge(9, 6, 1, [|10|])
//    let e9 = new BioParserEdge(6, 7, 1, [|11|])
//    let e10 = new BioParserEdge(7, 3, 2, [|12; 13|])
//    let e11 = new BioParserEdge(8, 9, 1, [|2|])
//
//    let seqE = new ResizeArray<_>()
//    let t = [| e2; e3; e6; e7; e8; e9; e10;|]
//    seqE.Add t
//    let s = ResizeArray.init 1 (fun _ -> new ResizeArray<_>())
//    s.[0].Add e1
//    s.[0].Add e11
//    let f = ResizeArray.init 1 (fun _ -> new ResizeArray<_>())
//    f.[0].Add e4
//    f.[0].Add e5
//    let tmp = convertToParserInputGraph seqE s f
    
    let path = "C:\Users\User\recursive-ascent\Tests\bio\problem_with_shift_2"
    let inputGraphPath = System.IO.Path.Combine(System.IO.Path.GetDirectoryName path, System.IO.Path.GetFileNameWithoutExtension path)
    searchMain inputGraphPath Shift_problem 0
    //let parseBioGraph = 
    0
