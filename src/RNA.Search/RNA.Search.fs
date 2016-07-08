module YC.Bio.RNA.Search

open Argu

open YC.BIO.BioGraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParserWithoutTree

open Microsoft.FSharp.Collections
open System.IO
open System.Collections.Generic

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
type SearchConfig =
    val SearchWithoutSPPF: BioParserInputGraph -> int -> ParseResult<ResultStruct>
    val SearchWithSPPF: ParserInputGraph -> ParseResult<int>
    val Tokenizer: char -> int
    val HighLengthLimit: int
    val LowLengthLimit: int
    val StartNonterm: int
    val NumToString : int -> string

    new(withoutSppf, withSppf, getSmb, lowLengthLimit, highLengthLimit, startNonterm, numToString) = 
        {
            SearchWithoutSPPF = withoutSppf
            SearchWithSPPF = withSppf
            Tokenizer = getSmb
            HighLengthLimit = highLengthLimit
            LowLengthLimit = lowLengthLimit
            StartNonterm = startNonterm
            NumToString = numToString
        }

let filterRnaParsingResult (graph:BioParserInputGraph) lowLengthLimit highLengthLimit res  =
    //let hihtLenghtLimit = 500.0
    let hihtLenghtLimit = float highLengthLimit
    match res:ParseResult<ResultStruct> with
    | Success ast -> 
        failwith "Result is success but it is unexpected success"
    | Success1 x ->        
        let weightLimit = 10000
        let filteredByLength = 
            x 
            |> Array.filter (fun i -> i.length >= uint16 lowLengthLimit && i.length <= uint16 highLengthLimit)
            |> Seq.groupBy(fun x -> x.le, x.re)
            //|> Seq.map(fun (_,a) -> a |> Seq.maxBy (fun x -> x.length))
            |> Seq.map(fun (edg,a) -> edg, a |> Seq.collect (fun res -> [res.lpos, res.rpos, res.length]))
            |> Array.ofSeq
        let qgEdgFromBio (e:BioParserEdge) =
            new QuickGraph.TaggedEquatableEdge<_,_>(e.Start, e.End, float e.RealLenght) 
        let subgraphsMemoization =
            let x = new System.Collections.Generic.Dictionary<_,_>()
            fun s e ->
                let flg,v = x.TryGetValue((s,e))
                if flg
                then true
                else 
                    x.Add((s,e),0)
                    false

        let qg = new QuickGraph.AdjacencyGraph<_,_>()
        let _ = qg.AddVerticesAndEdgeRange(graph.Edges |> Array.map qgEdgFromBio)
        
        let findSubgraph s e (lengths:HashSet<uint16>) = 
            let yen = new QuickGraph.Algorithms.ShortestPath.Yen.YenShortestPathsAlgorithm<_>(qg, s, e, 1000)
            let paths = yen.Execute()
            let filteredPaths = paths |> Seq.filter (fun p -> p |> Seq.sumBy (fun e -> e.Tag) |> uint16 |> lengths.Contains)
            //let allEdgsOfPaths = Seq.concat filteredPaths
            (*let hashSet = new System.Collections.Generic.HashSet<_>()*)
            //printfn "Length: %A" (Seq.toList allEdgsOfPaths).Length
            //for edge in allEdgsOfPaths do
            //    printfn "Edge: %A" edge
            (*Seq.iter (fun x -> hashSet.Add x |> ignore) allEdgsOfPaths
            hashSet, (hashSet |> Seq.sumBy (fun x -> x.Tag |> int))*)
            filteredPaths |> Seq.toList
        
        
        let subgraphs =
            filteredByLength
            |> Array.choose (fun ((le, re), poss) ->
                (*
                let startEdges = new ResizeArray<_>()
                let finalEdges = new ResizeArray<_>()
                let newStartEdge = qgEdgFromBio graph.Edges.[le]
                startEdges.Add (graph.Edges.[le], poss |> Seq.collect (fun (l,_,_) -> [l]) |> Seq.distinct)
                let newEndEdge = qgEdgFromBio graph.Edges.[re]
                finalEdges.Add (graph.Edges.[re], poss |> Seq.collect (fun (_,r,_) -> [r]) |> Seq.distinct)
                let additionalWeight = newStartEdge.Tag + newEndEdge.Tag |> int
                *)
                let lengths0 = new ResizeArray<_>()
                let newPoss = poss
                              |> Seq.map (fun (lpos,rpos,pathLength) -> let leftEdgePartOfPath = uint16 <| graph.Edges.[le].RealLenght - lpos - 1
                                                                        let rightEdgePartOfPath = uint16 <| rpos + 1
                                                                        pathLength - leftEdgePartOfPath - rightEdgePartOfPath |> lengths0.Add |> ignore
                                                                        lpos,rpos,pathLength - leftEdgePartOfPath - rightEdgePartOfPath)
                let toReturn = graph.Edges.[le], graph.Edges.[re], newPoss
                //let startEdge = graph.Edges.[le], poss |> Seq.collect (fun (l,_,_) -> [l]) |> Seq.distinct
                //let finalEdge = graph.Edges.[re], poss |> Seq.collect (fun (_,r,_) -> [r]) |> Seq.distinct

                if le = re || graph.Edges.[le].End = graph.Edges.[re].Start
                then ((*new System.Collections.Generic.HashSet<_> [|newStartEdge; newEndEdge|]*) [], (*additionalWeight,*) toReturn)
                     |> Some
                elif not <| subgraphsMemoization graph.Edges.[le].End graph.Edges.[re].Start
                then
                    let lengths = new HashSet<uint16>(Seq.distinct lengths0)
                    (*poss |> Seq.iter (fun (lpos,rpos,pathLength) -> let leftEdgePartOfPath = uint16 <| graph.Edges.[le].RealLenght - lpos - 1
                                                                    let rightEdgePartOfPath = uint16 <| rpos + 1
                                                                    pathLength - leftEdgePartOfPath - rightEdgePartOfPath |> lengths.Add |> ignore )
                                                                    *)
                    let s(*,w*) = findSubgraph graph.Edges.[le].End graph.Edges.[re].Start lengths
                    //s.Add newStartEdge |> ignore
                    //s.Add newEndEdge |> ignore
                    (*(s, w + additionalWeight, startEdges, finalEdges)*)
                    (s, toReturn)
                    |> Some
                else None
                )
        (*
        let mergedSubgraphs = new ResizeArray<_>()
        let mergedStart = Array.zeroCreate subgraphs.Length
        let mergedFinal = Array.zeroCreate subgraphs.Length
        let addStartFinal value =
            let r = new ResizeArray<_>()
            r.Add value
            r
            
        let callSF i startEdges finalEdges =
            //if mergedStart |> Array.contains startEdges.[j] |> not
            //then 
            mergedStart.[i] <- startEdges
            mergedFinal.[i] <- finalEdges
        //subgraphs |> Array.iteri (fun i x -> ())
        subgraphs
        |> Array.iteri
            (fun i (g1, w1, startEdges, finalEdges) ->
                if w1 > weightLimit || mergedSubgraphs.Count = 0
                then                     
                    mergedSubgraphs.Add (g1, ref w1)
                    callSF (mergedSubgraphs.Count - 1) startEdges finalEdges
                else 
                    let merged = ref false
                    mergedSubgraphs 
                    |> ResizeArray.iteri (
                        fun j (g2, w2) -> 
                            if !w2 < weightLimit && g2.Overlaps g1 
                            then 
                                g2.UnionWith g1
                                startEdges.ForEach (fun x -> mergedStart.[j].Add x)
                                finalEdges.ForEach (fun x -> mergedFinal.[j].Add x)
                                w2 := g2 |> Seq.sumBy (fun x -> x.Tag |> int)
                                merged := true
                                )
                    if not !merged
                    then 
                        mergedSubgraphs.Add(g1, ref w1)
                        callSF (mergedSubgraphs.Count - 1) startEdges finalEdges
            )
        //cut zero endings
        let mergedStart = Array.sub mergedStart 0 mergedSubgraphs.Count
        let mergedFinal = Array.sub mergedFinal 0 mergedSubgraphs.Count
        *)
        let adjacentEdgs = new SysDict<_,ResizeArray<_>>()
        graph.Edges
        |> Array.iter (
            fun e -> 
                let flg, v = adjacentEdgs.TryGetValue(e.Start)
                if flg
                then v.Add e
                else adjacentEdgs.Add(e.Start, new ResizeArray<_>([|e|]))
                )
        
        subgraphs
        |> Seq.map (fun (paths, toReturn) -> Seq.map (Seq.collect (fun (e:QuickGraph.TaggedEquatableEdge<_,_>) -> adjacentEdgs.[e.Source]
                                                                                                                         |> ResizeArray.filter (fun (x:BioParserEdge) -> x.End = e.Target)
                                                                                                                         |> ResizeArray.distinct)) paths
                                                    , toReturn)
        (*
        let mergedsg =
            mergedSubgraphs
              |> ResizeArray.map (fun (g1,n) -> 
                printfn "Subgraph. smb = %A edges = %A" !n g1.Count
                g1 |> Seq.collect (fun e -> adjacentEdgs.[e.Source]
                                            |> ResizeArray.filter (fun (x:BioParserEdge) -> x.End = e.Target)
                                            |> ResizeArray.distinct)
                )
        mergedsg, mergedStart, mergedFinal
        *)

        
    | Error e -> 
        failwithf "Input parsing failed: %A" e

let convertToParserInputGraph (edges : ResizeArray<seq<BioParserEdge>>) (startEdges : ResizeArray<BioParserEdge * seq<int>>[]) (finalEdges : ResizeArray<BioParserEdge * seq<int>>[])  = 
    //let init = startEdges |> ResizeArray.distinctBy  (fun (x, y) -> x |> ResizeArray.map (fun (x,_) -> x)
    let result = new ResizeArray<_>()
    let index = ref 0
    let edg f t l = new ParserEdge<_>(f,t,l)
    let changeIndex i = 
        index := !index + i
    let addEdge (arr : ResizeArray<_>) b e t i =
        arr.Add(edg b e t)
        changeIndex (i + 1)
    
    let processEdges (set : System.Collections.Generic.IEnumerable<BioParserEdge>) f (vMap : System.Collections.Generic.Dictionary<_, _>) (edgesSet : ResizeArray<_>) (processedEdges : ResizeArray<_>) =
        let checkAndAdd k v t edgeNumber =
            vMap.Add(k, v)
            f !index edgeNumber
            addEdge edgesSet !index (!index + 1) t 0
        set |> Seq.iteri (fun edgeNumber e ->
            if processedEdges.Contains e then
                let cond, v = vMap.TryGetValue e.Start
                for i = v to v + e.RealLenght - 1 do
                    f i edgeNumber
            else
            processedEdges.Add e
            if e.Tokens.Length > 1 then
                let cond, v = vMap.TryGetValue e.Start

                if not cond 
                then
                    checkAndAdd e.Start !index e.Tokens.[0] edgeNumber    
                else
                    f v edgeNumber
                    addEdge edgesSet v (!index) e.Tokens.[0] -1

                for i = 1 to e.Tokens.Length - 2 do
                    f !index edgeNumber
                    addEdge edgesSet !index (!index + 1) e.Tokens.[i] 0

                let cond, v = vMap.TryGetValue e.End

                if not cond 
                then
                    checkAndAdd e.End (!index + 1) e.Tokens.[e.Tokens.Length - 1] edgeNumber
                    index := !index + 1
                else
                    f !index edgeNumber
                    addEdge edgesSet !index v e.Tokens.[e.Tokens.Length - 1] 0
            else
                let c1, v1 = vMap.TryGetValue e.Start
                let c2, v2 = vMap.TryGetValue e.End
                if not c1
                then
                    vMap.Add(e.Start, !index)
                    f !index edgeNumber
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
            )
        ()
        
    let createStartFinalSets (set : ResizeArray<BioParserEdge * seq<int>>) (vMap: System.Collections.Generic.Dictionary<_, _>) (edgesSet : ResizeArray<_>) (processedEdges : ResizeArray<_>) = 
        let res = new ResizeArray<_>()
        //delete equal edges and positions
        let edges = 
            set |> Seq.groupBy (fun (x , y) -> x)
                |> Seq.map (fun (key,a) -> key,(a |> Seq.collect (fun (_,poss) -> poss ) |> Seq.distinct))
                |> Seq.toList
        let edgesWithoutPos = edges|> List.map (fun (x,_) -> x)
        let positions = edges |> List.map (fun (_,p) -> p) 
        let counter = ref 0
        let f i edge = 
            if positions.[edge] |> Seq.exists (fun pos -> !counter = pos) then 
                res.Add(i)
            counter := !counter + 1
        processEdges edgesWithoutPos f vMap edgesSet processedEdges
        res

    for g = 0 to edges.Count - 1 do
        index := 0
        let vertexMap = new System.Collections.Generic.Dictionary<_, _>()
        let edgesRes = new ResizeArray<_>()
        let processedEdges = new ResizeArray<_>()
        
        let startV = createStartFinalSets startEdges.[g] vertexMap edgesRes processedEdges
        let finalV = createStartFinalSets finalEdges.[g] vertexMap edgesRes processedEdges
        for i = 0 to finalV.Count - 1 do
            finalV.[i] <- finalV.[i] + 1 
        //processEdge edges.[g] (fun x -> ()) vertexMap edgesRes
        
        processEdges (edges.[g] |> Seq.filter (fun x -> not (ResizeArray.exists (fun (y, _) -> y = x) startEdges.[g] ||
                                                             ResizeArray.exists (fun (y, _) -> y = x) finalEdges.[g]))) (fun x y -> ()) vertexMap edgesRes processedEdges
                                                            
        finalV |>
        ResizeArray.iter (fun v -> addEdge edgesRes v !index (GLL.r16s.H22_H23.tokenToNumber <| GLL.r16s.H22_H23.RNGLR_EOF 1) -1) 

        let resGraph = new ParserInputGraph(startV.ToArray(), [|!index|])//finalV.ToArray())
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
                        |> filterRnaParsingResult graph 60 800
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

let searchInBioGraphs (searchCfg : SearchConfig) graphs agentsCount =
    let start = System.DateTime.Now
    let processSubgraphs (subgraphs:ResizeArray<_>) (startEdges : ResizeArray<_>[]) (finalEdges : ResizeArray<_>[]) =
        let parserInputGraphs = convertToParserInputGraph subgraphs startEdges finalEdges
        parserInputGraphs |> Seq.iteri (fun i (x:ParserInputGraph) -> x.PrintToDot ("ParserInputGraph" + i.ToString() + ".dot") GLL.r16s.H22_H23.numToString)
        let paths = new ResizeArray<_>()
        parserInputGraphs
        |> ResizeArray.iteri
            (fun i g ->
                let start = System.DateTime.Now
                let r = searchCfg.SearchWithSPPF g 
                let time = System.DateTime.Now - start
                match r : ParseResult<_> with
                    | Success t -> 
                        (time,t.GetStrings searchCfg.StartNonterm searchCfg.NumToString) |> paths.Add
                    | Success1 _ -> failwith "Unexpected Success1" //failwith "Success expected, but got Success1" 
                    | Error e -> printfn "Parse with tree failed"//failwithf "Success expected, but got Error: %A" e
            )
        paths
    (*
    let printResult results graphNumber =
        let maxLineLength = 80
        let resultPath = ".\\result.fa"
        let rec splitLine (line:string) =
            if line.Length <= maxLineLength then [line] else
            let buff = line.Substring (0, maxLineLength)
            buff::(splitLine buff)
        results
        |> ResizeArray.iteri (fun subGraphNumber (time,strings) -> let info = ">Graph" + graphNumber.ToString() + ".Subgraph" + subGraphNumber.ToString()+ ". Time: "+ time.ToString()
                                                                   let toPrint = strings |> List.collect (fun (line:string) -> splitLine line)
                                                                   File.AppendAllLines(resultPath, info::toPrint))
    *)

    let printResultGraphs (res:seq<seq<seq<BioParserEdge>> * (BioParserEdge * BioParserEdge * seq<int * int * uint16>)>) i numToString = 
        let maxLineLength = 80
        let resultPath = ".\\result.fa"
        let isPrinted = ref false

        let splitEdge (e:BioParserEdge) = 
            Seq.fold (fun string x -> string + numToString x) "" e.Tokens
    
        let pathToString (path:seq<BioParserEdge>) =
            Seq.fold (fun string e -> string + splitEdge e) "" path

        let rec splitLine (line:string) =
            if line.Length <= maxLineLength then [line] else
            (line.Substring (0, maxLineLength))::(splitLine (line.Substring maxLineLength))
        
        let printResult j (paths, (startE, finalE, poss)) = 
            let info = ">Graph" + i.ToString() + ".Subgraph" + j.ToString()+ "."// Time: "+ time.ToString()
            //let toPrint = strings |> List.collect (fun (line:string) -> splitLine line)
            let startfinal =
                poss
                |> Seq.map (fun (sp,fp,l) -> 
                                if startE = finalE then 
                                    (pathToString [startE]).Substring (sp,(fp-sp+1)),
                                    "",l
                                else 
                                    (pathToString [startE]).Substring sp,
                                    (pathToString [finalE]).Substring (0,fp),l)
            let lines =
                if Seq.length paths = 0 then
                    startfinal
                    |> Seq.map (fun (start,final,l) -> start + final)
                else
                    paths
                    |> Seq.collect (fun path ->
                        startfinal
                        |> Seq.choose (fun (start,final,l) -> 
                        let line = pathToString path
                        if line.Length = int l then 
                            start + line + final |> Some
                        else None))
            let index = ref 0
            let toPrint = 
                lines
                |> Seq.collect (fun line -> let header = info + (!index).ToString()
                                            index := !index + 1
                                            header::(splitLine line))
            File.AppendAllLines(resultPath, toPrint)
            isPrinted := true
        res
        |> Seq.iteri printResult
        !isPrinted

    let agent name  =
        MailboxProcessor.Start(fun inbox ->
            let rec loop n =
                async {
                        let! msg = inbox.Receive()
                        match msg with
                        | Data (i,graph) ->
                            try
                                printfn "\nSearch in agent %A. Graph %A." name i
                                printfn "Vertices: %A Edges: %A" graph.VertexCount graph.EdgeCount
                                let parseResult = searchCfg.SearchWithoutSPPF graph searchCfg.StartNonterm
                                printfn "SearchWithoutSPPF done"
                                let res = parseResult |> filterRnaParsingResult graph searchCfg.LowLengthLimit searchCfg.HighLengthLimit
                                //let subgraphsResults = processSubgraphs g s f
                                //printResult subgraphsResults i
                                if not <| printResultGraphs res i searchCfg.NumToString then
                                    printfn "Nothing found."
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
    
    new SearchConfig(GLL.tRNA.buildAbstract, GLL.tRNA.buildAbstractAst, getSmb, 120, 60, 4, GLL.tRNA.numToString)

let r16s_H22_H23_SearchConfig =

    let getSmb =
        let cnt = ref 0
        fun ch ->
            let i = incr cnt; !cnt 
            match ch with
            | 'A' | 'a' -> GLL.r16s.H22_H23.A i                
            | 'U' | 'u' 
            | 'T' | 't' -> GLL.r16s.H22_H23.U i
            | 'C' | 'c' -> GLL.r16s.H22_H23.C i
            | 'G' | 'g' -> GLL.r16s.H22_H23.G i
            | _ -> GLL.r16s.H22_H23.U i
            | x ->   failwithf "Strange symbol in input: %A" x
            |> GLL.r16s.H22_H23.tokenToNumber
    
    new SearchConfig(GLL.r16s.H22_H23.buildAbstract, GLL.r16s.H22_H23.buildAbstractAst, getSmb, 
                        //300, 550, 14, GLL.r16s.H22_H23.numToString)
                        318, 370, 14, GLL.r16s.H22_H23.numToString)
                        //400, 550, 14, GLL.r16s.H22_H23.numToString)
                        //800, 910, 14, GLL.r16s.H22_H23.numToString)

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
    
    new SearchConfig(GLL.shift_problem.buildAbstract, GLL.shift_problem.buildAbstractAst, getSmb, 100, 0, 1, GLL.shift_problem.numToString)

let searchMain path what agentsCount =
    let searchCfg = 
        match what with
        | TRNA -> tRNASearchConfig
        | R16S_H22_H23 -> r16s_H22_H23_SearchConfig
        | Shift_problem -> shift_problem_SearchConfig

    let graphs, longEdges = loadGraphFormFileToBioParserInputGraph path searchCfg.HighLengthLimit searchCfg.Tokenizer (GLL.tRNA.RNGLR_EOF 0)

    let graphs = 
        //graphs.[1500..]
        //graphs.[5000..5050]
        //graphs.[4071..4072]
        graphs.[114..]
    searchInBioGraphs searchCfg graphs agentsCount
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
 //   searchTRNA inputGraphPath agentsCount
    searchMain inputGraphPath R16S_H22_H23 agentsCount
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
    
//    let path = "D:/YC/YaccConstructor/Tests/bio/problem_with_shift_2/g"
//    let inputGraphPath = System.IO.Path.Combine(System.IO.Path.GetDirectoryName path, System.IO.Path.GetFileNameWithoutExtension path)
//    searchMain inputGraphPath Shift_problem 1
//    //let parseBioGraph = 
    0
