module YC.Bio.RNA.Search

open Argu

open YC.BIO.BioGraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput

open YC.Bio.RNA.tblReader
open YC.Bio.RNA.IO

open Microsoft.FSharp.Collections
open System.IO
open System.Collections.Generic

open MBrace.Azure
open MBrace.Core
open MBrace.Core.Builders
open MBrace.Core.CloudOperators
open MBrace.Runtime
//open GLL.shift_problem
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

let filterRnaParsingResult (graph:BioParserInputGraph) lowLengthLimit highLengthLimit (res : ResultStruct []) =
    //let hihtLenghtLimit = 500.0
    let hihtLenghtLimit = float highLengthLimit
    let weightLimit = 10000
    // (leftEdge, rightEdge) , (leftPosition, rightPosition, lengthOfPath)
    let filteredByLength = 
        res 
        |> Array.filter (fun i -> i.length >= uint16 lowLengthLimit && i.length <= uint16 highLengthLimit)
        |> Array.groupBy(fun x -> x.le, x.re)
        //|> Seq.map(fun (_,a) -> a |> Seq.maxBy (fun x -> x.length))
        |> Array.map(fun (edg,a) -> edg, a |> Array.collect (fun res -> [|res.lpos, res.rpos, res.length|]))
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
        if Seq.isEmpty paths then failwith "No path found."
        let filteredPaths = paths |> Seq.filter (fun p -> p |> Seq.sumBy (fun e -> e.Tag) |> uint16 |> lengths.Contains)
        if Seq.isEmpty filteredPaths then failwith "No path with expected length found."
        filteredPaths |> Seq.toList
        
        
    let subgraphs =
        filteredByLength
        |> Array.choose (fun ((le, re), poss) ->
            let lengths0 = new ResizeArray<_>()
            let newPoss = 
                poss
                |> Array.map (fun (lpos,rpos,pathLength) ->
                    let leftEdgePartOfPath = uint16 <| graph.Edges.[le].RealLenght - lpos// - 1
                    let rightEdgePartOfPath = uint16 <| rpos + 1
                    pathLength - leftEdgePartOfPath - rightEdgePartOfPath |> lengths0.Add
                    lpos,rpos,pathLength - leftEdgePartOfPath - rightEdgePartOfPath)
                |> Array.groupBy (fun (lpos,rpos,pathLength) -> pathLength)
                |> Array.map (fun (pathLength,values) ->
                    let mostLeft = ref graph.Edges.[re].RealLenght
                    let mostRight= ref -1
                    values
                    |> Array.iter (fun (lpos,rpos,_) -> 
                                    if lpos < !mostLeft then mostLeft := lpos
                                    if rpos > !mostRight then mostRight := rpos)
                    !mostLeft, !mostRight, pathLength)
            let toReturn = graph.Edges.[le], graph.Edges.[re], newPoss

            if le = re || graph.Edges.[le].End = graph.Edges.[re].Start
            then ([], toReturn)
                    |> Some
            elif not <| subgraphsMemoization graph.Edges.[le].End graph.Edges.[re].Start
            then
                let lengths = new HashSet<uint16>(lengths0)
                let s = findSubgraph graph.Edges.[le].End graph.Edges.[re].Start lengths

                (s, toReturn)
                |> Some
            else None
            )

    let adjacentEdgs = new SysDict<_,ResizeArray<_>>()
    graph.Edges
    |> Array.iter (
        fun e -> 
            let flg, v = adjacentEdgs.TryGetValue(e.Start)
            if flg
            then v.Add e
            else adjacentEdgs.Add(e.Start, new ResizeArray<_>([|e|]))
            )
    
    let edgesOfQgToPaths (paths : IEnumerable<QuickGraph.TaggedEquatableEdge<int,float>> list)= 
        let finalRes = new List<List<_>>()
        paths
        |> List.iter(fun edgesOfPath ->
            let result = new List<List<_>>()
            for edge in edgesOfPath do
                let edges = 
                    adjacentEdgs.[edge.Source]
                    |> ResizeArray.filter (fun (x:BioParserEdge) -> x.End = edge.Target && x.Tokens.Length = int edge.Tag)
                match edges.Count with
                | 0 ->  failwith "found no such edges in original graph"
                | 1 ->  if result.Count = 0
                        then 
                            result.Add (new List<_>([edges.[0]]))
                        else
                            result
                            |> ResizeArray.iter (fun x -> x.Add edges.[0])
                | n ->  let count = result.Count
                        if count = 0
                        then 
                            for edge in edges do
                                result.Add (new List<_>([edge]))
                        else
                            for i in 0..count-1 do
                                for j in 1..edges.Count-1 do
                                    result.Add (ResizeArray.concat [result.[i];new List<_>([edges.[j]])])
                                result.[i].Add edges.[0]
            finalRes.AddRange result)
        finalRes
        |> ResizeArray.distinctBy (fun ra -> ResizeArray.toArray ra)

    subgraphs
    |> Array.map (fun (paths, toReturn) ->
        edgesOfQgToPaths paths, toReturn)

let searchInBioGraphs (searchCfg : SearchConfig) graphs agentsCount =
    let start = System.DateTime.Now

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

                                //             debug
                                //printfn ""
                                //graph.Edges.[717].Tokens
                                //|> Array.iter (fun x -> printf "%s" <| searchCfg.NumToString x)
                                //printfn ""

                                let parseResult = searchCfg.SearchWithoutSPPF graph// searchCfg.StartNonterm
                                printfn "SearchWithoutSPPF done"
                                match parseResult with
                                | Success1 result -> 
                                    let res = result |> filterRnaParsingResult graph searchCfg.LowLengthLimit searchCfg.HighLengthLimit
                                    //let subgraphsResults = processSubgraphs g s f
                                    //printResult subgraphsResults i
                                    printPathsToFASTA ".\\result.fa" res i searchCfg.NumToString
                                | Success _ -> failwith "Result is success but it is unexpected success"
                                | Error e -> printfn "Input parsing failed: %A" e
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

let r16s_H22_H23_SearchConfig =

    let getSmb =
        let cnt = ref 0
        fun ch ->
            let i = incr cnt; !cnt 
            match ch with
            | 'A' | 'a' -> GLL.r16s.H22_H23.A ()                
            | 'U' | 'u' 
            | 'T' | 't' -> GLL.r16s.H22_H23.U ()
            | 'C' | 'c' -> GLL.r16s.H22_H23.C ()
            | 'G' | 'g' -> GLL.r16s.H22_H23.G ()
            | _ -> GLL.r16s.H22_H23.U ()
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
                        //800, 910, 14, GLL.r16s.H22_H23.numToString)

let searchMain path what agentsCount =
    let searchCfg = 
    
        r16s_H22_H23_SearchConfig

    let graphs, longEdges = loadGraphFormFileToBioParserInputGraph path searchCfg.HighLengthLimit searchCfg.Tokenizer// (GLL.tRNA.RNGLR_EOF 0)

    //printfn "dsdsdf %A" graphs.Length

    let workingDir = System.AppDomain.CurrentDomain.BaseDirectory + @"..\..\..\infernal\"
   
    let graphs = 
        graphs.[1..1]
        //graphs.[1500..]
        //graphs.[5000..5050]
        //graphs.[4071..4072]
        //graphs.[0..1]
    searchInBioGraphs searchCfg graphs agentsCount
    |> printfn "%A"
    //searchInCloud graphs
    ()

//Divides input on edges 1 to 10 symbols
let toSmallEdges path = 
    let inputExt = ".txt"
    let lblsExt = ".sqn"
    let graphStrauctureExt = ".grp"
    let rnd = new System.Random()

    let splitLine (line1:string) =
            let rec inner (line:string) = 
                let subLineLength = rnd.Next(1,11)
                if line.Length <= subLineLength then [line] else
                (line.Substring (0, subLineLength))::(inner (line.Substring subLineLength))
            inner line1
            |> List.toArray
    let strings = 
        File.ReadAllLines(path + inputExt)
        |> Array.collect splitLine
        |> List.ofArray

    let toPrint = 
        strings
        |> List.mapi (fun i line -> ">" + (i+1).ToString() + "\n" + line)
    
    let rec getVertexList i =
        if i = 0 then [] else
        getVertexList (i-1)@[("Vertex "+ i.ToString() + " ~ 0 .")]
    
    let rec getEdgeList i =
        if i = 0 then [] else
        getEdgeList (i-1)@[("Edge "+ i.ToString() + " : " + i.ToString() + " -> " + (i+1).ToString() + ", l = " + (strings.[i-1].Length).ToString() + " ~ 0 .")]

    File.WriteAllLines(path + lblsExt, toPrint)
    File.WriteAllLines(path + graphStrauctureExt, getVertexList (strings.Length+1)@["\n"]@ getEdgeList strings.Length)

[<EntryPoint>]
let main argv = 
    //let qq = getTbl @"C:\CM\log.txt"
    //let path = @"C:\YCInfernal\Tests\bio\16s_1_small_edges\test"
    //toSmallEdges path
    let argParser = ArgumentParser.Create<CLIArguments>()
    let args = argParser.Parse argv
    let appSetting = argParser.ParseAppSettings (System.Reflection.Assembly.GetExecutingAssembly())
    let agentsCount =
        args.GetResult(<@Agents@>, defaultValue = appSetting.GetResult(<@Agents@>, defaultValue = 1))        
    let inputGraphPath = 
        args.GetResult <@Input@>
        |> (fun s ->
                System.IO.Path.Combine(System.IO.Path.GetDirectoryName s, System.IO.Path.GetFileNameWithoutExtension s))
 //   searchTRNA inputGraphPath agentsCount
    searchMain inputGraphPath R16S_H22_H23 agentsCount

    0
