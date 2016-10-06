module YC.Bio.RNA.SearchFSA

open Argu
open YC.BIO.BioGraphLoader
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput
open YC.Bio.RNA.Search.Structs
open YC.Bio.RNA.tblReader
open YC.Bio.RNA.IO
open Microsoft.FSharp.Collections
open System.IO
open System.Collections.Generic
open System.Diagnostics

let filterRnaParsingResult (graph : BioParserInputGraph) lowLengthLimit highLengthLimit (res : ResultStruct []) = 
    let filteredByLength = 
        res
        |> Array.filter (fun i -> i.length >= uint16 lowLengthLimit && i.length <= uint16 highLengthLimit)
        |> Array.groupBy (fun x -> x.le, x.re)
        |> Array.map (fun (edg, a) -> edg, a |> Array.collect (fun res -> [| res.lpos, res.rpos, res.length |]))
    
    let outEdges = 
        let r = Array.init<_> graph.VertexCount (fun _ -> new System.Collections.Generic.List<int>())
        for i in 0..graph.EdgeCount - 1 do
            r.[graph.Edges.[i].Start].Add i
        r
    
    let getPaths s e (lengths : HashSet<uint16>) = 
        let maxLength = 
            lengths
            |> Seq.max
            |> int
        
        let rec dfs start curLength = 
            let toReturn = new List<List<BioParserEdge>>()
            for edgeNum in outEdges.[start] do
                let edge = graph.Edges.[edgeNum]
                let newLength = curLength + edge.RealLength
                if edge.End = e && newLength
                                   |> uint16
                                   |> lengths.Contains
                then new List<_>([ edge ]) |> toReturn.Add
                if newLength < maxLength then 
                    let paths = dfs edge.End newLength
                    paths |> ResizeArray.iter (fun (x : List<_>) -> x.Add edge)
                    toReturn.AddRange paths
            toReturn
        
        let paths = dfs s 0 |> ResizeArray.map ResizeArray.rev
        if lengths.Contains 0us then new List<_>() |> paths.Add
        paths
    
    let subgraphs = 
        filteredByLength |> Array.map (fun ((le, re), poss) -> 
                                let lengths = new HashSet<uint16>()
                                
                                let newPoss = 
                                    poss
                                    |> Array.map (fun (lpos, rpos, pathLength) -> 
                                           let leftEdgePartOfPath = graph.Edges.[le].RealLength - lpos
                                           let rightEdgePartOfPath = rpos + 1
                                           let newLength = (int pathLength) - leftEdgePartOfPath - rightEdgePartOfPath
                                           lpos, rpos, 
                                           if rpos <= lpos && le = re then -1 * newLength
                                           elif newLength < 0 && le = re then 0
                                           else newLength)
                                    |> Array.groupBy (fun (_, _, len) -> len)
                                    |> Array.map (fun (len, values) -> 
                                           let mostLeft = ref graph.Edges.[le].RealLength
                                           let mostRight = ref -1
                                           
                                           let pathLength = 
                                               if len < 0 then len * -1
                                               else len
                                           lengths.Add(uint16 pathLength) |> ignore
                                           values |> Array.iter (fun (lpos, rpos, _) -> 
                                                         if lpos < !mostLeft then mostLeft := lpos
                                                         if rpos > !mostRight then mostRight := rpos)
                                           !mostLeft, !mostRight, pathLength)
                                
                                let toReturn = graph.Edges.[le], graph.Edges.[re], newPoss
                                let s = getPaths graph.Edges.[le].End graph.Edges.[re].Start lengths
                                if ResizeArray.isEmpty s 
                                   && not (graph.Edges.[le].End = graph.Edges.[re].Start || le = re) then 
                                    failwith "No paths with expected length found."
                                else s, toReturn)
    
    subgraphs

let searchInBioGraphs (searchCfg : SearchConfig) graphs agentsCount = 
    let start = System.DateTime.Now
    
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
                            let parseResult = searchCfg.SearchWithoutSPPF graph
                            //             debug
                            (*printfn ""

                            graph.Edges
                            |> Array.iter (fun x ->
                                x.Tokens
                                |> Array.iter (searchCfg.NumToString >> printf "%s")
                                printfn "")
                            *)
                            (*graph.Edges.[55].Tokens
                            |> Array.iter (searchCfg.NumToString >> printf "%s")
                            printfn ""
                            graph.Edges.[57].Tokens
                            |> Array.iter (searchCfg.NumToString >> printf "%s")
                            printfn ""
                            graph.Edges.[58].Tokens
                            |> Array.iter (searchCfg.NumToString >> printf "%s")
                            printfn ""*)

                            match parseResult with
                            | Success1 result -> 
                                printfn "SearchWithoutSPPF succeed"
                                let res = 
                                    result 
                                    |> filterRnaParsingResult graph searchCfg.LowLengthLimit searchCfg.HighLengthLimit
                                printPathsToFASTA (".\\" + searchCfg.OutFileName) res i searchCfg.NumToString
                            | Success _ -> failwith "Result is success but it is unexpected success"
                            | Error _ -> printfn "Input parsing failed."
                        with e -> printfn "ERROR in bio graph parsing! %A" e.Message
                        return! loop n
                    | Die ch -> ch.Reply()
                }
            loop 0)
    
    let agents = Array.init agentsCount (sprintf "searchAgent%A" >> agent)
    graphs |> Array.iteri (fun i graph -> Data(i, graph) |> agents.[i % agentsCount].Post)
    agents |> Array.iter (fun a -> a.PostAndReply Die)
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

let getSubgraphs (edges : BioGraphEdge []) (startEdges : Dictionary<int,int>) (lengthLimit : int) (lengthOfBeginning : int) = 
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
    
    let getSubGraph (edge : BioGraphEdge) (posOnEdge : int) = 
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

let filterResultWithInfernal filename = 
    let workingDir = System.AppDomain.CurrentDomain.BaseDirectory + @"..\..\..\infernal\"
    let resultFileFullName = System.AppDomain.CurrentDomain.BaseDirectory + filename
    
    let runInfernal() = 
        let startInfo = new ProcessStartInfo()
        startInfo.FileName <- "cmscan.exe"
        startInfo.Arguments <- sprintf "--anytrunc --noali --tblout log.txt archaea.cm \"%s\"" resultFileFullName
        startInfo.WorkingDirectory <- workingDir
        Process.Start(startInfo).WaitForExit()
    //runInfernal()
    let tbl = getTbl (workingDir + "log.txt")
    let graphNameToBegEnd = new Dictionary<_, _>()
    File.ReadAllLines(resultFileFullName)
    |> Array.pairwise
    |> Array.iter (fun (name, _) -> // name >Graph0.Subgraph16.1 19239023:61 -> 19005610:68
           if name.[0] = '>' then 
               let names = name.Substring(1).Split([| ' '; ':' |])
               graphNameToBegEnd.Add(names.[0], (int (names.[1]), int (names.[2]))))
    //    let toWrite =
    //        tbl
    //        |> Array.map (fun (s, i, j) -> sprintf "%s %i %i" s i j)
    //
    //    File.WriteAllLines(workingDir + "tbl.txt", toWrite)
    let result = new Dictionary<_,_>()
    tbl |> Array.iter (fun (s, _, _) -> 
        let cond, (edgeN, edgePos) = graphNameToBegEnd.TryGetValue(s)
        if cond
        then 
            let c,v = result.TryGetValue(edgeN)
            if c
            then 
                if v > edgePos
                then
                    result.Remove(edgeN) |> ignore
                    result.Add(edgeN, edgePos)
            else
                result.Add(edgeN, edgePos)
        else failwith "Name of subgraph wasn't found")
    result

let searchMiddle path agentsCount = 
    let searchCfg = FSA_R16S_19_27_SearchConfig
    let sourceGraph, graphs, longEdges = 
        loadGraphFormFileToBioParserInputGraph path 545(*searchCfg.HighLengthLimit*) searchCfg.Tokenizer // (GLL.tRNA.RNGLR_EOF 0)
    //printLongEdges @"C:\CM\long_edges.fa" longEdges
    let graphs = 
        //graphs.[435..435]
        graphs.[5..]
    //graphs.[1500..]
    //graphs.[5000..5050]
    //graphs.[4071..4072]
    //graphs.[0..1]

    //searchInBioGraphs searchCfg graphs agentsCount |> printfn "%A"
    sourceGraph

let searchBeginning agentsCount sourceGraph = 
    let searchCfg = FSA_R16S_1_18_SearchConfig
    
    let convert (g : HashSet<BioGraphEdge> * HashSet<int>) = 
        let startEdges = new ResizeArray<_>()
        
        let edges = 
            let eList = new ResizeArray<_>()
            let edges, edgesToBegin = g
            edges|> Seq.iteri (fun i edge -> 
                         let tag = edge.Tag.str.ToCharArray() |> Array.map searchCfg.Tokenizer
                         let newEdge = 
                             new BioParserEdge(edge.Source, edge.Target, edge.Tag.length, tag, edge.Tag.id, 
                                               edge.Tag.sourceStartPos)
                         eList.Add(newEdge)
                         if edgesToBegin.Contains(edge.Tag.id) then startEdges.Add(i))
            eList.ToArray()
        new BioParserInputGraph(edges, startEdges |> Set)
    
    let startEdges = filterResultWithInfernal "R16S_19_27_result.fa"
    let subgraphs = 
        getSubgraphs sourceGraph startEdges searchCfg.HighLengthLimit searchCfg.LengthOfBeinning
    
    let subgraphs = 
        subgraphs
        |> Array.map convert
    printfn "\nSubgraphs count: %A.\n" subgraphs.Length
    searchInBioGraphs searchCfg subgraphs agentsCount |> printfn "%A"

let searchMain path agentsCount = 
    let sourceGraph = searchMiddle path agentsCount
    searchBeginning agentsCount sourceGraph
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
