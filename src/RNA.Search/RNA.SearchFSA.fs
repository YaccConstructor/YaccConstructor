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


let filterRnaParsingResult (graph:BioParserInputGraph) lowLengthLimit highLengthLimit (res : ResultStruct []) =
    let filteredByLength = 
        res 
        |> Array.filter (fun i -> i.length >= uint16 lowLengthLimit && i.length <= uint16 highLengthLimit)
        |> Array.groupBy(fun x -> x.le, x.re)
        |> Array.map(fun (edg,a) -> edg, a |> Array.collect (fun res -> [|res.lpos, res.rpos, res.length|]))
    
    let outEdges = 
        let r = Array.init<_> graph.VertexCount (fun _ -> new System.Collections.Generic.List<int>())
        for i in 0..graph.EdgeCount - 1 do
                r.[graph.Edges.[i].Start].Add i
        r

    let getPaths s e (lengths:HashSet<uint16>) = 
        let maxLength = lengths |> Seq.max |> int

        let rec dfs start curLength = 
            let toReturn = new List<List<BioParserEdge>>()

            for edgeNum in outEdges.[start] do
                let edge = graph.Edges.[edgeNum]
                let newLength = curLength + edge.RealLength

                if edge.End = e && newLength |> uint16 |> lengths.Contains
                then
                    new List<_>([edge]) |> toReturn.Add

                if newLength < maxLength
                then
                    let paths = dfs edge.End newLength
                    paths
                    |> ResizeArray.iter (fun (x : List<_>) -> 
                        x.Add edge)
                    toReturn.AddRange paths
                        
            toReturn
        
        let paths = 
            dfs s 0
            |> ResizeArray.map ResizeArray.rev
        if lengths.Contains 0us
        then
            new List<_>() |> paths.Add
        paths
        
    let subgraphs =
        filteredByLength
        |> Array.map (fun ((le, re), poss) ->
            let lengths = new HashSet<uint16>()
            let newPoss = 
                poss
                |> Array.map (fun (lpos,rpos,pathLength) ->
                    let leftEdgePartOfPath = graph.Edges.[le].RealLength - lpos
                    let rightEdgePartOfPath = rpos + 1
                    let newLength = (int pathLength) - leftEdgePartOfPath - rightEdgePartOfPath

                    lpos, rpos,
                        if rpos <= lpos && le = re
                        then
                            -1 * newLength
                        elif le = re
                        then
                            0
                        else
                            newLength)
                |> Array.groupBy (fun (_,_,len) -> len)
                |> Array.map (fun (len,values) ->
                    let mostLeft = ref graph.Edges.[le].RealLength
                    let mostRight = ref -1
                    let pathLength = if len < 0 then len * -1 else len

                    lengths.Add(uint16 pathLength) |> ignore

                    values
                    |> Array.iter (fun (lpos,rpos,_) -> 
                                    if lpos < !mostLeft then mostLeft := lpos
                                    if rpos > !mostRight then mostRight := rpos)
                    !mostLeft, !mostRight, pathLength)
            let toReturn = graph.Edges.[le], graph.Edges.[re], newPoss

            let s = getPaths graph.Edges.[le].End graph.Edges.[re].Start lengths
            if ResizeArray.isEmpty s && not (graph.Edges.[le].End = graph.Edges.[re].Start || le = re)
            then
                failwith "No paths with expected length found."
            else
                s, toReturn
            )
    subgraphs

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
                                let parseResult = searchCfg.SearchWithoutSPPF graph
                                
                                //             debug
                                printfn ""

                                graph.Edges
                                |> Array.iter (fun x ->
                                    x.Tokens
                                    |> Array.iter (searchCfg.NumToString >> printf "%s")
                                    printfn "")
                               
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
                                    printfn "SearchWithoutSPPF succed"
                                    let res = result |> filterRnaParsingResult graph searchCfg.LowLengthLimit searchCfg.HighLengthLimit
                                    printPathsToFASTA ".\\result.fa" res i searchCfg.NumToString
                                | Success _ -> failwith "Result is success but it is unexpected success"
                                | Error _ -> printfn "Input parsing failed." 
                            with
                            | e -> printfn "ERROR in bio graph parsing! %A" e.Message
                            return! loop n         
                        | Die ch -> ch.Reply()
                        }
            loop 0)

    let agents = Array.init agentsCount (sprintf "searchAgent%A" >> agent)

    graphs
    |> Array.iteri 
        (fun i graph -> 
            Data (i, graph) 
            |> agents.[i % agentsCount].Post
        )

    agents |> Array.iter (fun a -> a.PostAndReply Die)
    printfn "Total time = %A" (System.DateTime.Now - start)
    0

let printLongEdges path edges = 
    let toPrint =
        edges
        |> Array.mapi (fun i x -> [|">Long" + i.ToString();x|])
        |> Array.collect id

    File.AppendAllLines(path, toPrint)

let searchMain path what agentsCount =
    let searchCfg = FSA_r16s_H22_H23_SearchConfig

    let graphs, longEdges = loadGraphFormFileToBioParserInputGraph path searchCfg.HighLengthLimit searchCfg.Tokenizer// (GLL.tRNA.RNGLR_EOF 0)
    
    printLongEdges @"C:\CM\long_edges.fa" longEdges

    let workingDir = System.AppDomain.CurrentDomain.BaseDirectory + @"..\..\..\infernal\"
   
    //let graphs = 
        //graphs.[435..435]
    //    graphs.[20355..20355]
        //graphs.[1500..]
        //graphs.[5000..5050]
        //graphs.[4071..4072]
        //graphs.[0..1]
    searchInBioGraphs searchCfg graphs agentsCount
    |> printfn "%A"
    //searchInCloud graphs
    ()

/// Divides input on edges 1 to 10 symbols
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

let filterRes path (names : HashSet<_>) = 
    let lines = File.ReadAllLines(path)
    let buf =
        lines
        |> Array.pairwise 
    buf
    |> Array.filter (fun (name, str) -> names.Contains (name.Substring(1)))

let filterLongEdges path (names : Dictionary<_,_>) = 
    let lines = File.ReadAllLines(path)
    let buf =
        lines
        |> Array.pairwise 
    buf
    |> Array.collect (fun (name, str) ->
        let cond, value = names.TryGetValue (name.Substring(1))
        if cond
        then 
            [|name; str.Substring(fst value, snd value - fst value)|]
        else
            [||])

let printPairs path pairs = 
    let x = 
        pairs
        |> Array.collect (fun (x,y) -> [|x;y|])
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
    let agentsCount =
        args.GetResult(<@Agents@>, defaultValue = 1)//appSetting.GetResult(<@Agents@>, defaultValue = 1))        
    let inputGraphPath = 
        args.GetResult <@Input@>
        |> (fun s ->
                System.IO.Path.Combine(System.IO.Path.GetDirectoryName s, System.IO.Path.GetFileNameWithoutExtension s))
    searchMain inputGraphPath R16S_H22_H23 agentsCount
    0
