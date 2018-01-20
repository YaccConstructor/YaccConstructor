open System.IO
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.Common.ASTGLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon
open YC.API
open Yard.Frontends.YardFrontend
open Yard.Generators.GLL
open Yard.Core.Conversions.ExpandMeta

open System.Collections.Generic
open System.Linq
open System
open System.Threading

open QuickGraph.Graphviz

open YC.GLL.SPPF


let getParserSource grammarFile conv = 
    let fe = new YardFrontend()
    let gen = new GLL()
    generate ((*grammarsDir + *)grammarFile)
             fe gen 
             None
             conv
             [|""|]
             [] :?> ParserSourceGLL

let parserSources : ParserSourceGLL [] = 
    let conv = [new ExpandMeta()]
    //[||]
    [|for fileNumber in 1..98 -> getParserSource (sprintf "../../grammars/BaselineDomain-%i.yrd" fileNumber ) conv |]

let myPC =
    let conv = [new ExpandMeta()]
    [|getParserSource "../../mygrammar1.yrd" conv; getParserSource "../../mygrammar2.yrd" conv |]

let inputs =
    [| for fileNumber in 1..98 -> 
        let input = File.ReadAllLines(sprintf "../../data/Observations-%i.txt" fileNumber) |> Array.map (fun (x : string) -> x.Split([| |]).[1])
        [| for ps in parserSources ->
                let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
                shuffleInput.AddTokens(input)
                shuffleInput |] |]

let notFullInputs =
    [| for fileNumber in 1..98 -> 
        let input = File.ReadAllLines(sprintf "../../data/Observations-%i.txt" fileNumber) |> Array.map (fun (x : string) -> x.Split([| |]).[1])
        input,
        [| for ps in parserSources ->
                let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
                shuffleInput.AddTokens(input.[0..0])
                shuffleInput |] |]

let shuffledInput = 
    let input = [| "A75"; "A14"; "A99"; "A68"; "A14"; "A65" ;"A64" ; "A66" ; "A7" ; "A74"; "A89"; "A56"; "A37"; "A43"; "A78"; "A88"; "A58"; "A33" |]
    [| for ps in parserSources ->
        let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
        shuffleInput.AddTokens(input)
        shuffleInput |]

let myShuffledInput = 
    let input = [|"A75";"A25";"A29";"A14"|]
    [| for ps in myPC ->
        let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
        shuffleInput.AddTokens(input)
        shuffleInput |]

let shuffleLines (lines : string [] []) = 
    let shuffled = new ResizeArray<_>()
    let length = lines |> Array.fold (fun sum x -> sum + x.Length) 0
    let maxLength = lines |> Array.maxBy(fun x -> x.Length) |> (fun x -> x.Length)
   
    for i in [0..maxLength] do
        for line in lines do
            if line.Length > i
            then
                shuffled.Add(line.[i])

    shuffled
    |> Array.ofSeq

let shuffleEvery n lines =
    lines
    |> Array.indexed
    |> Array.filter(fun (i,_) -> i % n = 0)
    |> Array.map(fun (_,x) -> x)
    |> (fun x -> x.Length, shuffleLines x)
    

let genInputs() =
    let linestoShuffle = 
        [| for fileNumber in 1..98 -> 
            File.ReadAllLines(sprintf "../../data/Observations-%i.txt" fileNumber) |> Array.map (fun (x : string) -> x.Split([| |]).[1]) |]
    
    [|40..1|]
    |> Array.map(fun i -> shuffleEvery i linestoShuffle)
    |> Array.distinctBy(fun (x,_) -> x)
    |> Array.map(fun (l,x) ->
        l,[| for ps in parserSources ->
                let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
                shuffleInput.AddTokens(x)
                shuffleInput |])

type Msg = 
    | Data of int * ShuffleInputGraph<string>
    | Die of AsyncReplyChannel<unit>

let baseTestNoShuffleAgents() = 
    let times = new List<_>()
    let numberOfShuffled = ref 0

    for input in inputs do
        let startTime = System.DateTime.UtcNow
        let agent name = 
            MailboxProcessor.Start(fun inbox -> 
                let rec loop n = 
                    async { 
                        let! msg = inbox.Receive()
                        match msg with
                        | Data(i, input: ShuffleInputGraph<string>) -> 
                            try 
                                let parser = new Parser(parserSources.[i])
                                ()//parser.GetPrefixTree(input) |> ignore
                            with e -> printfn "ERROR in parsing! %A" e.Message
                            return! loop n
                        | Die ch -> 
                            printfn "Graph parser agent %A finished!" name
                            ch.Reply()
                    }
                loop 0)
    
        let agents = Array.init 1 (sprintf "Agent%A" >> agent)
        let qToProcess = Queue<_>(input |> Array.indexed)
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

        times.Add((System.DateTime.UtcNow - startTime).TotalMilliseconds)
        printfn "Time: %A" times.[times.Count-1]

    printfn "Avg time: %A" (times.ToArray() |> Array.average)
    ()

let baseTestNoShuffleThreads() = 
    let times = new List<_>()
    let numberOfShuffled = ref 0

    for input in inputs do
        let numberOfThreads = 1
        let qToProcess = Array.init numberOfThreads (fun i -> Queue<_>())
        input
        |> Array.indexed
        |> Array.iter(fun (i,x) ->
            qToProcess.[i % numberOfThreads].Enqueue(parserSources.[i], x))
        
        let agent n = 
            let myQ = qToProcess.[n]
            let x = 
                new ThreadStart(fun x -> 
                                    while myQ.Count > 0 do
                                        let ps, input = myQ.Dequeue()
                                        let parser = new Parser(ps)
                                        ())//parser.GetPrefixTree(input) |> ignore)
            new Thread(x)
        let agents = Array.init numberOfThreads (agent)
                   
        let startTime = System.DateTime.UtcNow
        agents |> Array.iter(fun a -> a.Start())
        agents |> Array.iter(fun a -> a.Join())

        times.Add((System.DateTime.UtcNow - startTime).TotalMilliseconds)
        printfn "Time: %A" times.[times.Count-1]

    printfn "Avg time: %A" (times.ToArray() |> Array.average)
    ()

let toDot graph fileName =
            let printer = GraphvizAlgorithm(graph)
            printer.CommonVertexFormat.Shape <- Dot.GraphvizVertexShape.Ellipse
            printer.FormatEdge.Add(fun (e:FormatEdgeEventArgs<int,QuickGraph.TaggedEdge<int,int>>) -> e.EdgeFormatter.Label.Value <- sprintf "%i" e.Edge.Tag)//intToString.[e.Edge.Tag])
            printer.FormatVertex.Add(fun (v:FormatVertexEventArgs<int>) -> v.VertexFormatter.Label <- sprintf "%A" v.Vertex)  
            let str = printer.Generate()        
            
            System.IO.File.WriteAllText(fileName, str)

let baseTestNoShuffle() = 
    let times = Array.init 9 (fun x -> new List<_>())
    let numberOfShuffled = ref 0

    for line,inputs in notFullInputs do
        let parsers = 
            inputs
            |> Array.mapi(fun i x ->
                let p = new Parser(parserSources.[i])
                p.Initialise x true
                p)

        
        for i in 0..line.Length-1 do
            if i <> 0
            then
                for input in inputs do
                    input.AddTokens(line.[i..i])
            printfn "Length %i.Parsing..." (i+1)
            let startTime = System.DateTime.UtcNow
            let trees = 
                parsers
                |> Array.choose( fun parser -> parser.GetPrefixTree())
            printfn "Length %i.Parsing finished. Printing..." (i+1)
            trees
            |> Array.iteri(fun j (_,tree) ->
                toDot tree (sprintf "./trees/%iprefixTree%i.dot" i j)
                )
            printfn "Length %i.Printing finished. Merging..." (i+1)
            mergeSPPFS (i+1) trees
            printfn "Length %i.Merging finished. Done." (i+1)
            
            times.[i].Add((System.DateTime.UtcNow - startTime).TotalMilliseconds)
        
            printfn "Time: %A" times.[times.[i].Count-1]
            printfn "-----------------------------------------"
    //printfn "Avg time: %A" (times.ToArray() |> Array.average)
    ()

let test() =
    let times = new List<_>()
    let inputs = genInputs()

    for l, input in inputs do
        let startTime = System.DateTime.UtcNow
        let prefixTrees = 
            input
            |> Array.indexed
            |> Array.choose (fun (i,input) -> 
                let parser = new Parser(parserSources.[i])//parserSources.[grammarN])
                //parser.BuildAst inputs.[inputN].[grammarN] |> ignore
                None)//parser.GetPrefixTree(input))
        

        mergeSPPFS l prefixTrees
            
        times.Add(l, (System.DateTime.UtcNow - startTime).TotalMilliseconds)
        System.IO.File.AppendText(sprintf "Length %i. Time: %A" l times.[times.Count-1]) |> ignore
        ()



[<EntryPoint>]
let main argv = 
    baseTestNoShuffle()
    (*
    let times = new List<_>()

    for inputN in 0..0(*..97*) do
        let startTime = System.DateTime.UtcNow
        let prefixTrees = 
            [|0..97|]
            |> Array.choose (fun grammarN -> 
                let parser = new Parser(parserSources.[grammarN])//parserSources.[grammarN])
                //parser.BuildAst inputs.[inputN].[grammarN] |> ignore
                parser.GetPrefixTree(shuffledInput.[grammarN])//inputs.[inputN].[grammarN])
                (*try
                    let parser = new Parser(myPC.[grammarN])//parserSources.[grammarN])
                    //parser.BuildAst inputs.[inputN].[grammarN] |> ignore
                    parser.GetPrefixTrees(shuffledInput.[grammarN])//inputs.[inputN].[grammarN])
                with
                    | _ -> 
                        printfn "failed"
                        [||]//failwithf "err in input %i grammar %i" inputN grammarN
                *)
                //tree.AstToDot("qwe.dot")
                //printfn "Iter %i time %A" i (System.DateTime.Now - iterStartTime)
                )
        
        let toDot graph fileName (intToString: Dictionary<_,_> ) =
            let printer = GraphvizAlgorithm(graph)
            printer.CommonVertexFormat.Shape <- Dot.GraphvizVertexShape.Ellipse
            printer.FormatEdge.Add(fun (e:FormatEdgeEventArgs<int,QuickGraph.TaggedEdge<int,int>>) -> e.EdgeFormatter.Label.Value <- sprintf "%i" e.Edge.Tag)//intToString.[e.Edge.Tag])
            printer.FormatVertex.Add(fun (v:FormatVertexEventArgs<int>) -> v.VertexFormatter.Label <- sprintf "%A" v.Vertex)  
            let str = printer.Generate()        
            
            System.IO.File.WriteAllText(fileName, str)

//        for i in myPC.[1].IntToString do
//            printfn "qwe %A %A" i.Key i.Value

        prefixTrees
        |> Array.iteri(fun i (_,tree) ->
            toDot tree (sprintf "prefixTree%i.dot" i) parserSources.[i].IntToString
            )
        
        //printfn "Shuff inp: %A" (shuffledInput.[0].GetEdges())

        let length = 18//inputs.[inputN].[0].GetTokens().Count()
        mergeSPPFS length prefixTrees
            
        times.Add((System.DateTime.UtcNow - startTime).TotalMilliseconds)
        printfn "Iter %i. Time: %A" inputN times.[times.Count-1]
    
    printfn "Avg time %A" (Seq.average times)
    *)
//    let tree = buildAst parserSources.[0] inputs.[0]
//    tree.AstToDot("qwe.dot")
    0 // return an integer exit code
