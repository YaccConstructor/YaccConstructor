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

open TrieToFormula

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

//let parserSources : ParserSourceGLL [] = 
//    let conv = [new ExpandMeta()]
//    //[||]
//    [|for fileNumber in 1..98 -> getParserSource (sprintf "../../grammars/BaselineDomain-%i.yrd" fileNumber ) conv |]
//
//let myPC =
//    let conv = [new ExpandMeta()]
//    [|getParserSource "../../mygrammar1.yrd" conv; getParserSource "../../mygrammar2.yrd" conv |]
//
//let inputs =
//    [| for fileNumber in 1..98 -> 
//        let input = File.ReadAllLines(sprintf "../../data/Observations-%i.txt" fileNumber) |> Array.map (fun (x : string) -> x.Split([| |]).[1])
//        [| for ps in parserSources ->
//                let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
//                shuffleInput.AddTokens(input)
//                shuffleInput |] |]
//
//let notFullInputs =
//    [| for fileNumber in 1..98 -> 
//        let input = File.ReadAllLines(sprintf "../../data/Observations-%i.txt" fileNumber) |> Array.map (fun (x : string) -> x.Split([| |]).[1])
//        input,
//        [| for ps in parserSources ->
//                let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
//                shuffleInput.AddTokens(input.[0..0])
//                shuffleInput |] |]
//
//let shuffledInput = 
//    let input = [| "A75"; "A14"; "A99"; "A68"; "A14"; "A65" ;"A64" ; "A66" ; "A7" ; "A74"; "A89"; "A56"; "A37"; "A43"; "A78"; "A88"; "A58"; "A33" |]
//    [| for ps in parserSources ->
//        let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
//        shuffleInput.AddTokens(input)
//        shuffleInput |]
//
//let myShuffledInput = 
//    let input = [|"A75";"A25";"A29";"A14"|]
//    [| for ps in myPC ->
//        let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
//        shuffleInput.AddTokens(input)
//        shuffleInput |]
//
//let shuffleLines (lines : string [] []) = 
//    let shuffled = new ResizeArray<_>()
//    let length = lines |> Array.fold (fun sum x -> sum + x.Length) 0
//    let maxLength = lines |> Array.maxBy(fun x -> x.Length) |> (fun x -> x.Length)
//   
//    for i in [0..maxLength] do
//        for line in lines do
//            if line.Length > i
//            then
//                shuffled.Add(line.[i])
//
//    shuffled
//    |> Array.ofSeq
//
//let shuffleEvery n lines =
//    lines
//    |> Array.indexed
//    |> Array.filter(fun (i,_) -> i % n = 0)
//    |> Array.map(fun (_,x) -> x)
//    |> (fun x -> x.Length, shuffleLines x)
//    
//
//let genInputs() =
//    let linestoShuffle = 
//        [| for fileNumber in 1..98 -> 
//            File.ReadAllLines(sprintf "../../data/Observations-%i.txt" fileNumber) |> Array.map (fun (x : string) -> x.Split([| |]).[1]) |]
//    
//    [|40..1|]
//    |> Array.map(fun i -> shuffleEvery i linestoShuffle)
//    |> Array.distinctBy(fun (x,_) -> x)
//    |> Array.map(fun (l,x) ->
//        l,[| for ps in parserSources ->
//                let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
//                shuffleInput.AddTokens(x)
//                shuffleInput |])
//
//type Msg = 
//    | Data of int * ShuffleInputGraph<string>
//    | Die of AsyncReplyChannel<unit>
//
//let baseTestNoShuffleAgents() = 
//    let times = new List<_>()
//    let numberOfShuffled = ref 0
//
//    for input in inputs do
//        let startTime = System.DateTime.UtcNow
//        let agent name = 
//            MailboxProcessor.Start(fun inbox -> 
//                let rec loop n = 
//                    async { 
//                        let! msg = inbox.Receive()
//                        match msg with
//                        | Data(i, input: ShuffleInputGraph<string>) -> 
//                            try 
//                                let parser = new Parser(parserSources.[i])
//                                ()//parser.GetPrefixTree(input) |> ignore
//                            with e -> printfn "ERROR in parsing! %A" e.Message
//                            return! loop n
//                        | Die ch -> 
//                            printfn "Graph parser agent %A finished!" name
//                            ch.Reply()
//                    }
//                loop 0)
//    
//        let agents = Array.init 1 (sprintf "Agent%A" >> agent)
//        let qToProcess = Queue<_>(input |> Array.indexed)
//        while qToProcess.Count > 0 do
//            agents
//            |> Array.iter (fun a ->
//                if a.CurrentQueueLength < 10
//                then 
//                    for i in 0..9 do
//                        if qToProcess.Count > 0 
//                        then
//                            let (i,graph) = qToProcess.Dequeue()
//                            Data(i, graph) |> a.Post)            
//
//        agents |> Array.iter (fun a -> a.PostAndReply Die)
//
//        times.Add((System.DateTime.UtcNow - startTime).TotalMilliseconds)
//        printfn "Time: %A" times.[times.Count-1]
//
//    printfn "Avg time: %A" (times.ToArray() |> Array.average)
//    ()
//
//let baseTestNoShuffleThreads() = 
//    let times = new List<_>()
//    let numberOfShuffled = ref 0
//
//    for input in inputs do
//        let numberOfThreads = 1
//        let qToProcess = Array.init numberOfThreads (fun i -> Queue<_>())
//        input
//        |> Array.indexed
//        |> Array.iter(fun (i,x) ->
//            qToProcess.[i % numberOfThreads].Enqueue(parserSources.[i], x))
//        
//        let agent n = 
//            let myQ = qToProcess.[n]
//            let x = 
//                new ThreadStart(fun x -> 
//                                    while myQ.Count > 0 do
//                                        let ps, input = myQ.Dequeue()
//                                        let parser = new Parser(ps)
//                                        ())//parser.GetPrefixTree(input) |> ignore)
//            new Thread(x)
//        let agents = Array.init numberOfThreads (agent)
//                   
//        let startTime = System.DateTime.UtcNow
//        agents |> Array.iter(fun a -> a.Start())
//        agents |> Array.iter(fun a -> a.Join())
//
//        times.Add((System.DateTime.UtcNow - startTime).TotalMilliseconds)
//        printfn "Time: %A" times.[times.Count-1]
//
//    printfn "Avg time: %A" (times.ToArray() |> Array.average)
//    ()
//
//let toDot graph fileName =
//            let printer = GraphvizAlgorithm(graph)
//            printer.CommonVertexFormat.Shape <- Dot.GraphvizVertexShape.Ellipse
//            printer.FormatEdge.Add(fun (e:FormatEdgeEventArgs<int,QuickGraph.TaggedEdge<int,int>>) -> e.EdgeFormatter.Label.Value <- sprintf "%i" e.Edge.Tag)//intToString.[e.Edge.Tag])
//            printer.FormatVertex.Add(fun (v:FormatVertexEventArgs<int>) -> v.VertexFormatter.Label <- sprintf "%A" v.Vertex)  
//            let str = printer.Generate()        
//            
//            System.IO.File.WriteAllText(fileName, str)
//
//let baseTestNoShuffle() = 
//    let times = Array.init 9 (fun x -> new List<_>())
//    let numberOfShuffled = ref 0
//
//    for line,inputs in notFullInputs do
//        let parsers = 
//            inputs
//            |> Array.mapi(fun i x ->
//                let p = new Parser(parserSources.[i])
//                p.Initialise x true
//                p)
//
//        
//        for i in 0..line.Length-1 do
//            if i <> 0
//            then
//                for input in inputs do
//                    input.AddTokens(line.[i..i])
//            printfn "Length %i.Parsing..." (i+1)
//            let startTime = System.DateTime.UtcNow
//            let trees = 
//                parsers
//                |> Array.choose( fun parser -> parser.GetPrefixTree())
//            printfn "Length %i.Parsing finished. Printing..." (i+1)
//            trees
//            |> Array.iteri(fun j (_,tree) ->
//                toDot tree (sprintf "./trees/%iprefixTree%i.dot" i j)
//                )
//            printfn "Length %i.Printing finished. Merging..." (i+1)
//            mergeSPPFS (i+1) trees
//            printfn "Length %i.Merging finished. Done." (i+1)
//            
//            times.[i].Add((System.DateTime.UtcNow - startTime).TotalMilliseconds)
//        
//            printfn "Time: %A" times.[times.[i].Count-1]
//            printfn "-----------------------------------------"
//    //printfn "Avg time: %A" (times.ToArray() |> Array.average)
//    ()
//
//let test() =
//    let times = new List<_>()
//    let inputs = genInputs()
//
//    for l, input in inputs do
//        let startTime = System.DateTime.UtcNow
//        let prefixTrees = 
//            input
//            |> Array.indexed
//            |> Array.choose (fun (i,input) -> 
//                let parser = new Parser(parserSources.[i])//parserSources.[grammarN])
//                //parser.BuildAst inputs.[inputN].[grammarN] |> ignore
//                None)//parser.GetPrefixTree(input))
//        
//
//        mergeSPPFS l prefixTrees
//            
//        times.Add(l, (System.DateTime.UtcNow - startTime).TotalMilliseconds)
//        System.IO.File.AppendText(sprintf "Length %i. Time: %A" l times.[times.Count-1]) |> ignore
//        ()
let testSimpleProgram () =     
    let parserSources : ParserSourceGLL [] = 
        let conv = [new ExpandMeta()]
        [|for fileNumber in 0..1 -> getParserSource "../../gammarFromPda.yrd" conv |]
    //A25 A29 A83 A65 A64 A66 A75 A14 A99
    //let input = [| "A75"; "A14"; "A99"; "A68"; "A14"; "A65" ;"A64" ; "A66" ; "A7" |]//; "A74"; "A89"; "A56"; "A37"; "A43"; "A78"; "A88"; "A58"; "A33" |]
    let input = [| "If0"; "If0"; "App"; "App"; "If1"; "If1"; "Err"; "Err"|]
    //let inputToNum = new Dictionary<_,_>()
    //input |> Array.iteri(fun i x -> inputToNum.Add(x, i))
    let timeInit = System.DateTime.UtcNow;
    let shuffledInput = 
        [| for ps in parserSources ->
            let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
            shuffleInput.AddTokens(input)
            shuffleInput |]

    let tries = 
        parserSources
        |> Array.mapi (fun i ps ->
            let parser = new Parser(parserSources.[i])
            parser.Parse shuffledInput.[i] true
            parser.GetPrefixTree(), i
            )
    
    let usedVars = new List<_>()

    let sppfFormulas = 
        tries
        |> Array.filter (fun (x,i) -> x.IsSome)
        |> Array.map (fun (x,i) -> 
            let beg,trie = x.Value
            let formula, vars = trieToFormula trie beg input i
            usedVars.Add(vars)
            formula)

    //printfn "Number of tries: %i" sppfFormulas.Length

    let xors = Array.init input.Length (fun x -> new List<_>())

    let genXors (vars : List<string[]>) =
        for v in vars do
            v
            |> Seq.iteri(fun i x -> xors.[i].Add(x))//String.Join(" ", x))
        
        xors
        |> Array.map(fun x -> String.Join(" ", x))
        |> (fun x -> String.Join("\n", x))
    
    let finalFormula =        
        //Array.append sppfFormulas xors
        sppfFormulas
        |> AND
    printfn "Time: %A" (System.DateTime.UtcNow - timeInit)
    System.IO.File.WriteAllText("myFormula.txt", finalFormula.ToString())
    System.IO.File.WriteAllText("myFormulaXORS.txt", genXors usedVars)


let testBaseLineDomainGrammars () =     
    let parserSources : ParserSourceGLL [] = 
        let conv = [new ExpandMeta()]
        [|for fileNumber in 1..98 -> getParserSource (sprintf "../../grammarsNew/BaselineDomain-%i.yrd" fileNumber ) conv |]
    //A25 A29 A83 A65 A64 A66 A75 A14 A99
    //let input = [| "A75"; "A14"; "A99"; "A68"; "A14"; "A65" ;"A64" ; "A66" ; "A7" |]//; "A74"; "A89"; "A56"; "A37"; "A43"; "A78"; "A88"; "A58"; "A33" |]
    let input = [| "A25"; "A29"; "A83"; "A65"; "A64"; "A66"; "A75"; "A14"; "A99" |]
    let timeInit = System.DateTime.UtcNow;
    let shuffledInput = 
        [| for ps in parserSources ->
            let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
            shuffleInput.AddTokens(input)
            shuffleInput |]

    let tries = 
        parserSources
        |> Array.mapi (fun i ps ->
            let parser = new Parser(parserSources.[i])
            parser.Parse shuffledInput.[i] true
            parser.GetPrefixTree(), i
            )
    
    let usedVars = new List<_>()

    let sppfFormulas = 
        tries
        |> Array.filter (fun (x,i) -> x.IsSome)
        |> Array.map (fun (x,i) -> 
            let beg,trie = x.Value
            let formula, vars = trieToFormula trie beg input i
            usedVars.Add(vars)
            formula)

    //printfn "Number of tries: %i" sppfFormulas.Length

    let xors = Array.init input.Length (fun x -> new List<_>())

    let genXors (vars : List<string[]>) =
        for v in vars do
            v
            |> Seq.iteri(fun i x -> xors.[i].Add(x))//String.Join(" ", x))
        
        xors
        |> Array.map(fun x -> String.Join(" ", x))
        |> (fun x -> String.Join("\n", x))
    
    let finalFormula =        
        //Array.append sppfFormulas xors
        sppfFormulas
        |> AND
    printfn "Time: %A" (System.DateTime.UtcNow - timeInit)
    System.IO.File.WriteAllText("myFormula.txt", finalFormula.ToString())
    System.IO.File.WriteAllText("myFormulaXORS.txt", genXors usedVars)

let testGraphParsing () =     
    let parserSources : ParserSourceGLL [] = 
        let conv = [new ExpandMeta()]
        [|getParserSource "../../S.yrd"conv;
          getParserSource "../../D.yrd"conv;
          getParserSource "../../K.yrd"conv;
          getParserSource "../../M.yrd"conv|]
    let timeInit = System.DateTime.UtcNow;

    let inputEdges = 
        Array.init 50 id
        |> Array.collect(fun i ->
            Array.init 50 (fun j -> [|new ParserEdge<string>(i, j, "eps"); new ParserEdge<string>(i, j, "M"); new ParserEdge<string>(i, j, "K"); new ParserEdge<string>(i, j, "S")|])
            |> Array.collect id)
        |> (fun x -> Array.append x ([|new ParserEdge<string>(0, 1, "S"); new ParserEdge<string>(0, 1, "eps"); new ParserEdge<string>(0, 1, "M"); new ParserEdge<string>(0, 1, "K"); new ParserEdge<string>(50, 51, "D")|]))
    
    let inputEpsEdges = 
        Array.init 50 id
        |> Array.collect(fun i ->
            Array.init 50 (fun j -> new ParserEdge<string>(i, j, "eps")))
        |> (fun x -> Array.append x ([|new ParserEdge<string>(0, 1, "eps")|]))

    let tokenToInt = (fun x -> match x with |"M"-> 0 |"K"-> 1 |"D"-> 2 |"S"-> 3 |"eps"-> -1)
    let intToString = (fun x -> match x with |0<token> -> "M" |1<token> -> "K" |2<token> -> "D"| 3<token> ->"S" | -1<token> -> "eps")
    
    let shuffledInput = new SimpleInputGraph<string>([|0|], [|51|], tokenToInt)
    shuffledInput.AddVerticesAndEdgeRange(inputEdges) |> ignore

    let inputGraphformula = 
        inputGraphToFormula shuffledInput

    shuffledInput.AddEdgeRange(inputEpsEdges) |> ignore

    let sppfsFormula = 
        parserSources
        |> Array.mapi (fun i ps ->
            let parser = new Parser(ps)
            let roots = parser.GetRoots shuffledInput
            sppfRootsToFormula roots intToString i
            )
        |> (fun x -> AND(x))
    
    let edgesMappingFormula = 
        let len = parserSources.Length
        edgesMapping inputEdges len

    let finalFormula =        
        [|inputGraphformula; sppfsFormula; edgesMappingFormula|] |> AND |> reduceFormula
    
    Z3logic.solveFormula finalFormula

    printfn "Time: %A" (System.DateTime.UtcNow - timeInit)
    



(*
let testSingleLetterGrammar () = 
    let input = [|1..100|]|> Array.map (fun i -> sprintf "A%i" i)

    let parserSources : ParserSourceGLL [] = 
        let conv = [new ExpandMeta()]
        [|for fileNumber in 1..100 -> getParserSource (sprintf "../../grammars/grammars100/grammar%i.yrd" fileNumber ) conv |]

    let inputs =
        [| for ps in parserSources ->
                let shuffleInput = new ShuffleInputGraph<string>(ps.StringToToken)
                shuffleInput.AddTokens(input)
                shuffleInput |]

    let tries = 
        parserSources
        |> Array.mapi (fun i ps ->
            let parser = new Parser(parserSources.[i])
            parser.Parse inputs.[i] true
            parser.GetPrefixTree(), i
            )
    let sppfFormulas = 
        tries
        |> Array.filter (fun (x,i) -> x.IsSome)
        |> Array.map (fun (x,i) -> 
            let beg,trie = x.Value
            let formula = trieToFormula trie beg input.Length (sprintf "A%iB"i)
            formula)

    printfn "Number of tries: %i" sppfFormulas.Length

    let genXors (vars : string [][]) =
        vars
        |> Array.map(fun x -> String.Join(" ", x))
        |> (fun x -> String.Join("\n", x))
    
    let vars = 
        [|0..99|]
        |> Array.map(fun i ->
            [|0..99|]
            |> Array.map(fun j -> sprintf "A%iB%i" j i)
            )


    
    let finalFormula =        
        //Array.append sppfFormulas xors
        sppfFormulas
        |> AND

    System.IO.File.WriteAllText("myFormula.txt", finalFormula.ToString())
    System.IO.File.WriteAllText("myFormulaXORS.txt", genXors vars)
*)



[<EntryPoint>]
let main argv = 
    //testSingleLetterGrammar()
    //testBaseLineDomainGrammars()

    //testSimpleProgram()

    //Z3logic.simpleTest()
    testGraphParsing()
    0
