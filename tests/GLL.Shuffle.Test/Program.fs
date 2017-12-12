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
                shuffleInput.AddTokens(input.[..1])
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
                parser.GetPrefixTree(input))
        

        mergeSPPFS l prefixTrees
            
        times.Add(l, (System.DateTime.UtcNow - startTime).TotalMilliseconds)
        System.IO.File.AppendText(sprintf "Length %i. Time: %A" l times.[times.Count-1]) |> ignore
        ()



[<EntryPoint>]
let main argv = 
    
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

//    let tree = buildAst parserSources.[0] inputs.[0]
//    tree.AstToDot("qwe.dot")
    0 // return an integer exit code
