open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common
open Yard.Frontends.YardFrontend
open YC.API
open AbstractParser
open System.Collections.Generic
open ResultProcessing

let loadGraph graphFile tokenizer =
    let data = System.IO.File.ReadAllLines(graphFile)
    let startVrts = data.[0].Split ' ' |> Array.map int
    let edges = 
        data.[1..] |> Array.map (fun s -> s.Split ' ' |> fun a -> new ParserEdge<_>(int a.[0], int a.[2], a.[1]))
    let graph = new SimpleInputGraph<_>(startVrts, [||], tokenizer)
    graph.AddVerticesAndEdgeRange edges |> ignore
    graph

(*
ba: ASSERT
ca: ASSERT

s0: C s0 RT s0 | G s0 RL s0 | ca s0 | ca | eps

s1: C s1 RT s1 | G s0 RL s1 | eps

[<Start>]
s: ba s | s ba| s1 s | s s1 | ba | C s RT s1 | C s1 RT s | C s RT s 
*)

let loadGrammar grammarFile = 
    let data = System.IO.File.ReadAllLines grammarFile
    let getLocks = int <| data.[0].Trim()    
    let calls = int <| data.[1].Trim()
    let asserts = int <| data.[2].Trim()
    let assertsGrm = [|0 .. asserts - 1|] |> Array.map (fun i -> "A" + string i) |> String.concat " | "
    let mutable grmHead = 
        "ba: " + assertsGrm + " \n"
      + "ca: " + assertsGrm + " \n"
      
    let genBrs tmplt count =
        [|0..count - 1|] 
        |> Array.map (fun i -> sprintf tmplt i i)
        |> String.concat "\n    |" 

    let s1Head  = "\ns1: {} \n"
    let s1Calls = genBrs " C%i s1 RT%i s1" calls
    let s1Locks = genBrs " G%i s0 RL%i s1" getLocks
    
    let sHead = 
        "[<Start>]\n"
      + "s: ba s | s ba | s s1 | s1 s | ba \n"
    let sCalls1 = genBrs " C%i s RT%i s1" calls
    let sCalls2 = genBrs " C%i s1 RT%i s" calls
    let sCalls3 = genBrs " C%i s RT%i s" calls
    
    let s0Head  = "\ns0: {} | ca s0 | ca \n"
    let s0Calls = genBrs " C%i s0 RT%i s0" calls
    let s0Locks = genBrs " G%i s0 RL%i s0" getLocks
    
    let alts a = a |> String.concat  "\n    |"
    
    grmHead
    + alts [|sHead; sCalls1; sCalls2; sCalls3|]
    + "\n" 
    + alts [|s0Head; s0Calls; s0Locks|]
    + "\n"
    + alts [|s1Head; s1Calls; s1Locks|] 
    + "\n"
(*
let printAllPaths parserSource inputGraph outputFile = 
    let roots = getAllSPPFRootsAsINodes parserSource inputGraph
    if roots.Length < 1
    then 
        printfn "doesn't parsed"
    else
        let result = 
            roots
            |> Array.collect(fun root -> (allPathsForRoot root parserSource.IntToString).ToArray())
            |> Array.map(fun x -> System.String.Join("; ", x))
            |> Array.distinct

        System.IO.File.WriteAllLines(outputFile, result)  
*)
let printAllBadAsserts parserSource inputGraph outputFile = 
    let roots = getAllSPPFRootsAsINodes parserSource inputGraph
    if roots.Length < 1
    then 
        printfn "doesn't parsed"
    else
        let result = 
            roots
            |> Array.collect(fun root -> getBadAsserts root parserSource.IntToString)
            |> Array.map(fun x -> System.String.Join("; ", x))
            |> Array.distinct

        System.IO.File.WriteAllLines(outputFile, result)

let printGraph (graph : SimpleInputGraph<_>) (file : string) = 
    graph.PrintToDot file id

[<EntryPoint>]
let main argv =
    //let graph = ".\\..\\..\\graph"
    //let grammarFile = ".\\..\\..\\grammar"
    let graph = argv.[0]
    /// LOCKS, CALLS, ASSERTS
    let grammarFile = argv.[1]
    
    let grammar = loadGrammar grammarFile    
    
    //System.IO.File.WriteAllText("resultGrammar.yrd", grammar)

    let parserSource =
        let fe = new YardFrontend()
        let gen = new GLL()
        GenerateFromStrToObj grammar fe gen None Seq.empty [||] :?> ParserSourceGLL
    
    let tokenizer str = str |> parserSource.StringToToken |> int

    let inputGraph = loadGraph graph tokenizer

    printGraph inputGraph "inputGraph.dot"

    let outputFile = argv.[2]
    //printAllPaths parserSource inputGraph outputFile
    printAllBadAsserts parserSource inputGraph outputFile
    0 // return an integer exit code