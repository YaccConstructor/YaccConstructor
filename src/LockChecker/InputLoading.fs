module InputLoading
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common
open Yard.Frontends.YardFrontend
open YC.API
open AbstractParser
open System.Collections.Generic

(*
ba: ASSERT
ca: ASSERT

s0: C s0 RT s0 | G s0 RL s0 | ca s0 | ca | eps

s1: C s1 RT s1 | G s0 RL s1 | eps

[<Start>]
s: ba s | s ba| s1 s | s s1 | ba | C s RT s1 | C s RT s 
*)

let genGrammar calls locks asserts = 
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
    let s1Locks = genBrs " G%i s0 RL%i s1" locks
    
    let sHead = 
        "[<Start>]\n"
      + "s: ba s | s ba | s s1 | s1 s | ba \n"
    let sCalls1 = genBrs " C%i s RT%i s1" calls
    //let sCalls2 = genBrs " C%i s1 RT%i s" calls
    let sCalls3 = genBrs " C%i s RT%i s" calls
    
    let s0Head  = "\ns0: {} | ca s0 | ca \n"
    let s0Calls = genBrs " C%i s0 RT%i s0" calls
    let s0Locks = genBrs " G%i s0 RL%i s0" locks
    
    let alts a = a |> String.concat  "\n    |"
    
    grmHead
    + alts [|sHead; sCalls1;(* sCalls2;*) sCalls3|]
    + "\n" 
    + alts [|s0Head; s0Calls; s0Locks|]
    + "\n"
    + alts [|s1Head; s1Calls; s1Locks|] 
    + "\n"

let parseGraphFile graphFile = 
    let data = System.IO.File.ReadAllLines graphFile
    let infoLine = data.[data.Length-2]
    let startVLine = data.[data.Length-1] 
    let edgesLines = data.[..data.Length-3]

    let info = infoLine.Split ' '

    let calls = int <| info.[1].Trim()
    let locks = int <| info.[2].Trim()    
    let asserts = int <| info.[3].Trim()

    let grammar = genGrammar calls locks asserts

    let startVerts = startVLine.Split ' ' |> Array.map int
    let edges = 
        edgesLines |> Array.map (fun s -> s.Split ' ' |> fun a -> new ParserEdge<_>(int a.[0], int a.[2], a.[1]))
    
    grammar, edges, startVerts


let loadInput graphFile =
    let grammar, edges, startVerts = parseGraphFile graphFile

    let parserSource =
        let fe = new YardFrontend()
        let gen = new GLL()
        GenerateFromStrToObj grammar fe gen None Seq.empty [||] :?> ParserSourceGLL
    
    let tokenizer str = str |> parserSource.StringToToken |> int

    let inputGraph = new SimpleInputGraph<_>(startVerts, [||], tokenizer)
    inputGraph.AddVerticesAndEdgeRange edges |> ignore

    parserSource, inputGraph