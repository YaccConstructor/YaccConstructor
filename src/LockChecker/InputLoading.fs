module InputLoading
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common
open Yard.Frontends.YardFrontend
open YC.API
open AbstractParser
open System
open System.Collections.Generic
open Yard.Core
open Yard.Core.IL
//open Yard.Generators.Common.FSA.Common

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
(*
let genRules calls locks asserts =
    let getAltFromArray (alts : Production<_,_> []) = 
        let st = alts.[0]
        alts.[1..]
        |> Array.fold (fun a t -> PAlt(t,a)) st
    (*  
    let assertsRule name = 
        let tokens = 
            [|0 .. asserts - 1|]
            |> Array.map (fun i -> defaultPSeq [PToken (namedSource ("A" + i.ToString())) ] )
            
        let body = getAltFromArray tokens
            
        let name = namedSource name
        defaultRule name body
    
    let ba = assertsRule "ba"
    let ca = assertsRule "ca"
    *)
    (*
    let getArrayOfAlternativeFours token1 ref1 token2 ref2 count = 
        [|0..count - 1|] 
        |> Array.map (fun i -> 
            let c = token1 + i.ToString() |> namedSource |> PToken
            let ret = token2 + i.ToString() |> namedSource |> PToken
            [c; PRef(namedSource ref1, None); ret; PRef(namedSource ref2, None)]
            |> defaultPSeq
            )
    
    // "\ns1: {} \n"
    // " C%i s1 RT%i s1" calls
    // genBrs " G%i s0 RL%i s1" locks
    let s1Rule = 
        let alts1 = getArrayOfAlternativeFours "C" "s1" "RT" "s1" calls            
        let alts2 = getArrayOfAlternativeFours "G" "s0" "RL" "s1" locks        
        let body = Array.concat [alts1; alts2; [|defaultPSeq []|] ] |> getAltFromArray
        let name = namedSource "s1"
        let r = defaultRule name body
        r
    *)
    // "[<Start>]\n"
    // "s: ba s | s ba | s s1 | s1 s | ba \n"
    // " C%i s RT%i s1" calls
    // " C%i s RT%i s" calls
    (*
    let sRule =         
        let baAlts = 
            [|defaultPSeq [PRef("ba" |> namedSource, None); PRef("s" |> namedSource, None)];
             defaultPSeq [PRef("s" |> namedSource, None); PRef("ba" |> namedSource, None)];
             defaultPSeq [PRef("s" |> namedSource, None); PRef("s1" |> namedSource, None)];
             defaultPSeq [PRef("s1" |> namedSource, None); PRef("s" |> namedSource, None)];
             defaultPSeq [PRef("ba" |> namedSource, None)]|]
        
        let alts1 = getArrayOfAlternativeFours "C" "s" "RT" "s1" calls            
        let alts2 = getArrayOfAlternativeFours "C" "s" "RT" "s" calls
        
        let body = Array.concat [alts1; alts2; baAlts ] |> getAltFromArray
        let name = namedSource "s"
        let r = defaultRule name body
        r.isStart <- true 
        r
      *)  
    // "\ns0: {} | ca s0 | ca \n"
    // " C%i s0 RT%i s0" calls
    // " G%i s0 RL%i s0" locks
    (*
    let s0Rule =         
        let caAlts = 
            [|defaultPSeq [PRef("ca" |> namedSource, None); PRef("s0" |> namedSource, None)];
             defaultPSeq [];
             defaultPSeq [PRef("ca" |> namedSource, None)]|]
        
        let alts1 = getArrayOfAlternativeFours "C" "s0" "RT" "s0" calls            
        let alts2 = getArrayOfAlternativeFours "G" "s0" "RL" "s0" locks
        
        let body = Array.concat [alts1; alts2; caAlts ] |> getAltFromArray
        let name = namedSource "s0"
        let r = defaultRule name body
        r
    *)
    
    let name = namedSource "s0"
    let body = name |> PToken
    let q = defaultRule name body
    
    [q(*ba; ca; s1Rule; sRule; s0Rule*)]
    
*)
let parseGraphFile graphFile = 
    let data = System.IO.File.ReadAllLines graphFile
    
    let data = if data.[data.Length-1].Length < 1 then data.[..data.Length-2] else data
    
    let infoLine = data.[data.Length-2]
    let startVLine = data.[data.Length-1] 
    
    let edgesLines = data.[..data.Length-3]

    let info = infoLine.Split ' '

    let calls = int <| info.[1].Trim()
    let locks = int <| info.[2].Trim()    
    let asserts = int <| info.[3].Trim()
    let startVertsLines = startVLine.Split([|' '|],StringSplitOptions.RemoveEmptyEntries)
    let startVerts = startVertsLines |> Array.map int
    let edges = 
        edgesLines |> Array.map (fun s -> s.Split ' ' |> fun a -> new ParserEdge<_>(int a.[0], int a.[2], a.[1]))
    
    edges, calls, locks, asserts, startVerts


let loadInput graphFile =
    let edges, calls, locks, asserts, startVerts = parseGraphFile graphFile
    //let grammar = genGrammar calls locks asserts
    let time = System.DateTime.UtcNow
    //let rules = genRules calls locks asserts
    printfn "Rules Generation time is %A" (System.DateTime.UtcNow - time)
    let time = System.DateTime.UtcNow
    let parserSource =
        let fe = new YardFrontend()
        let gen = new GLL()
        gen.GenerateByRules (*rules*) calls locks asserts
        //GenerateFromStrToObj grammar fe gen None Seq.empty [||] :?> ParserSourceGLL
    
    printfn "ParserSource time is %A" (System.DateTime.UtcNow - time)
    let tokenizer str = str |> parserSource.StringToToken |> int
    
    let r = new HashSet<_>()
    let ev = edges |> Array.iter (fun e ->
        r.Add e.Source |> ignore
        r.Add e.Target |> ignore)

    printfn "Start: %A" startVerts.Length
    let inputGraph = new SimpleInputGraph<_>(startVerts |> Array.filter (fun x -> r.Contains x), [||], tokenizer)
    
    //let inputGraph = new SimpleInputGraph<_>(startVerts, [||], tokenizer)
    inputGraph.AddVerticesAndEdgeRange edges |> ignore

    parserSource, inputGraph