namespace Yard.Generators.Common.FSA

open System.Collections.Generic

open Yard.Generators.Common.FSA.Common
open Microsoft.FSharp.Collections
open AbstractAnalysis.Common
open Yard.Core.IL
open Yard.Generators.Common.Epsilon
open Yard.Generators.Common.SymbolSets
open Yard.Generators.Common
(*
type FSA(ruleList : Rule<Source,Source> list) =
    let time = ref System.DateTime.Now
    let fsa =
        ruleList
        |> convertRulesToFSA
            |> printDot @".\FSA1initialFSA.dot"
            |> (fun x -> //printfn "Convertion to fsa time: %A" (System.DateTime.Now - !time)
                        //System.IO.File.WriteAllLines(@".\time.txt", [sprintf "Convertion to fsa time: %A" (System.DateTime.Now - !time)])
                        time := System.DateTime.Now
                        x)
        |> removeEpsilonEdges
            |> printDot @".\FSA2withoutEpsilon.dot"
            |> (fun x -> //printfn "Epsilon edges removal time: %A" (System.DateTime.Now - !time)
                        //System.IO.File.AppendAllLines(@".\time.txt", [sprintf "Epsilon edges removal time: %A" (System.DateTime.Now - !time)])
                        time := System.DateTime.Now
                        x)
        |> toDFA              
            |> printDot @".\FSA3determ.dot"
            |> (fun x -> //printfn "To dfa convetrion time: %A" (System.DateTime.Now - !time)
                        //System.IO.File.AppendAllLines(@".\time.txt", [sprintf "To dfa convetrion time: %A" (System.DateTime.Now - !time)])
                        time := System.DateTime.Now
                        x)
        |> minimizeFSA
            |> printDot @".\FSA4minimized.dot"
            |> (fun x -> //printfn "Minimization time: %A" (System.DateTime.Now - !time)
                        //System.IO.File.AppendAllLines(@".\time.txt", [sprintf "Minimization time: %A" (System.DateTime.Now - !time)])
                        time := System.DateTime.Now
                        x)

    member this.States = fsa.States
    //after convertion to dfa each startstates HashSet contains only one state
    member this.StartState = fsa.StartStates.[fsa.StartComponentNumber] |> Seq.find (fun x -> true)
    member this.FinalStates = fsa.FinalStates
    member this.NontermCount = fsa.StartStates.Length
    member this.StateToNontermName = fsa.StateToNontermName
    member this.LastStates = fsa.LastStates
    member this.StartStates = fsa.StartStates
    member this.PrintDot filePrintPath = printDot filePrintPath fsa
    member this.RuleList = ruleList
    member this.Alphabet = fsa.Alphabet
*)



type FSA((*ruleList : Rule<Source,Source> list,*) calls : int, locks : int, asserts : int) =
    (*let ruleList = 
        let name = namedSource "s0"
        let body = name |> PToken
        let q = defaultRule name body
       
        [q(*ba; ca; s1Rule; sRule; s0Rule*)]
        *)
    let alphabet = new HashSet<EdgeSymbol>()
    let finalStates = new HashSet<_>([3<positionInGrammar>;4<positionInGrammar>;5<positionInGrammar>;9<positionInGrammar>;12<positionInGrammar>])
    let stateToNontermName = new Dictionary<int<positionInGrammar>, string>()
    
    let lastStates = new HashSet<int<positionInGrammar>>()
    let startStates = [| new HashSet<_>([1<positionInGrammar>])|]
    
    let fsa =
        let getStates calls locks asserts =
            let states = new ResizeArray<ResizeArray<EdgeSymbol*int<positionInGrammar>>>()
            
            let ba = 
                ResizeArray.init asserts id
                |> ResizeArray.map (fun i -> Term("A" + i.ToString()),12<positionInGrammar>)
            states.Add(ba)
            
            let ca = 
                ResizeArray.init asserts id
                |> ResizeArray.map (fun i -> Term("A" + i.ToString()),12<positionInGrammar>)
            states.Add(ca)
            
            ///s edges
            let sEdges = new ResizeArray<_>()
            sEdges.Add(Nonterm(1<positionInGrammar>), 5<positionInGrammar>)
            sEdges.Add(Nonterm(4<positionInGrammar>), 6<positionInGrammar>)
            sEdges.Add(Nonterm(2<positionInGrammar>), 7<positionInGrammar>)
            
            states.Add(sEdges)
            
            ///s0 edges
            let s0Edges = new ResizeArray<_>()
            s0Edges.Add(Nonterm(0<positionInGrammar>), 9<positionInGrammar>)
            states.Add(s0Edges)
            // s1
            for i in [4..12] do
                states.Add(new ResizeArray<_>())
            
            states.[5].Add(Nonterm(2<positionInGrammar>), 12<positionInGrammar>)
            states.[6].Add(Nonterm(2<positionInGrammar>), 12<positionInGrammar>)
            states.[7].Add(Nonterm(4<positionInGrammar>), 12<positionInGrammar>)
            states.[7].Add(Nonterm(1<positionInGrammar>), 12<positionInGrammar>)
            
            states.[8].Add(Nonterm(4<positionInGrammar>), 12<positionInGrammar>)
            states.[8].Add(Nonterm(2<positionInGrammar>), 12<positionInGrammar>)
            
            states.[9].Add(Nonterm(3<positionInGrammar>), 12<positionInGrammar>)
            
            states.[10].Add(Nonterm(3<positionInGrammar>), 12<positionInGrammar>)
            
            states.[11].Add(Nonterm(4<positionInGrammar>), 12<positionInGrammar>)
            
            //s edges calls
            let currVertexNum = states.Count-1
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                states.Add(new ResizeArray<_>())
                states.Add(new ResizeArray<_>())
            
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                let numbStr = (i - currVertexNum - 1).ToString()
                states.[2].Add(Term("C" + numbStr), i * 1<positionInGrammar>)
                states.[i].Add(Nonterm(2<positionInGrammar>), (i + calls) * 1<positionInGrammar>)
                states.[i + calls].Add(Term("RT" + numbStr), 8<positionInGrammar>)
            
            //s0 edges calls
            let currVertexNum = states.Count-1
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                states.Add(new ResizeArray<_>())
                states.Add(new ResizeArray<_>())
            
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                let numbStr = (i - currVertexNum - 1).ToString()
                states.[3].Add(Term("C" + numbStr), i * 1<positionInGrammar>)
                states.[i].Add(Nonterm(3<positionInGrammar>), (i + calls) * 1<positionInGrammar>)
                states.[i + calls].Add(Term("RT" + numbStr), 10<positionInGrammar>)
            
            //s0 edges gets
            let currVertexNum = states.Count-1
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                states.Add(new ResizeArray<_>())
                states.Add(new ResizeArray<_>())
            
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                let numbStr = (i - currVertexNum - 1).ToString()
                states.[3].Add(Term("G" + numbStr), i * 1<positionInGrammar>)
                states.[i].Add(Nonterm(3<positionInGrammar>), (i + calls) * 1<positionInGrammar>)
                states.[i + calls].Add(Term("RL" + numbStr), 10<positionInGrammar>)
            
            //s1 edges calls
            let currVertexNum = states.Count-1
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                states.Add(new ResizeArray<_>())
                states.Add(new ResizeArray<_>())
            
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                let numbStr = (i - currVertexNum - 1).ToString()
                states.[4].Add(Term("C" + numbStr), i * 1<positionInGrammar>)
                states.[i].Add(Nonterm(4<positionInGrammar>), (i + calls) * 1<positionInGrammar>)
                states.[i + calls].Add(Term("RT" + numbStr), 11<positionInGrammar>)
            
            //s1 edges gets
            let currVertexNum = states.Count-1
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                states.Add(new ResizeArray<_>())
                states.Add(new ResizeArray<_>())
            
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                let numbStr = (i - currVertexNum - 1).ToString()
                states.[4].Add(Term("G" + numbStr), i * 1<positionInGrammar>)
                states.[i].Add(Nonterm(3<positionInGrammar>), (i + calls) * 1<positionInGrammar>)
                states.[i + calls].Add(Term("RL" + numbStr), 11<positionInGrammar>)
            
            states
        
        let states = getStates calls locks asserts
        
        stateToNontermName.Add(0<positionInGrammar>, "ca")
        stateToNontermName.Add(1<positionInGrammar>, "ba")
        stateToNontermName.Add(2<positionInGrammar>, "s")
        stateToNontermName.Add(3<positionInGrammar>, "s0")
        stateToNontermName.Add(4<positionInGrammar>, "s1")
        
        {   States = 
                states
                |> ResizeArray.map (fun x -> x.ToArray())
                |> fun x -> x.ToArray();
            Alphabet = set alphabet;
            StateToNontermName = stateToNontermName;
            StartComponentNumber = 2;
            StartStates    = [|new HashSet<_>([|0<positionInGrammar>|]);
                               new HashSet<_>([|1<positionInGrammar>|]);
                               new HashSet<_>([|2<positionInGrammar>|]);
                               new HashSet<_>([|3<positionInGrammar>|]);
                               new HashSet<_>([|4<positionInGrammar>|])|];
            FinalStates    = finalStates;
            LastStates     = new HashSet<_>(lastStates)}
            
    //let x = 
    //    printDot "ruchnoiFSA.dot" fsa

    member this.States = fsa.States
    //after convertion to dfa each startstates HashSet contains only one state
    member this.StartState = fsa.StartStates.[fsa.StartComponentNumber] |> Seq.find (fun x -> true)
    member this.FinalStates = fsa.FinalStates
    member this.NontermCount = fsa.StartStates.Length
    member this.StateToNontermName = fsa.StateToNontermName
    //member this.LastStates = fsa.LastStates
    member this.StartStates = fsa.StartStates
    member this.PrintDot filePrintPath = printDot filePrintPath fsa
    //member this.RuleList = ruleList
    member this.Alphabet = fsa.Alphabet
