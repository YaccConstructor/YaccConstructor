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



type FSA( calls : int, locks : int, asserts : int) =
    let alphabet = new HashSet<EdgeSymbol>()
    let finalStates = new HashSet<_>([3<positionInGrammar>;
                                      4<positionInGrammar>;
                                      5<positionInGrammar>;
                                      9<positionInGrammar>;
                                      12<positionInGrammar>])
    let stateToNontermName = new Dictionary<int<positionInGrammar>, string>()
    
    let lastStates = new HashSet<int<positionInGrammar>>()
    let startStates = [| new HashSet<_>([1<positionInGrammar>])|]
    
    let sState = 2<positionInGrammar>
    let s0State = 3<positionInGrammar>
    let s1State = 4<positionInGrammar>
    let baState = 0<positionInGrammar>
    let caState = 1<positionInGrammar>
    
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
            sEdges.Add(Nonterm(baState), 5<positionInGrammar>)
            sEdges.Add(Nonterm(s1State), 6<positionInGrammar>)
            sEdges.Add(Nonterm(sState), 7<positionInGrammar>)
            
            states.Add(sEdges)
            
            ///s0 edges
            let s0Edges = new ResizeArray<_>()
            s0Edges.Add(Nonterm(caState), 9<positionInGrammar>)
            states.Add(s0Edges)
            // s1
            for i in [4..12] do
                states.Add(new ResizeArray<_>())
            assert (states.Count = 13)
            
            states.[5].Add(Nonterm(sState), 12<positionInGrammar>)
            states.[6].Add(Nonterm(sState), 12<positionInGrammar>)
            states.[7].Add(Nonterm(s1State), 12<positionInGrammar>)
            states.[7].Add(Nonterm(baState), 12<positionInGrammar>)
            
            states.[8].Add(Nonterm(s1State), 12<positionInGrammar>)
            states.[8].Add(Nonterm(sState), 12<positionInGrammar>)
            
            states.[9].Add(Nonterm(s0State), 12<positionInGrammar>)
            
            states.[10].Add(Nonterm(s0State), 12<positionInGrammar>)
            
            states.[11].Add(Nonterm(s1State), 12<positionInGrammar>)
            
            //s edges calls
            let currVertexNum = states.Count-1
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                states.Add(new ResizeArray<_>())
                states.Add(new ResizeArray<_>())
            
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                let numbStr = (i - currVertexNum - 1).ToString()
                states.[2].Add(Term("C" + numbStr), i * 1<positionInGrammar>)
                states.[i].Add(Nonterm(sState), (i + calls) * 1<positionInGrammar>)
                states.[i].Add(Nonterm(s1State), (i + calls) * 1<positionInGrammar>)
                states.[i + calls].Add(Term("RT" + numbStr), 8<positionInGrammar>)
            
            //s0 edges calls
            let currVertexNum = states.Count-1
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                states.Add(new ResizeArray<_>())
                states.Add(new ResizeArray<_>())
            
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                let numbStr = (i - currVertexNum - 1).ToString()
                states.[3].Add(Term("C" + numbStr), i * 1<positionInGrammar>)
                states.[i].Add(Nonterm(s0State), (i + calls) * 1<positionInGrammar>)
                states.[i + calls].Add(Term("RT" + numbStr), 10<positionInGrammar>)
            
            //s0 edges locks
            let currVertexNum = states.Count-1
            for i in [currVertexNum + 1 .. locks + currVertexNum] do
                states.Add(new ResizeArray<_>())
                states.Add(new ResizeArray<_>())
            
            for i in [currVertexNum + 1 .. locks + currVertexNum] do
                let numbStr = (i - currVertexNum - 1).ToString()
                states.[3].Add(Term("G" + numbStr), i * 1<positionInGrammar>)
                states.[i].Add(Nonterm(s0State), (i + locks) * 1<positionInGrammar>)
                states.[i + locks].Add(Term("RL" + numbStr), 10<positionInGrammar>)
            
            //s1 edges calls
            let currVertexNum = states.Count-1
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                states.Add(new ResizeArray<_>())
                states.Add(new ResizeArray<_>())
            
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                let numbStr = (i - currVertexNum - 1).ToString()
                states.[4].Add(Term("C" + numbStr), i * 1<positionInGrammar>)
                states.[i].Add(Nonterm(s1State), (i + calls) * 1<positionInGrammar>)
                states.[i + calls].Add(Term("RT" + numbStr), 11<positionInGrammar>)
            
            //s1 edges locks
            let currVertexNum = states.Count-1
            for i in [currVertexNum + 1 .. locks + currVertexNum] do
                states.Add(new ResizeArray<_>())
                states.Add(new ResizeArray<_>())
            
            for i in [currVertexNum + 1 .. locks + currVertexNum] do
                let numbStr = (i - currVertexNum - 1).ToString()
                states.[4].Add(Term("G" + numbStr), i * 1<positionInGrammar>)
                states.[i].Add(Nonterm(s0State), (i + locks) * 1<positionInGrammar>)
                states.[i + locks].Add(Term("RL" + numbStr), 11<positionInGrammar>)
            
            states
        
        let states = getStates calls locks asserts
        
        stateToNontermName.Add(baState, "ba")
        stateToNontermName.Add(caState, "ca")
        stateToNontermName.Add(sState, "s")
        stateToNontermName.Add(s0State, "s0")
        stateToNontermName.Add(s1State, "s1")
        
        {   States = 
                states
                |> ResizeArray.map (fun x -> x.ToArray())
                |> fun x -> x.ToArray();
            Alphabet = set alphabet;
            StateToNontermName = stateToNontermName;
            StartComponentNumber = 2;
            StartStates    = [|new HashSet<_>([|baState|]);
                               new HashSet<_>([|caState|]);
                               new HashSet<_>([|sState|]);
                               new HashSet<_>([|s0State|]);
                               new HashSet<_>([|s1State|])|];
            FinalStates    = finalStates;
            LastStates     = new HashSet<_>(lastStates)}
            
    let x = 
        ()
        //printDot "ruchnoiFSA.dot" fsa

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
(*
type FSA( calls : int, locks : int, asserts : int) =
    let alphabet = new HashSet<EdgeSymbol>()
    let finalStates = new HashSet<_>([2<positionInGrammar>])
    let stateToNontermName = new Dictionary<int<positionInGrammar>, string>()
    
    let lastStates = new HashSet<int<positionInGrammar>>()
    let startStates = [| new HashSet<_>([1<positionInGrammar>])|]
    
    let sState = 1<positionInGrammar>
    let aState = 0<positionInGrammar>
    let finSt = 2<positionInGrammar>
    
    let fsa =
        let getStates calls locks asserts =        
            let states = new ResizeArray<ResizeArray<EdgeSymbol*int<positionInGrammar>>>()
            let aSt = 
                ResizeArray.init asserts id
                |> ResizeArray.map (fun i -> Term("A" + i.ToString()),finSt)
            states.Add(aSt)
            
            ///s edges
            let sEdges = new ResizeArray<_>()
            sEdges.Add(Nonterm(sState), 3<positionInGrammar>)
            sEdges.Add(Nonterm(aState), finSt)
            states.Add(sEdges)
            
            /// final state
            states.Add(new ResizeArray<_>())
            
            let state3Edges = new ResizeArray<_>()
            state3Edges.Add(Nonterm(sState), finSt)
            states.Add(state3Edges)
            
            //s edges calls
            let currVertexNum = states.Count-1
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                states.Add(new ResizeArray<_>())
                states.Add(new ResizeArray<_>())
            
            for i in [currVertexNum + 1 .. calls + currVertexNum] do
                let numbStr = (i - currVertexNum - 1).ToString()
                states.[1].Add(Term("C" + numbStr), i * 1<positionInGrammar>)
                states.[i].Add(Nonterm(sState), (i + calls) * 1<positionInGrammar>)
                states.[i + calls].Add(Term("RT" + numbStr), finSt)
                states.[i].Add(Term("RT" + numbStr), finSt)
            
            //s edges locks
            let currVertexNum = states.Count-1
            for i in [currVertexNum + 1 .. locks + currVertexNum] do
                states.Add(new ResizeArray<_>())
                states.Add(new ResizeArray<_>())
            
            for i in [currVertexNum + 1 .. locks + currVertexNum] do
                let numbStr = (i - currVertexNum - 1).ToString()
                states.[1].Add(Term("G" + numbStr), i * 1<positionInGrammar>)
                states.[i].Add(Nonterm(sState), (i + locks) * 1<positionInGrammar>)
                states.[i + locks].Add(Term("RL" + numbStr), finSt)
                states.[i].Add(Term("RL" + numbStr), finSt)
            
            states
        
        let states = getStates calls locks asserts
        
        stateToNontermName.Add(aState, "a")
        stateToNontermName.Add(sState, "s")
        
        {   States = 
                states
                |> ResizeArray.map (fun x -> x.ToArray())
                |> fun x -> x.ToArray();
            Alphabet = set alphabet;
            StateToNontermName = stateToNontermName;
            StartComponentNumber = 1;
            StartStates    = [|new HashSet<_>([|aState|]);
                               new HashSet<_>([|sState|]);|];
            FinalStates    = finalStates;
            LastStates     = new HashSet<_>(lastStates)}
            
    let x = 
        ()
        //printDot "ruchnoiFSA.dot" fsa

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
    *)