module AutomataCombinatorsTests

open System
open System.Collections.Generic
open NUnit.Framework
open Yard.Generators.Common.AutomataCombinators
open Yard.Generators.Common.FSA.Common
open Yard.Generators.Common.FSA
open AbstractAnalysis.Common

[<TestFixture>]
type ``Automaton correctness tests``() = 
    let findStartState (fsa: FSA) name =
        fsa.StateToNontermName 
            |> Seq.find (fun pair -> pair.Value = name) 
            |> fun pair -> pair.Key
    
    let countIncoming transitions node = 
        transitions |> Map.filter (fun key value -> value = node) |> Map.count

    let join left right = 
        left |> Map.map (fun key value -> (value, Map.find key right))
    
    let isStart (fsa: FSA) state = 
        fsa.StartStates |> Array.exists (fun starts -> starts.Contains state)
    
    let isFinal (fsa: FSA) state =
        fsa.FinalStates.Contains state

    let rec assertIsomorphic (expected: FSA) (actual: FSA) expectedStartState actualStartState =
        Assert.AreEqual (isStart expected expectedStartState, isStart actual actualStartState)
        Assert.AreEqual (isFinal expected expectedStartState, isFinal actual actualStartState)

        let expectedTransitions = Map expected.States.[int expectedStartState]
        let actualTransitions = Map actual.States.[int actualStartState]

        let valuesSet map = Map.toSeq map |> Seq.map snd |> set
        let keysSet map = Map.toSeq map |> Seq.map fst |> set

        let expectedLabels = expectedTransitions |> keysSet 
        let actualLabels = actualTransitions |> keysSet
        Assert.AreEqual (expectedLabels, actualLabels)
        Assert.AreEqual (expectedLabels.Count, expectedTransitions.Count)
        Assert.AreEqual (actualLabels.Count, actualTransitions.Count)
        
        actualLabels |> Set.iter 
            (
                fun label ->
                    Assert.True (actual.Alphabet.Contains label)
                    match label with
                    | Nonterm i -> 
                        Assert.True (isStart actual i)
                    | _ -> ()
            )

        let front = join expectedTransitions actualTransitions |> valuesSet
        
        front |> Set.iter 
            (
                fun pair -> 
                    Assert.AreEqual (countIncoming expectedTransitions (fst pair),
                                     countIncoming actualTransitions (snd pair))
                    assertIsomorphic expected actual (fst pair) (snd pair)
            )
    
    let checkConstraints (fsa: FSA) =
        Assert.AreEqual (fsa.StateToNontermName.Count, (fsa.StateToNontermName.Values |> set).Count)
        Assert.AreEqual (fsa.NontermCount, fsa.StateToNontermName.Count)
        Assert.AreEqual (fsa.NontermCount, fsa.StartStates.Length)

        fsa.StateToNontermName |> Seq.iter 
            (
                fun pair -> 
                    Assert.True (isStart fsa pair.Key)
            )

        Assert.IsTrue (isStart fsa fsa.StartState)

        fsa.StartStates |> Array.iter (fun starts -> Assert.AreEqual (1, starts.Count))

    let assertIsomorphic (expected: FSA) (actual: FSA) =
        checkConstraints expected
        checkConstraints actual

        Assert.AreEqual (expected.Alphabet, actual.Alphabet)

        let expectedNonterminalNames = actual.StateToNontermName.Values |> set
        let actualNonterminalNames = actual.StateToNontermName.Values |> set
        
        Assert.AreEqual (expectedNonterminalNames, actualNonterminalNames)
        
        actualNonterminalNames |> Set.iter 
            (
                fun name ->
                    let expectedStart = findStartState expected name
                    let actualStart = findStartState actual name
                    assertIsomorphic expected actual expectedStart actualStart
            )
       
    let nid id = id * 1<positionInGrammar>

    let fakeNonterm actualFsa name = 
        Nonterm (findStartState actualFsa name)

    [<Test>]
    member this.``Terminal token``() = 
        let factory = new AutomataFactory()
        factory.TerminalToken "a" |> factory.Terminal |> factory.Start "S"

        let actualFsa = factory.Produce()
        let expectedFsa = 
            new FSA ({
                         States = [| [|(Term "a", nid 1)|]; [||] |]
                         Alphabet = Set [Term "a"]
                         StateToNontermName = new Dictionary<_, _> (dict [(nid 0, "S")])
                         StartComponentNumber = 0
                         StartStatesOfEachNonterminal = [|new HashSet<_> ([nid 0])|]
                         FinalStates = new HashSet<_> ([nid 1])
                     })
        assertIsomorphic expectedFsa actualFsa
   
    [<Test>]
    member this.``Nonterminal token``() = 
        let factory = new AutomataFactory()
        factory.NonterminalToken "S" |> factory.Reference |> factory.Start "S"

        let actualFsa = factory.Produce()
        let expectedFsa = 
            new FSA ({
                         States = [| [|(fakeNonterm actualFsa "S", nid 1)|]; [||] |]
                         Alphabet = Set [fakeNonterm actualFsa "S"]
                         StateToNontermName = new Dictionary<_, _> (dict [(nid 0, "S")])
                         StartComponentNumber = 0
                         StartStatesOfEachNonterminal = [|new HashSet<_> ([nid 0])|]
                         FinalStates = new HashSet<_> ([nid 1])
                     })
        assertIsomorphic expectedFsa actualFsa
    
    [<Test>]
    member this.``Sequence of tokens``() = 
        let factory = new AutomataFactory()
        let a = factory.TerminalToken "a" |> factory.Terminal
        let b = factory.TerminalToken "b" |> factory.Terminal
        factory.Sequence a b |> factory.Start "S"

        let actualFsa = factory.Produce()
        let expectedFsa = 
            new FSA ({
                         States = [| [|(Term "a", nid 1)|]; [|(Term "b", nid 2)|]; [||] |]
                         Alphabet = Set [Term "a"; Term "b"]
                         StateToNontermName = new Dictionary<_, _> (dict [(nid 0, "S")])
                         StartComponentNumber = 0
                         StartStatesOfEachNonterminal = [|new HashSet<_> ([nid 0])|]
                         FinalStates = new HashSet<_> ([nid 2])
                     })
        assertIsomorphic expectedFsa actualFsa

    [<Test>]
    member this.``Alternation of tokens``() = 
        let factory = new AutomataFactory()
        let a = factory.TerminalToken "a" |> factory.Terminal
        let b = factory.TerminalToken "b" |> factory.Terminal
        factory.Alternation a b |> factory.Start "S"

        let actualFsa = factory.Produce()
        let expectedFsa = 
            new FSA ({
                         States = [| [|(Term "a", nid 1); (Term "b", nid 1)|]; [||] |]
                         Alphabet = Set [Term "a"; Term "b"]
                         StateToNontermName = new Dictionary<_, _> (dict [(nid 0, "S")])
                         StartComponentNumber = 0
                         StartStatesOfEachNonterminal = [|new HashSet<_> ([nid 0])|]
                         FinalStates = new HashSet<_> ([nid 1])
                     })
        assertIsomorphic expectedFsa actualFsa

    [<Test>]
    member this.``Sequence of sequences``() = 
        let factory = new AutomataFactory()
        let a = factory.TerminalToken "a" |> factory.Terminal
        let b = factory.TerminalToken "b" |> factory.Terminal
        let c = factory.TerminalToken "c" |> factory.Terminal
        let d = factory.TerminalToken "d" |> factory.Terminal
        let ab = factory.Sequence a b
        let cd = factory.Sequence c d
        factory.Sequence ab cd |> factory.Start "S"

        let actualFsa = factory.Produce()
        let expectedFsa = 
            new FSA ({
                         States = [| [|(Term "a", nid 1)|]; 
                                     [|(Term "b", nid 2)|]; 
                                     [|(Term "c", nid 3)|]; 
                                     [|(Term "d", nid 4)|]; [||] |]
                         Alphabet = Set [Term "a"; Term "b"; Term "c"; Term "d"]
                         StateToNontermName = new Dictionary<_, _> (dict [(nid 0, "S")])
                         StartComponentNumber = 0
                         StartStatesOfEachNonterminal = [|new HashSet<_> ([nid 0])|]
                         FinalStates = new HashSet<_> ([nid 4])
                     })
        assertIsomorphic expectedFsa actualFsa

    [<Test>]
    member this.``Sequence of alternations``() = 
        let factory = new AutomataFactory()
        let a = factory.TerminalToken "a" |> factory.Terminal
        let b = factory.TerminalToken "b" |> factory.Terminal
        let c = factory.TerminalToken "c" |> factory.Terminal
        let d = factory.TerminalToken "d" |> factory.Terminal
        let ab = factory.Alternation a b
        let cd = factory.Alternation c d
        factory.Sequence ab cd |> factory.Start "S"

        let actualFsa = factory.Produce()
        let expectedFsa = 
            new FSA ({
                         States = [| [|(Term "a", nid 1); (Term "b", nid 1)|]; 
                                     [|(Term "c", nid 2); (Term "d", nid 2)|];
                                     [||] |]
                         Alphabet = Set [Term "a"; Term "b"; Term "c"; Term "d"]
                         StateToNontermName = new Dictionary<_, _> (dict [(nid 0, "S")])
                         StartComponentNumber = 0
                         StartStatesOfEachNonterminal = [|new HashSet<_> ([nid 0])|]
                         FinalStates = new HashSet<_> ([nid 2])
                     })
        assertIsomorphic expectedFsa actualFsa

    [<Test>]
    member this.``Alternation of sequences``() = 
        let factory = new AutomataFactory()
        let a = factory.TerminalToken "a" |> factory.Terminal
        let b = factory.TerminalToken "b" |> factory.Terminal
        let c = factory.TerminalToken "c" |> factory.Terminal
        let d = factory.TerminalToken "d" |> factory.Terminal
        let ab = factory.Sequence a b
        let cd = factory.Sequence c d
        factory.Alternation ab cd |> factory.Start "S"

        let actualFsa = factory.Produce()
        let expectedFsa = 
            new FSA ({
                         States = [| [|(Term "a", nid 1); (Term "c", nid 2)|]; 
                                     [|(Term "b", nid 3)|];
                                     [|(Term "d", nid 3)|];
                                     [||] |]
                         Alphabet = Set [Term "a"; Term "b"; Term "c"; Term "d"]
                         StateToNontermName = new Dictionary<_, _> (dict [(nid 0, "S")])
                         StartComponentNumber = 0
                         StartStatesOfEachNonterminal = [|new HashSet<_> ([nid 0])|]
                         FinalStates = new HashSet<_> ([nid 3])
                     })
        assertIsomorphic expectedFsa actualFsa

    [<Test>]
    member this.``Alternation of alternations``() = 
        let factory = new AutomataFactory()
        let a = factory.TerminalToken "a" |> factory.Terminal
        let b = factory.TerminalToken "b" |> factory.Terminal
        let c = factory.TerminalToken "c" |> factory.Terminal
        let d = factory.TerminalToken "d" |> factory.Terminal
        let ab = factory.Alternation a b
        let cd = factory.Alternation c d
        factory.Alternation ab cd |> factory.Start "S"

        let actualFsa = factory.Produce()
        let expectedFsa = 
            new FSA ({
                         States = [| [|(Term "a", nid 1); 
                                       (Term "b", nid 1); 
                                       (Term "c", nid 1); 
                                       (Term "d", nid 1)|]; 
                                     [||] |]
                         Alphabet = Set [Term "a"; Term "b"; Term "c"; Term "d"]
                         StateToNontermName = new Dictionary<_, _> (dict [(nid 0, "S")])
                         StartComponentNumber = 0
                         StartStatesOfEachNonterminal = [|new HashSet<_> ([nid 0])|]
                         FinalStates = new HashSet<_> ([nid 1])
                     })
        assertIsomorphic expectedFsa actualFsa
    
    [<Test>]
    member this.``Multiple rules``() = 
        let factory = new AutomataFactory()
        factory.TerminalToken "a" |> factory.Terminal |> factory.Rule "A"
        factory.NonterminalToken "A" |> factory.Reference |> factory.Start "S"

        let actualFsa = factory.Produce()
        let expectedFsa = 
            new FSA ({
                         States = [| [|(fakeNonterm actualFsa "A", nid 2)|]
                                     [|(Term "a", nid 2)|]
                                     [||] |]
                         Alphabet = Set [Term "a"; fakeNonterm actualFsa "A"]
                         StateToNontermName = new Dictionary<_, _> (dict [(nid 0, "S"); (nid 1, "A")])
                         StartComponentNumber = 0
                         StartStatesOfEachNonterminal = [|new HashSet<_> ([nid 0]); new HashSet<_> ([nid 1])|]
                         FinalStates = new HashSet<_> ([nid 2])
                     })
        assertIsomorphic expectedFsa actualFsa
                
    [<Test>]
    member this.``Epsilons``() = 
        let factory = new AutomataFactory()
        let a = factory.TerminalToken "a" |> factory.Terminal
        let b = factory.TerminalToken "b" |> factory.Terminal
        let beps = factory.Alternation b factory.Epsilon
        let abeps = factory.Sequence a beps
        factory.Alternation abeps factory.Epsilon |> factory.Start "S"

        let actualFsa = factory.Produce()
        let expectedFsa = 
            new FSA ({
                         States = [| [|(Term "a", nid 1)|]
                                     [|(Term "b", nid 2)|]
                                     [||] |]
                         Alphabet = Set [Term "a"; Term "b"]
                         StateToNontermName = new Dictionary<_, _> (dict [(nid 0, "S")])
                         StartComponentNumber = 0
                         StartStatesOfEachNonterminal = [|new HashSet<_> ([nid 0])|]
                         FinalStates = new HashSet<_> ([nid 0; nid 1; nid 2])
                     })
        assertIsomorphic expectedFsa actualFsa
