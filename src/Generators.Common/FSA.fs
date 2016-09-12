namespace Yard.Generators.Common.FSA

open System.Collections.Generic

open Yard.Generators.Common.FSA.Common

open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Generators.Common.Epsilon
open Yard.Generators.Common.SymbolSets
open Yard.Generators.Common

type FSA(ruleList : Rule.t<Source.t,Source.t> list) =
    let fsa =
        ruleList
        |> convertRulesToFSA
        |> printDot @"C:\zgrviewer-0.10.0\Graphs\initialNFA.dot"
        |> removeEpsilonEdges
        |> printDot @"C:\zgrviewer-0.10.0\Graphs\withoutEpsilon.dot"
        |> toDFA
        |> printDot @"C:\zgrviewer-0.10.0\Graphs\DFA.dot"
        |> minimizeFSA
        |> printDot @"C:\zgrviewer-0.10.0\Graphs\minimizedDFA.dot"
    let firstSet = genFirstSet fsa
    do
        ()
    member this.States = fsa.States
    member this.StartState = fsa.StartState
    member this.FinalStates = fsa.FinalStates
    member this.NontermCount = fsa.StartStates.Length
    member this.FirstSet = firstSet
    member this.StateToNontermName = fsa.StateToNontermName
    member this.PrintDot filePrintPath = printDot filePrintPath fsa