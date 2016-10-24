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
        |> printDot @".\initialNFA.dot"
        |> removeEpsilonEdges
        |> printDot @".\withoutEpsilon.dot"
        |> toDFA
        |> printDot @".\DFA.dot"
        |> minimizeFSA
        |> printDot @".\minimizedDFA.dot"
    //let firstSet = genFirstSet fsa
    do
        ()
    member this.States = fsa.States
    member this.StartState = fsa.StartState
    member this.FinalStates = fsa.FinalStates
    member this.NontermCount = fsa.StartStates.Length
    //member this.FirstSet = firstSet
    member this.StateToNontermName = fsa.StateToNontermName
    member this.PrintDot filePrintPath = printDot filePrintPath fsa