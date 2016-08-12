namespace Yard.Generators.Common.FSA

open System.Collections.Generic

open Yard.Generators.Common.FSA.Common

open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Generators.Common.Epsilon
open Yard.Generators.Common.SymbolSets
open Yard.Generators.Common

type FSA(ruleList : Rule.t<Source.t,Source.t> list) =
    let _states = ref None
    let _startState = ref None
    let _finalState = ref None
    let _nontermsCount = ref None
    let _nontermStringDict = ref None
    let _firstSet = ref None

    do
        let states, alphabet, nonterms, nontermStringDict, startState, finalState, finalStatesOfComponents = convertRulesToFSA ruleList

        printDot @"C:\zgrviewer-0.10.0\Graphs\initFSA.dot" states startState finalState nontermStringDict

        let classes = findEquivalenceClasses states finalStatesOfComponents alphabet nontermStringDict
        let states, nontermsCount, nontermStringDict, startState, finalState = buildEquivalentFSA states alphabet classes nonterms nontermStringDict startState finalState
        
        _states := states |> Some
        _startState := startState |> Some
        _finalState := finalState |> Some
        _nontermsCount := nonterms.Count |> Some
        _nontermStringDict := nontermStringDict |> Some
        printDot @"C:\zgrviewer-0.10.0\Graphs\minimizedFSA.dot" states startState finalState nontermStringDict
        _firstSet := genFirstSet states nontermsCount |> Some

    member this.States = (!_states).Value
    member this.StartState = (!_startState).Value
    member this.FinalState = (!_finalState).Value
    member this.NontermCount = (!_nontermsCount).Value
    member this.FirstSet = (!_firstSet).Value
    member this.IntToString = (!_nontermStringDict).Value
    member this.PrintDot filePrintPath = printDot filePrintPath this.States this.StartState this.FinalState this.IntToString