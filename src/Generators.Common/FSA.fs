namespace Yard.Generators.Common.FSA

open System.Collections.Generic

open Yard.Generators.Common.FSA.Common
open Microsoft.FSharp.Collections
open AbstractAnalysis.Common
open Yard.Core.IL
open Yard.Generators.Common.Epsilon
open Yard.Generators.Common.SymbolSets
open Yard.Generators.Common

type FSA(fsa: InternalFSA) =       

    member this.States = fsa.States
    //after convertion to dfa each startstates HashSet contains only one state
    member this.StartState = fsa.StartStatesOfEachNonterminal.[fsa.StartComponentNumber] |> Seq.find (fun x -> true)
    member this.FinalStates = fsa.FinalStates
    member this.NontermCount = fsa.StartStatesOfEachNonterminal.Length
    member this.StateToNontermName = fsa.StateToNontermName
    //member this.LastStates = fsa.LastStates
    member this.StartStates = fsa.StartStatesOfEachNonterminal
    member this.PrintDot filePrintPath = printDot filePrintPath fsa
    //member this.RuleList = ruleList
    member this.Alphabet = fsa.Alphabet