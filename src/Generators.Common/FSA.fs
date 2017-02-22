namespace Yard.Generators.Common.FSA

open System.Collections.Generic

open Yard.Generators.Common.FSA.Common

open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Generators.Common.Epsilon
open Yard.Generators.Common.SymbolSets
open Yard.Generators.Common

type FSA(ruleList : Rule.t<Source.t,Source.t> list, printDebug : bool) =
    let fsa =
        ruleList
        |> convertRulesToFSA
        |> (fun x -> if printDebug then printDot @".\FSA1initialFSA.dot" x else x)
        |> removeEpsilonEdges 
        |> (fun x -> if printDebug then printDot @".\FSA2withoutEpsilon.dot" x else x)
        |> toDFA 
        |> (fun x -> if printDebug then printDot @".\FSA3determ.dot" x else x)
        |> minimizeFSA   
        |> (fun x -> if printDebug then printDot @".\FSA4minimized.dot" x else x)
    //let firstSet = genFirstSet fsa

    member this.States = fsa.States
    member this.StartState = fsa.StartState
    member this.FinalStates = fsa.FinalStates
    member this.NontermCount = fsa.StartStates.Length
    //member this.FirstSet = firstSet
    member this.StateToNontermName = fsa.StateToNontermName
    member this.PrintDot filePrintPath = printDot filePrintPath fsa