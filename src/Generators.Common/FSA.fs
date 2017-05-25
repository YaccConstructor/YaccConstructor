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
        printfn "heeeeeeyyeyeyyezlskjfzes,mnzsjkvhzlsjrg"
        ruleList
        |> convertRulesToFSA  |> printDot @".\FSA1initialFSA.dot"
        |> removeEpsilonEdges //|> printDot @".\FSA2withoutEpsilon.dot"
        //|> toDFA              //|> printDot @".\FSA3determ.dot"
        //|> minimizeFSA        |> printDot @".\FSA4minimized.dot"
    //let firstSet = genFirstSet fsa

    member this.States = fsa.States
    member this.StartState = fsa.StartState
    member this.FinalStates = fsa.FinalStates
    member this.NontermCount = fsa.StartStates.Length
    //member this.FirstSet = firstSet
    member this.StateToNontermName = fsa.StateToNontermName
    member this.PrintDot filePrintPath = printDot filePrintPath fsa
    member this.RuleList = ruleList
