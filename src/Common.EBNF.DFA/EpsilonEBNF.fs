module Yard.EBNF.DFA.Epsilon

open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Generators.Common
open Yard.EBNF.DFA.Indexator
open Yard.EBNF.DFA.NumberedRules

let canInferEpsilonNFA (rules : NumberedRulesEBNF) (indexator : IndexatorEBNF) =
    let result : bool[] = Array.zeroCreate (indexator.fullCount + 1) //+1 for epsilon
    let mutable modified = true
    result.[indexator.errorIndex] <- true
    result.[indexator.epsilonIndex] <- true
    while modified do
        modified <- false
        for i in 0..rules.rulesCount-1 do
            if not result.[rules.leftSide i] then
                let rec checkEpsilon stateNum =
                    if stateNum = (rules.numberOfStates i) - 1 then true
                    else
                        let stateVertex = rules.state i stateNum
                        let rec checkEdges = function
                        |[] -> false
                        |(x : Edge<_,_>)::xs ->
                            if result.[x.label] && x.dest.label > stateNum && checkEpsilon x.dest.label then true
                            else checkEdges xs
                        stateVertex.outEdges |> List.ofSeq |> checkEdges
                if checkEpsilon 0 then
                    modified <- true
                    result.[rules.leftSide i] <- true
    result

let hasEpsilonTail (rules : NumberedRulesEBNF) (canInferEpsilon : bool[]) =
    let result : bool[][] = Array.zeroCreate (rules.rulesCount)
    for i in 0..rules.rulesCount-1 do
        result.[i] <- Array.zeroCreate (rules.numberOfStates i)
        result.[i].[rules.numberOfStates i - 1] <- true
        for j = rules.numberOfStates i - 2 downto 0 do
            let state = rules.state i j
            let rec hasInfEpsilonSymbol = function
            |[] -> false
            |(x : Edge<_,_>)::xs ->
                if result.[i].[x.dest.label] && canInferEpsilon.[x.label] then true
                else hasInfEpsilonSymbol xs
            result.[i].[j] <-
                (state.outEdges |> List.ofSeq |> List.filter (fun x -> state.label < x.dest.label) |> hasInfEpsilonSymbol)
    result

