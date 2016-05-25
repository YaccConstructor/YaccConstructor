module Yard.EBNF.DFA.Epsilon

open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Generators.Common
open Yard.Generators.Common.EBNF
open Yard.EBNF.DFA.NumberedRules


let epsilonsDFA (rules : NumberedRulesDFA) (indexator : IndexatorEBNF) =
    let canInferEpsilon : bool[] = Array.zeroCreate (indexator.fullCount)
    let hasEpsilonTail : bool[][] = Array.zeroCreate (rules.rulesCount)
    for i in 0..rules.rulesCount-1 do
        hasEpsilonTail.[i] <- Array.zeroCreate (rules.numberOfStates i)
        for state in rules.finishStates i do
            hasEpsilonTail.[i].[state] <- true

    let mutable modified = true
    while modified do
        modified <- false
        for i in 0..rules.rulesCount-1 do
            if not canInferEpsilon.[rules.leftSide i] then
                if rules.startPos i |> Array.exists (fun x -> hasEpsilonTail.[i].[x]) then
                    modified <- true
                    canInferEpsilon.[rules.leftSide i] <- true

        for i in 0..rules.rulesCount-1 do
            for j = rules.numberOfStates i - 1 downto 0 do
                if not hasEpsilonTail.[i].[j] then
                    let state = rules.state i j
                    let rec hasInfEpsilonSymbol = function
                    |[] -> false
                    |(x : Edge<_,_>)::xs ->
                        if hasEpsilonTail.[i].[x.dest.label] && canInferEpsilon.[x.label] then true
                        else hasInfEpsilonSymbol xs
                    if (state.outEdges |> List.ofSeq |> hasInfEpsilonSymbol) then
                        hasEpsilonTail.[i].[j] <- true
                        modified <- true

    canInferEpsilon, hasEpsilonTail