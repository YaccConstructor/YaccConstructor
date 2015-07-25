module Yard.EBNF.DFA.SymbolSets

open Yard.Generators.Common
open Yard.EBNF.DFA.GrammarWithNFARightSide
open Yard.EBNF.DFA.Indexator
open Yard.EBNF.DFA.NumberedRules

let firstSetNFA (rules : NumberedRulesEBNF) (indexator : IndexatorEBNF) (canInferEpsilon : bool[]) =
    let result : Set<int>[] = Array.create indexator.fullCount Set.empty
    
    let calc term = 
    //for term = indexator.nonTermCount to indexator.fullCount-1 do
        let was : bool[] = Array.zeroCreate indexator.fullCount
        let rec dfs u = 
            result.[u] <- result.[u].Add term
            was.[u] <- true
                        
            let rec check (prod : NFAProduction.t) = 
                let rec checkOutSymbols (state : Vertex<_,_>) =
                    let rec checkEdges = function
                        |[] -> false
                        | (x : Edge<_,_>) :: xs -> 
                            if (x.label = u) then true
                            elif (canInferEpsilon.[x.label] && state.label < x.dest.label) then checkOutSymbols x.dest || checkEdges xs
                            else checkEdges xs
                    if state.label >= prod.numberOfStates then false
                    else state.outEdges |> List.ofSeq |> checkEdges
                checkOutSymbols prod.startState
                    

            for i = 0 to rules.rulesCount-1 do
                let v = rules.leftSide i
                if (not was.[v]) then
                    if (check (rules.rightSide i)) then
                      dfs v
        dfs term

    calc indexator.errorIndex
    for term = indexator.nonTermCount to indexator.fullCount-1 do
        calc term
    for nonTerm = 1 to indexator.nonTermCount do
        if(canInferEpsilon.[nonTerm]) then
            result.[nonTerm] <- result.[nonTerm].Add indexator.epsilonIndex
    result

let followSetNFA (rules : NumberedRulesEBNF) (indexator : IndexatorEBNF) (canInferEpsilon : bool[]) (firstSet : Set<int>[]) =
    let result : Set<int>[][] = Array.zeroCreate rules.rulesCount
        
    
    for i = 0 to rules.rulesCount-1 do
        result.[i] <- Array.create (rules.numberOfStates i) Set.empty
        let evaluateFollowWithoutReturns() =
            for j = rules.numberOfStates i - 1 downto 0 do
                result.[i].[j] <- Set.empty
                let state = rules.state i j
                for edge in state.outEdges |> List.ofSeq |> List.filter (fun (x : Edge<_,_>) -> state.label < x.dest.label) do
                    if (canInferEpsilon.[edge.label]) then 
                        result.[i].[j] <- Set.union result.[i].[j] result.[i].[edge.dest.label]
                    if (edge.label <> indexator.epsilonIndex) then 
                        result.[i].[j] <- Set.union result.[i].[j] firstSet.[edge.label]
        let rec addFollows() =
            let mutable modified = false
            for j = rules.numberOfStates i - 1 downto 0 do
                let state = rules.state i j
                for edge in state.outEdges do
                    let nextFollow = result.[i].[edge.dest.label]
                    if (canInferEpsilon.[edge.label] && not (Set.isSubset nextFollow result.[i].[j])) then
                        modified <- true
                        result.[i].[j] <- Set.union result.[i].[j] nextFollow
            if modified then addFollows()
        evaluateFollowWithoutReturns()
        addFollows()
    result
