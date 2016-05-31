module Yard.EBNF.DFA.SymbolSets

open Yard.Generators.Common
open Yard.Generators.Common.EBNF
open Yard.EBNF.GrammarWithDFARightSide
open Yard.EBNF.DFA.NumberedRules

let firstSetDFA (rules : NumberedRulesDFA) (indexator : IndexatorEBNF) (canInferEpsilon : bool[]) =
    let result : Set<int>[] = Array.create indexator.fullCount Set.empty
    
    let calc term = 
    //for term = indexator.nonTermCount to indexator.fullCount-1 do
        let was : bool[] = Array.zeroCreate indexator.fullCount
        let rec dfs u = 
            result.[u] <- result.[u].Add term
            was.[u] <- true
                        
            let check prod = 
                let visitedVertices = ref Set.empty
                let rec checkOutSymbols (state : Vertex<_,_>) =
                    visitedVertices := visitedVertices.Value.Add state.label
                    let rec checkEdges = function
                        |[] -> false
                        | (x : Edge<_,_>) :: xs -> 
                            if (x.label = u) then true
                            elif canInferEpsilon.[x.label] && (not <| visitedVertices.Value.Contains x.dest.label) then checkOutSymbols x.dest || checkEdges xs
                            else checkEdges xs
                    if state.label >= rules.numberOfStates prod then false
                    else state.outEdges |> List.ofSeq |> checkEdges
                checkOutSymbols (rules.state prod (rules.startPos prod))

            for i = 0 to rules.rulesCount-1 do
                let v = rules.leftSide i
                if (not was.[v]) then
                    if (check i) then
                      dfs v
        dfs term

    calc indexator.errorIndex
    for term = indexator.nonTermCount to indexator.fullCount-1 do
        calc term
    (*for nonTerm = 1 to indexator.nonTermCount do
        if(canInferEpsilon.[nonTerm]) then
            result.[nonTerm] <- result.[nonTerm].Add indexator.epsilonIndex*)
    result

let followSetDFA (rules : NumberedRulesDFA) (indexator : IndexatorEBNF) (canInferEpsilon : bool[]) (firstSet : Set<int>[]) =
    let result : Set<int>[][] = Array.zeroCreate rules.rulesCount        
    
    for i = 0 to rules.rulesCount-1 do
        result.[i] <- Array.init (rules.numberOfStates i) 
            (fun j ->
                let mutable curSet = Set.empty
                let stateVertex = rules.state i j
                for edge in stateVertex.outEdges do
                    curSet <- Set.union firstSet.[edge.label] curSet
                curSet)
        let mutable modified = true
        while modified do
            modified <- false
            for j = 0 to rules.numberOfStates i - 1 do
                let stateVertex = rules.state i j
                for edge in stateVertex.outEdges do
                    if canInferEpsilon.[edge.label] then
                        let diff = Set.difference result.[i].[edge.dest.label] result.[i].[j]
                        if not diff.IsEmpty then
                            result.[i].[j] <- Set.union diff result.[i].[j]
                            modified <- true                
    result
