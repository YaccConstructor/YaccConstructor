module Yard.EBNF.StateSets

open Yard.Generators.Common
open Yard.EBNF.Indexator
open Yard.EBNF.NumberedRules

//only edges with epsilon, not with terminals that can infer epsilon
let epsilonReachable (rules : NumberedRulesEBNF) (indexator : IndexatorEBNF) =
    let result : Set<int>[][] = Array.zeroCreate rules.rulesCount
    for i in 0..rules.rulesCount-1 do
        result.[i] <- Array.create (rules.numberOfStates i) Set.empty
        let was : bool[] = Array.zeroCreate (rules.numberOfStates i)
        let rec getEpsilonReachable (state : Vertex<_,_>) =
            result.[i].[state.label] <- Set.add state.label result.[i].[state.label]
            for edge in state.outEdges do
                if edge.label = indexator.epsilonIndex && not <| Set.contains edge.dest.label result.[i].[state.label] then 
                    result.[i].[state.label] <- Set.add edge.dest.label result.[i].[state.label]
                    if not was.[edge.dest.label] then getEpsilonReachable edge.dest
                    result.[i].[state.label] <- Set.union result.[i].[state.label] result.[i].[edge.dest.label]
            was.[state.label] <- true        
        for j = rules.numberOfStates i - 1 downto 0 do
            if not was.[j] then getEpsilonReachable (rules.state i j)
    result

let usefulStates (rules : NumberedRulesEBNF) (indexator : IndexatorEBNF) = 
        let result : Set<int>[] = Array.create rules.rulesCount Set.empty
        for i in 0..rules.rulesCount-1 do
            for j = 0 to (rules.rightSide i).numberOfStates - 1 do
                let symbol = rules.symbol i j
                if symbol <> indexator.epsilonIndex then result.[i] <- result.[i].Add j
            result.[i] <- result.[i].Add ((rules.rightSide i).numberOfStates - 1) //last state
        result

let startPositions (rules : NumberedRulesEBNF) (epsilonReachable : Set<int>[][]) (usefulStates : Set<int>[]) =
    let result : Set<int>[] = Array.create rules.rulesCount Set.empty
    for i in 0..rules.rulesCount-1 do
        result.[i] <- Set.intersect epsilonReachable.[i].[0] usefulStates.[i]
    result

let nextPositions (rules : NumberedRulesEBNF) (indexator : IndexatorEBNF) (epsilonReachable : Set<int>[][]) (usefulStates : Set<int>[]) =
    let result : Set<int>[][] = Array.zeroCreate rules.rulesCount
    for i in 0..rules.rulesCount-1 do
        result.[i] <- Array.create (rules.numberOfStates i) Set.empty
        for j in 0..rules.numberOfStates i - 1 do
            let (symbol, nextPos) = (rules.symbol i j, rules.nextPos i j)
            if symbol <> indexator.epsilonIndex then
                result.[i].[j] <- Set.intersect (usefulStates.[i]) epsilonReachable.[i].[nextPos]
            else
                result.[i].[j] <- Set.intersect (usefulStates.[i]) epsilonReachable.[i].[j]
    result