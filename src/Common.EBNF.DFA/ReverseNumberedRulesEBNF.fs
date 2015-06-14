module Yard.EBNF.DFA.ReverseNumberedRules

open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.EBNF.DFA.GrammarWithNFARightSide
open Yard.EBNF.DFA.Indexator
open Yard.EBNF.DFA.NumberedRules
open Yard.Generators.Common

type ReverseNumberdRulesEBNF (rules : NumberedRulesEBNF, indexator : IndexatorEBNF) =
    let rightReverseNFA =
        let reverseNFA (nfaProduction : NFAProduction.t) =
            let number = ref -1
            let nextReverseStateNumber =
                incr number
                !number
            let rec buildReverseStateVertex (currentState : Vertex<_,_>) (currentReverseState : Vertex<_,_>) (stateToReverseVertex : ResizeArray<_>) (processedVertex : HashSet<_>) =
                if not (processedVertex.Contains(currentState.label))
                then
                    processedVertex.Add(currentState.label) |> ignore
                    stateToReverseVertex.Add(currentReverseState)
                    let currentEdges = currentState.outEdges
                    for edge in currentEdges do
                        let vertexDest = edge.dest
                        let vertexDestLabel = vertexDest.label
                        let labelEdge = edge.label
                        if processedVertex.Contains(vertexDestLabel)
                        then
                            let vertexDestReverse = stateToReverseVertex.[vertexDestLabel]
                            vertexDestReverse.addEdge(new Edge<_,_>(currentReverseState, labelEdge))
                        else
                            let vertexDestReverse = new Vertex<_,_>(vertexDest.label)
                            vertexDestReverse.addEdge(new Edge<_,_>(currentReverseState, labelEdge))
                            buildReverseStateVertex vertexDest vertexDestReverse stateToReverseVertex processedVertex

            let stateToReverseVertex = new ResizeArray<Vertex<_,_>>(nfaProduction.numberOfStates)
            let firstState = nfaProduction.startState
            let lastReverseState = new Vertex<_,_>(firstState.label)
            let setOfLabels = new HashSet<_>()
            buildReverseStateVertex firstState lastReverseState stateToReverseVertex setOfLabels
            let firstReverseState = stateToReverseVertex.[stateToReverseVertex.Count - 1]
            let prodReverse : NFAProduction.t = {numberOfStates = nfaProduction.numberOfStates; startState = firstReverseState; stateToVertex = stateToReverseVertex |> Seq.toArray}
            prodReverse

        Array.map (fun i -> rules.rightSide i |> reverseNFA) [|0..(rules.rulesCount - 1)|]
            
//    let symbolAndNextPos =
//        let result : (int * int) [][] = Array.zeroCreate rules.rulesCount
//        for i in 0..rules.rulesCount - 1 do
//            let nfaReverse = rightReverseNFA.[i]
//            result.[i] <- Array.create nfaReverse.numberOfStates (indexator.epsilonIndex, 0)
//            for j in 0..nfaReverse.numberOfStates - 1 do
//                let rec getSymbol = function
//                |[] -> ()
//                |(x : Edge<_,_>)::xs -> 
//                    if x.label <> indexator.epsilonIndex then result.[i].[j] <- (x.label, x.dest.label) else getSymbol xs
//                nfaReverse.stateToVertex.[j].outEdges |> List.ofSeq |> getSymbol
//        result

    let getTable rule =
        let nfaReverseProd = rightReverseNFA.[rule] 
        printfn "Number of states = %A" nfaReverseProd.numberOfStates
        let result : HashSet<_>[][] = Array.zeroCreate nfaReverseProd.numberOfStates
        for i in 0..nfaReverseProd.numberOfStates - 1 do
            result.[i] <- Array.zeroCreate (indexator.fullCount + 1)
        for i in 0..nfaReverseProd.numberOfStates - 1 do
            for j in 0..indexator.fullCount do
                result.[i].[j] <- new HashSet<_>() 

        printfn "Full count indexator = %A" indexator.fullCount
        let rec collectStates (currentState : Vertex<_,_>) (processedStates : HashSet<_>) =
            if not (processedStates.Contains(currentState.label)) then
                processedStates.Add(currentState.label) |> ignore
                let edges = currentState.outEdges
                for edge in edges do
                    let label = edge.label
                    let vertexDist = edge.dest
                    result.[currentState.label].[label].Add(vertexDist.label) |> ignore
                    collectStates vertexDist processedStates
        let startState = nfaReverseProd.startState
        let processedStates = new HashSet<_>()
        collectStates startState processedStates
        result  

    member this.rulesCount = rules.rulesCount
    member this.startRule = rules.startRule
    member this.rightReverseSide num = rightReverseNFA.[num]
    member this.numberOfStates num = rightReverseNFA.[num].numberOfStates
    member this.state rule pos = rightReverseNFA.[rule].stateToVertex.[pos]
    member this.getStateTable rule = getTable rule 
    //member this.rulesWithLeftSide = 




type ReverseNumberdRulesDFA (rules : NumberedRulesEBNF, indexator : IndexatorEBNF) = 
    let getDFATable (rule : NFAProduction.t) (indexator : IndexatorEBNF) =
        let finiteStatesDFA = new HashSet<_>()
        let containsSet (statesDFA : ResizeArray<_>) (statesNFA : HashSet<_>) = 
            match Seq.tryFindIndex (fun set -> statesNFA.SetEquals(set)) statesDFA with
            | Some index    -> (true, index)
            | None          -> (false, -1)

        let move (stateSet : HashSet<_>) symbol =
            let result = new HashSet<_>()
            for state in stateSet do
                let vertex = rule.stateToVertex.[state]
                let edges = vertex.outEdges
                for edge in edges do
                    if indexator.epsilonIndex <> edge.label && edge.label = symbol
                    then
                        result.Add(edge.dest.label) |> ignore
            result

        let epsilonClosureSet (set : HashSet<_>) =
            let stack = new Stack<_>(set)
            let resultSet = new HashSet<_>(set)
            while stack.Count <> 0 do
                let numOfVertex = stack.Pop()
                let vertex = rule.stateToVertex.[numOfVertex]
                let edges = vertex.outEdges
                for edge in edges do
                    if edge.label = indexator.epsilonIndex
                    then
                        let vertexDist = edge.dest
                        if not (resultSet.Contains(vertexDist.label))
                        then
                            resultSet.Add(vertexDist.label) |> ignore
                            stack.Push(vertexDist.label)
            resultSet

        let table = 
            let resultDict = new Dictionary<_,_>()

            let startStateNFA = rule.startState
            let startSetNFA = new HashSet<_>()
            startSetNFA.Add(startStateNFA.label) |> ignore

            let statesDFA = new ResizeArray<_>()
            let matchStatesDFA = new Queue<_>()
            let startStateDFA = epsilonClosureSet startSetNFA
            statesDFA.Add(startStateDFA)
            matchStatesDFA.Enqueue((startStateDFA, 0))

            if startStateDFA.Contains(0)
            then
                finiteStatesDFA.Add(0) |> ignore
        
            while matchStatesDFA.Count <> 0 do
                let state = matchStatesDFA.Dequeue()
                let set = fst state
                let numberOfState = snd state
                for i in 0..indexator.fullCount do
                    let nextSet = move set i |> epsilonClosureSet
                    if nextSet.Count <> 0
                    then
                        let containsNextSet = containsSet statesDFA nextSet
                        if not (fst containsNextSet )
                        then
                            statesDFA.Add(nextSet)
                            matchStatesDFA.Enqueue(nextSet, statesDFA.Count - 1)
                            resultDict.Add((numberOfState, i), statesDFA.Count - 1)
                        else
                            resultDict.Add((numberOfState, i), snd containsNextSet)
                        if nextSet.Contains(0)
                        then
                            finiteStatesDFA.Add(statesDFA.Count - 1) |> ignore

            let result : int[][] = Array.zeroCreate statesDFA.Count
            for i in 0..statesDFA.Count - 1 do
                result.[i] <- Array.create indexator.fullCount -1
                for j in 0..indexator.fullCount - 1 do
                    if resultDict.ContainsKey((i, j))
                    then
                        result.[i].[j] <- resultDict.[(i, j)]
                    else
                        result.[i].[j] <- -1
            result
        table, finiteStatesDFA


    let reverseNumberedRulesEBNF = ReverseNumberdRulesEBNF(rules, indexator)

    member this.rulesCount = rules.rulesCount
    member this.startRule = rules.startRule
    member this.dfaTable rule = getDFATable (reverseNumberedRulesEBNF.rightReverseSide rule) indexator