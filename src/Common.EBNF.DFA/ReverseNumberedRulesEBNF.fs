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
            let rec buildReverseStateVertex (currentState : Vertex<_,_>) (currentReverseState : Vertex<_,_>) (firstReverseState : Vertex<_,_>) (processedVertex : HashSet<_>) =
                processedVertex.Add(currentState.label) |> ignore
                let currentEdges = currentState.outEdges
                if currentEdges.Count = 0 then
                    firstReverseState = currentReverseState |> ignore
                for edge in currentEdges do
                    let vertexDest = edge.dest
                    let labelEdge = edge.label
                    let vertexDestReverse = new Vertex<_,_>(vertexDest.label)
                    vertexDestReverse.addEdge(new Edge<_,_>(currentReverseState, labelEdge))
                    if not (processedVertex.Contains(vertexDest.label)) then
                        buildReverseStateVertex vertexDest vertexDestReverse firstReverseState processedVertex
            let rec setReverseStateLabels (currentState : Vertex<_,_>) (stateToReverseVertex : ResizeArray<Vertex<_,_>>) =
                currentState.setLabel(nextReverseStateNumber)
                stateToReverseVertex.Add(currentState)
                let currentEdges = currentState.outEdges
                for edge in currentEdges do
                    setReverseStateLabels edge.dest stateToReverseVertex

            let stateToReverseVertex = new ResizeArray<Vertex<_,_>>()
            let firstState = nfaProduction.startState
            let lastReverseState = new Vertex<_,_>(firstState.label)
            let firstReverseState = null
            let setOfLabels = new HashSet<_>()
            buildReverseStateVertex firstState lastReverseState firstReverseState setOfLabels
            setReverseStateLabels firstReverseState stateToReverseVertex
            let prodReverse : NFAProduction.t = {numberOfStates = nfaProduction.numberOfStates; startState = firstReverseState; stateToVertex = stateToReverseVertex |> Seq.toArray}
            prodReverse

        Array.map (fun i -> rules.rightSide i |> reverseNFA) [|0..rules.rulesCount|]
            
    let symbolAndNextPos =
        let result : (int * int) [][] = Array.zeroCreate rules.rulesCount
        for i in 0..rules.rulesCount - 1 do
            let nfaReverse = rightReverseNFA.[i]
            result.[i] <- Array.create nfaReverse.numberOfStates (indexator.epsilonIndex, 0)
            for j in 0..nfaReverse.numberOfStates - 1 do
                let rec getSymbol = function
                |[] -> ()
                |(x : Edge<_,_>)::xs -> 
                    if x.label <> indexator.epsilonIndex then result.[i].[j] <- (x.label, x.dest.label) else getSymbol xs
                nfaReverse.stateToVertex.[j].outEdges |> List.ofSeq |> getSymbol
        result

    let getTable rule =
        let nfaReverseProd = rightReverseNFA.[rule] 
        let result : HashSet<_>[][] = Array.zeroCreate nfaReverseProd.numberOfStates
        for i in 0..nfaReverseProd.numberOfStates do
            result.[i] <- Array.create indexator.fullCount (new HashSet<int>())
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

    member this.rulesCount = rules.rulesCount
    member this.startRule = rules.startRule
    member this.rightReverseSide num = rightReverseNFA.[num]
    member this.numberOfStates num = rightReverseNFA.[num].numberOfStates
    member this.state rule pos = rightReverseNFA.[rule].stateToVertex.[pos]
    member this.symbol rule pos = 
        let (symbol, _) = symbolAndNextPos.[rule].[pos]
        symbol
    member this.nextPos rule pos = 
        let (_, nextPos) = symbolAndNextPos.[rule].[pos]
        nextPos
    member this.getStateTable rule = getTable rule 
    //member this.rulesWithLeftSide = 

//type ReverseNumberdRulesDFA (rules : ReverseNumberdRulesEBNF) = 
//    
//    member this.rulesCount = rules.rulesCount
//    member this.startRule = rules.startRule
//    member this.startSymbol = 
//    member this.leftSideArr = 
//    member this.rightSide num = 
//    member this.numberOfStates num = 
//    member this.state rule pos = 
//    member this.symbol rule pos = 
//    member this.nextPos rule pos = 
//    member this.rulesWithLeftSide = 