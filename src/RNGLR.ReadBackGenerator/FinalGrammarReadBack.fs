namespace Yard.ReadBack.FinalGrammar

open Yard.Core.IL
open Yard.EBNF.FinalGrammar
open Yard.EBNF.GrammarWithNFARightSide
open Yard.Generators.Common   

type FinalGrammarReadBack(ruleList : Rule.t<Source.t,Source.t> list, caseSensitive) as this =
    inherit FinalGrammarNFA(ruleList, caseSensitive)

    //expected that in every right part states # start from 0
    let invertNfa (rightPart : NFAProduction.t) : NFAProduction.t =
        let numberOfStates = rightPart.numberOfStates
        let invertNum num = numberOfStates - 1 - num
        let stateToVertex = 
            Array.init numberOfStates 
            <| fun i -> new Vertex<int,int>(rightPart.stateToVertex.[invertNum i].label)
        let startState = stateToVertex.[0]
        for i = 0 to numberOfStates - 1 do
            let destination = stateToVertex.[invertNum i]
            for edge in rightPart.stateToVertex.[i].outEdges do
                stateToVertex.[invertNum edge.label].addEdge
                 <| new Edge<int,int>(destination, edge.label)
        let prod : NFAProduction.t = {numberOfStates = numberOfStates; startState = startState; stateToVertex = stateToVertex}
        prod
    
    let _invertedRight = this.rules.rightSideArr |> Array.map invertNfa

    member this.invertedRightSide num = _invertedRight.[num]