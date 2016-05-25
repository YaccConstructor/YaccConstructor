namespace Yard.EBNF.DFA.NumberedRules

open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.EBNF.GrammarWithDFARightSide
open Yard.Generators.Common.EBNF
open Yard.Generators.Common
//open Microsoft.FSharp.Collections
//open FSharpx.Collections.Experimental

type NumberedRulesDFA (ruleList : Rule.t<Source.t,Source.t> list, indexator : IndexatorEBNF, caseSensitive) =
    
    let rules = Array.ofList ruleList
    let start =
        rules
        |> Array.findIndex (fun rule -> rule.isStart)
    let nfaRules = new NumberedRulesEBNF(ruleList, indexator, caseSensitive, false, None)
    
    let nfaToDfa (stateToVertex : Vertex<_,_>[]) (startStates : int[]) (finishStates : Set<int>) =
        let setVerticesDict = new Dictionary<string, Vertex<int,int>>()
        let setVerticesQueue = new Queue<Vertex<_,_> * int[]>()

        let setVerticesToKey (setVertex : int[]) =
            setVertex |> Array.map string |> String.concat "|"
            
        let finishStates' = ref Set.empty

        let nextNewVertex =
            let count = ref 0
            fun key isFinish ->
                let vertex = new Vertex<_,_>(!count)
                if isFinish then
                    finishStates' := Set.add !count !finishStates'
                incr count
                setVerticesDict.[key] <- vertex
                vertex

        let move state symbol =
            seq {
                for edge in stateToVertex.[state].outEdges do
                    if edge.label = symbol then
                        yield edge.dest.label
            }
            
        let epsilonClose states =
            let closure = ref <| Set.ofSeq states
            let stack = new Stack<int>()
            for state in !closure do
                stack.Push state
            while stack.Count > 0 do
                let vertex = stateToVertex.[stack.Pop()]
                for edge in vertex.outEdges do
                    if edge.label = indexator.epsilonIndex && edge.dest.label |> closure.Value.Contains |> not then
                        closure := Set.add edge.dest.label !closure
                        stack.Push edge.dest.label
            Set.toArray !closure
            
        let startSetVertex = startStates |> epsilonClose
        nextNewVertex (setVerticesToKey startSetVertex) (Array.exists (fun x -> Set.contains x finishStates) startSetVertex)
        |> fun x -> setVerticesQueue.Enqueue (x, startSetVertex)
        let startState' = 0
        while setVerticesQueue.Count > 0 do
            let vertex, stateSet = setVerticesQueue.Dequeue()
            for i = 0 to indexator.fullCount - 1 do 
                let nextSet =
                    seq {
                        for state in stateSet do
                            yield! (move state i)
                    }
                    |> epsilonClose
                if nextSet.Length > 0 then
                    let nextKey = setVerticesToKey nextSet
                    if not <| setVerticesDict.ContainsKey nextKey then
                        let isFinish = Array.exists (fun x -> Set.contains x finishStates) nextSet
                        let nextVertex = nextNewVertex nextKey isFinish
                        vertex.addEdge (new Edge<_,_>(nextVertex, i))
                        setVerticesQueue.Enqueue (nextVertex, nextSet)
                    else
                        vertex.addEdge (new Edge<_,_>(setVerticesDict.[nextKey], i))
        let stateToVertex' = Array.zeroCreate setVerticesDict.Count
        for keyValue in setVerticesDict do
            stateToVertex'.[keyValue.Value.label] <- keyValue.Value
        (stateToVertex', startState', !finishStates')
    
    let revertAutoma (stateToVertex : Vertex<_,_>[]) (startStates : int[]) (finishStates : int[]) =
        let numberOfStates = stateToVertex.Length
        let stateToVertex' = Array.init numberOfStates (fun i -> new Vertex<_,_>(i))
        for state in stateToVertex do
            for edge in state.outEdges do
                let revertedEdge = new Edge<_,_>(stateToVertex'.[state.label], edge.label)
                stateToVertex'.[edge.dest.label].addEdge(revertedEdge)
        let startStates' = finishStates
        let finishStates' = startStates |> Set.ofArray
        (stateToVertex', startStates', finishStates')

    let rightReverted = 
        nfaRules.rightSideArr 
        |> Array.map (fun x -> revertAutoma x.stateToVertex [|x.startState.label|] [|x.stateToVertex.Length - 1|])
        |> Array.map (fun (stateToVertex, startStates, finishStates) -> nfaToDfa stateToVertex startStates finishStates)
    let right = 
        rightReverted 
        |> Array.map 
            (fun (stateToVertex, startStates, finishStates) -> revertAutoma stateToVertex [|startStates|] (finishStates|> Set.toArray))
        
    
    //debug
    (*let allNfaStates, allNfaEdges = 
        let foldProd (statesAcc, edgesAcc) (prod : NFAProduction.t) =
            let numberOfEdges = prod.stateToVertex |> Array.fold (fun sum (x : Vertex<_,_>) -> x.outEdges.Count + sum) 0
            (statesAcc + prod.numberOfStates, edgesAcc + numberOfEdges)
        Array.fold foldProd (0,0) nfaRules.rightSideArr

    let allDfaStates, allDfaEdges =
        let foldProd (statesAcc, edgesAcc) (prod : DFAProduction.t) =
            let numberOfEdges = prod.stateToVertex |> Array.fold (fun sum (x : Vertex<_,_>) -> x.outEdges.Count + sum) 0
            (statesAcc + prod.numberOfStates, edgesAcc + numberOfEdges)
        Array.fold foldProd (0,0) right

    let statesDiff = allNfaStates - allDfaStates
    let edgesDiff = allNfaEdges - allDfaEdges
    let p =
        printfn "DfaStates win %d (%d percent)" statesDiff (statesDiff*100/allNfaStates)
        printfn "DfaEdges win %d (%d percent)" edgesDiff (edgesDiff*100/allNfaEdges)
    //debug*)

    let left = nfaRules.leftSideArr
    let symbolAndNextPos =
        let result : (int * int)[] [][] = Array.zeroCreate rules.Length
        for i in 0..rules.Length-1 do
            let stateToVertex, _,_ = right.[i]
            result.[i] <- Array.create stateToVertex.Length [||]
            for j in 0..stateToVertex.Length-1 do
                result.[i].[j] <- stateToVertex.[j].outEdges |> Array.ofSeq |> Array.map (fun x -> (x.label, x.dest.label))
        result

    member this.rulesCount = nfaRules.rulesCount
    member this.startRule = nfaRules.startRule
    member this.startSymbol = left.[start]
    member this.leftSide num = left.[num]
    member this.leftSideArr = left
    member this.rightSide num = right.[num]
    member this.numberOfStates num = let stateToVertex, _, _ = right.[num] in stateToVertex.Length
    member this.state rule pos = let stateToVertex, _, _ = right.[rule] in stateToVertex.[pos]
    member this.startPos rule = let _, startStates, _ = right.[rule] in startStates
    //member this.table rule = getTable rule
    member this.symbolsAndNextPos rule pos =
        symbolAndNextPos.[rule].[pos]
    member this.ruleWithLeftSide symbol = nfaRules.ruleWithLeftSide symbol
    member this.finishStates num = let _, _, finishStates = right.[num] in finishStates
    member this.rightRevertedArr = rightReverted
    //member this.errorRulesExists = errRulesExists
