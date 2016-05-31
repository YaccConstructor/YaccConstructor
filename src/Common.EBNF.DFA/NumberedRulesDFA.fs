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

    let minimize (stateToVertex : Vertex<_,_>[]) (startState : int) (finishStates : Set<int>) =
        let statesN = stateToVertex.Length
        //
        let nonequalityTable = Array.zeroCreate (statesN - 1)
        for i = 0 to statesN - 2 do
            nonequalityTable.[i] <- Array.create (statesN - i - 1) false
        let check x y = 
            if x = y then false 
            elif x < y then nonequalityTable.[x].[statesN - y - 1] 
            else nonequalityTable.[y].[statesN - x - 1]
        let set x y = if x < y then nonequalityTable.[x].[statesN - y - 1] <- true else nonequalityTable.[y].[statesN - x - 1] <- true
        for i = 0 to statesN - 2 do
            for j = i + 1 to statesN - 1 do
                if finishStates.Contains i <> finishStates.Contains j then
                    set i j
        let modified = ref true
        while !modified do
            modified := false
            for i = 0 to statesN - 2 do
                for j = i + 1 to statesN - 1 do
                    if not <| check i j then
                        for iEdge in stateToVertex.[i].outEdges do
                            let wasLabel = ref false
                            for jEdge in stateToVertex.[j].outEdges do
                                if iEdge.label = jEdge.label then
                                    wasLabel := true
                                    if check iEdge.dest.label jEdge.dest.label then
                                        set i j
                                        modified := true
                            if not !wasLabel then
                                set i j
                                modified := true
                        for jEdge in stateToVertex.[j].outEdges do
                            let wasLabel = stateToVertex.[i].outEdges |> Seq.fold (fun x y -> x || y.label = jEdge.label) false
                            if not wasLabel then
                                set i j
                                modified := true

        let newVerticesDict = Array.create statesN None
        let newStateToVertexList = new ResizeArray<_>()
        let nextVertex oldState =
            let res = new Vertex<_,_>(newStateToVertexList.Count)
            newStateToVertexList.Add (res, oldState)
            res
        for i = 0 to statesN - 2 do
            match newVerticesDict.[i] with
            | Some x -> ()
            | None -> 
                let newVertex = nextVertex i
                newVerticesDict.[i] <- Some newVertex
                for j = i + 1 to statesN - 1 do
                    if not <| check i j then
                        newVerticesDict.[j] <- Some newVertex
        if newVerticesDict.[statesN - 1].IsNone then
            let newVertex = nextVertex (statesN - 1)
            newVerticesDict.[statesN - 1] <- Some newVertex
        for (newVertex, oldState) in newStateToVertexList do
            for edge in stateToVertex.[oldState].outEdges do
                let newDest = newVerticesDict.[edge.dest.label].Value
                newVertex.addEdge(new Edge<_,_>(newDest,edge.label))

        let newStateToVertex = newStateToVertexList |> Seq.map (fun (x, _) -> x) |> Seq.toArray
        let newStartState = newVerticesDict.[startState].Value.label
        let newFinishStates = finishStates |> Set.map (fun x -> newVerticesDict.[x].Value.label)
        newStateToVertex, newStartState, newFinishStates                            

    let right = 
        nfaRules.rightSideArr 
        |> Array.map (fun x -> nfaToDfa x.stateToVertex [|x.startState.label|] ([x.numberOfStates - 1] |> Set.ofList))
        |> Array.map (fun (stateToVertex, startState, finishStates) -> minimize stateToVertex startState finishStates)

    let rightReverted = 
        right 
        |> Array.map (fun (stateToVertex, startState, finishStates) -> revertAutoma stateToVertex [|startState|] (finishStates |> Set.toArray))
    //debug
    (*let moreThanOneFinishStates =
        let acc = ref 0
        let max = ref 0
        rightReverted 
        |> Array.iter 
            (fun (_, _, finishStates) -> 
                if finishStates.Count > 1 then 
                    incr acc
                    if finishStates.Count > !max then
                        max := finishStates.Count
            )
        printfn "%d out of %d are polyfinish, max %d" !acc rightReverted.Length !max
    //*)
        
    
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
    member this.startPos rule = let _, startState, _ = right.[rule] in startState
    //member this.table rule = getTable rule
    member this.symbolsAndNextPos rule pos =
        symbolAndNextPos.[rule].[pos]
    member this.ruleWithLeftSide symbol = nfaRules.ruleWithLeftSide symbol
    member this.finishStates num = let _, _, finishStates = right.[num] in finishStates
    member this.rightRevertedArr = rightReverted
    //member this.errorRulesExists = errRulesExists
