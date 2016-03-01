module Yard.Generators.RIGLRGenerator.Automata

open Yard.Generators.Common.FinalGrammar
open QuickGraph.FSA.GraphBasedFsa
open System.Collections.Generic
open QuickGraph.Algorithms
open QuickGraph

type RCAEdge =
    | Sh of int
    | Ri of int
    | Push of int
 
let constructRIA (grammar : FinalGrammar) =  
    let statesToItems = new Dictionary<int, int*int>(100)
    let itemsToStates = new Dictionary<int*int, ResizeArray<int>>(100)
    let primaryParent = new Dictionary<int, int>(100)
    let dealtWith = new HashSet<int>()
    let reductionStates = new ResizeArray<int>(10)
    let nextState = ref 1   

    let IRIA = new FSA<RCAEdge>()         
    
    let addState state item parent edgeTag =
        IRIA.AddVerticesAndEdge (new EdgeFSA<RCAEdge>(parent, state, edgeTag)) |> ignore
        primaryParent.Add (state, parent)
        statesToItems.Add (state, item)
        if itemsToStates.ContainsKey item
        then itemsToStates.[item].Add state
        else itemsToStates.Add (item, new ResizeArray<int>([state]))
        incr nextState

    let rec findTargetStateOnPath (targetStates: ResizeArray<int>) curState =
        if curState = 0
        then None
        elif targetStates.Contains curState
        then Some curState
        else findTargetStateOnPath targetStates primaryParent.[curState]
    
//    let rec isLabeledPathExist src dest labels =
//        if src = dest && labels = []
//        then true
//        else
            
//    let findReductionTarget nonTerm rule =
//        let targets = new ResizeArray<int>()
//        for i in 0 .. grammar.rules.rulesCount do
//            grammar.rules.rightSide i 
//            |> Array.iteri (fun x j -> if x = nonTerm then targets.Add (i, j))
        
    IRIA.AddVertex 0 |> ignore
    statesToItems.Add (0, (grammar.startRule, 0))
    itemsToStates.Add ((grammar.startRule, 0), new ResizeArray<int>([0]))
    IRIA.InitState.Add 0

    while IRIA.VertexCount > dealtWith.Count do
        let curState = IRIA.Vertices 
                       |> Seq.find (fun x -> not (dealtWith.Contains x))
        let curItem = statesToItems.[curState]
        let rule, pos = fst curItem, snd curItem
        if grammar.rules.length rule > pos
        then
            let nextSymbol = grammar.rules.symbol rule pos
            addState !nextState (rule, pos + 1) curState (Smbl(Sh(nextSymbol)))
            if grammar.indexator.isNonTerm nextSymbol 
            then
                if nextSymbol <> grammar.rules.leftSide rule || pos <> 0  //maybe xor
                then
                    for nextSymbolRule in grammar.rules.rulesWithLeftSide nextSymbol do
                        if itemsToStates.ContainsKey (nextSymbolRule, 0)
                        then
                            let targetStates = itemsToStates.[(nextSymbolRule, 0)]
                            match findTargetStateOnPath targetStates curState with
                            | Some x -> IRIA.AddEdge (new EdgeFSA<RCAEdge>(curState, x, Eps)) |> ignore
                            | None -> addState !nextState (nextSymbolRule, 0) curState Eps
                        else addState !nextState (nextSymbolRule, 0) curState Eps
                elif curState <> 0
                then
                    primaryParent.[curState]
                    |> IRIA.OutEdges                    
                    |> Seq.filter (fun x -> x.Tag = Eps)
                    |> Seq.iter 
                           (
                               fun x -> 
                                   IRIA.AddEdge (new EdgeFSA<RCAEdge>(curState, x.Target, Eps))
                                   |> ignore
                           )
        else reductionStates.Add curState |> ignore  
        dealtWith.Add curState |> ignore
//    for state in Seq.toList reductionStates |> List.tail do
//        let nonTerm = fst statesToItems.[state] |> grammar.rules.leftSide                    
    IRIA