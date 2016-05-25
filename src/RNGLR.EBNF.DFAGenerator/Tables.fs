namespace Yard.Generators.RNGLR.EBNF.DFA

open Yard.Generators.Common
open Yard.Generators.Common.LR.DFA
open Yard.Generators.RNGLR.EBNF.DFA.States
open Yard.Generators.Common.DFA.FinalGrammar

type DFATable = (int * (int * (int * int) list) list * int list)[]

type TablesReadBack (grammar : FinalGrammarDFA, states : StatesInterpreter) =
    let _reduces, _zeroReduces, _gotos, _acc =
        let symbolCount = grammar.indexator.fullCount
        let reduces : list<int * int>[,] = Array2D.create states.count symbolCount []
        let zeroReduces : list<int>[,] = Array2D.create states.count symbolCount []
        let gotos : int list[,] = Array2D.create states.count symbolCount []
        let mutable acc = []
        if grammar.canInferEpsilon.[grammar.rules.leftSide grammar.startRule] then acc <- (*startState*)0::acc
        let endRule = 
            KernelInterpreterDFA.toKernel 
                (grammar.startRule, (let _,x,_ = grammar.rules.rightRevertedArr.[grammar.startRule] in x))
        for i = 0 to states.count-1 do
            let vertex = states.vertex i
            let mainKernels, mainLookaheads = states.mainKernels i, states.mainLookaheads i
            let derivedKernels, derivedLookaheads = states.derivedKernels i, states.derivedLookaheads i

            for e in vertex.outEdges do
                let symbol = e.label
                gotos.[i, symbol] <- e.dest.label::gotos.[i, symbol]
                if gotos.[i, symbol].Length > 1 then
                    eprintfn "Several gotos form state %d on symbol %d: %A" i symbol gotos.[i, symbol]

            for j = 0 to mainKernels.Length - 1 do
                let k, la = mainKernels.[j], mainLookaheads.[j]
                let prod, pos = KernelInterpreterDFA.unzip k
                if k = endRule then acc <- i::acc
                elif grammar.hasEpsilonTail.[prod].[pos] then
                    for symbol in la do 
                        reduces.[i, symbol] <- (prod, pos)::reduces.[i, symbol]
            
            for j = 0 to derivedKernels.Length - 1 do
                let k, la = derivedKernels.[j], derivedLookaheads.[j]
                let prod, pos = KernelInterpreterDFA.unzip k
                if k = endRule then acc <- i::acc
                elif grammar.hasEpsilonTail.[prod].[pos] then
                    for symbol in la do 
                        zeroReduces.[i, symbol] <- prod::zeroReduces.[i, symbol]
        reduces, zeroReduces, gotos, acc

    let _dfas =
        let rec statesAndTransitions = function
            | (vertex : Vertex<_,_>) :: vs ->
                let rec transitions = function
                    | (edge : Edge<_,_>) :: es ->
                        (edge.dest.label, edge.label) :: transitions es
                    | [] -> []
                if vertex.outEdges.Count > 0 then
                    (vertex.label, (vertex.outEdges |> List.ofSeq |> transitions)) :: statesAndTransitions vs
                else
                    statesAndTransitions vs
            | [] -> [] 
        grammar.rules.rightRevertedArr 
        |> Array.map
            // first state ([0] is always start)
            (fun (stateToVertex, startState, finishStates) -> stateToVertex.Length, (stateToVertex |> Array.toList |> statesAndTransitions), Set.toList finishStates)

    member this.reduces = _reduces
    member this.zeroReduces = _zeroReduces
    member this.gotos = _gotos
    member this.acc = _acc
    member this.dfas = _dfas