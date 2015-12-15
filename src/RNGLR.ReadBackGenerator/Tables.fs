namespace Yard.Generators.RNGLR.ReadBack

open Yard.Generators.Common
open Yard.Generators.Common.LR.Kernels
open Yard.Generators.Common.LR.NFA
open Yard.Generators.RNGLR.ReadBack.States
open Yard.Generators.Common.EBNF.FinalGrammar
open

type TablesReadBack (grammar : FinalGrammarNFA, states : StatesInterpreter) =
    let _reduces, _gotos, _acc =
        let symbolCount = grammar.indexator.fullCount
        let reduces : list<int * RnglrReduceLabel>[,] = Array2D.create states.count symbolCount []
        let gotos : int list[,] = Array2D.create states.count symbolCount []
        let mutable acc = []
        if grammar.canInferEpsilon.[grammar.rules.leftSide grammar.startRule] then acc <- (*startState*)0::acc
        let endRule = KernelInterpreterNFA.toKernel (grammar.startRule, grammar.rules.numberOfStates grammar.startRule - 1)
        for i = 0 to states.count-1 do
            let vertex = states.vertex i
            for e in vertex.outEdges do
                let symbol = e.label
                gotos.[i, symbol] <- e.dest.label::gotos.[i, symbol]
                if gotos.[i, symbol].Length > 1 then
                    eprintfn "Several gotos form state %d on symbol %d: %A" i symbol gotos.[i, symbol]
            let kernels, lookaheads = states.kernels i, states.lookaheads i
            for j = 0 to kernels.Length - 1 do
                let k, la = kernels.[j], lookaheads.[j]
                let prod, pos = KernelInterpreterNFA.unzip k
                if k = endRule then acc <- i::acc
                elif grammar.hasEpsilonTail.[prod].[pos] then
                    for symbol in la do 
                        let reduceLabel =
                            if Set.contains pos grammar.startPositions.[prod] then
                                ZeroReduce
                            else Reduce
                        reduces.[i, symbol] <- (prod, reduceLabel)::reduces.[i, symbol]
        reduces, gotos, acc

    let _nfas =
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
        grammar.rules.rightSideArr |> Array.map
            // first state ([0] is always start)
            (fun x -> x.numberOfStates , (x.stateToVertex |> Array.toList |> statesAndTransitions))

    member this.reduces = _reduces
    member this.gotos = _gotos
    member this.acc = _acc
    member this.nfas = _nfas