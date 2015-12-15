module Yard.Generators.Common.LR.Linear

open Yard.Generators.Common
open Yard.Generators.Common.FinalGrammar
open Yard.Generators.Common.LR.Kernels

type KernelInterpreterLinear =
    inherit KernelInterpreter
    static member inline symbol (grammar : FinalGrammar) kernel =
        let rule = KernelInterpreter.getProd kernel
        let pos = KernelInterpreter.getPos kernel
        if grammar.rules.length rule = pos then grammar.indexator.eofIndex
        else grammar.rules.symbol rule pos

    static member inline symbolAndLookAheads (grammar : FinalGrammar) (kernel, endLookeheads) =
        let rule = KernelInterpreter.getProd kernel
        let pos = KernelInterpreter.getPos kernel
        if grammar.rules.length rule = pos then
            grammar.indexator.eofIndex, Set.empty
        else
            let lookAheads =
                if grammar.epsilonTailStart.[rule] > pos + 1 then grammar.followSet.[rule].[pos]
                else Set.union grammar.followSet.[rule].[pos] endLookeheads
            grammar.rules.symbol rule pos, lookAheads

type StatesInterpreter (stateToVertex : Vertex<int,int>[], stateToKernels : Kernel[][], stateToLookahead : Set<int>[][]) =
    member this.count = stateToVertex.Length
    member this.vertex i = stateToVertex.[i]
    member this.kernels i = stateToKernels.[i]
    member this.lookaheads i = stateToLookahead.[i]

