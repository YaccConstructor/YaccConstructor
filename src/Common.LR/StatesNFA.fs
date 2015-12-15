module Yard.Generators.Common.LR.NFA

open Yard.Generators.Common.EBNF.FinalGrammar
open Yard.Generators.Common.LR.Kernels

type KernelInterpreterNFA =
    inherit KernelInterpreter
    static member inline nextPos (grammar : FinalGrammarNFA) kernel =
        let rule = KernelInterpreter.getProd kernel
        let pos = KernelInterpreter.getPos kernel
        let nextStates = grammar.nextPositions.[rule].[pos]
        nextStates |> Set.map (fun x -> KernelInterpreter.toKernel(rule, x))

    static member inline symbol (grammar : FinalGrammarNFA) kernel =
        let rule = KernelInterpreter.getProd kernel
        let pos = KernelInterpreter.getPos kernel
        if pos = grammar.rules.numberOfStates rule - 1 then grammar.indexator.eofIndex
        else grammar.rules.symbol rule pos

    static member symbolAndLookAheads (grammar : FinalGrammarNFA) (kernel, endLookeheads) =
        let rule = KernelInterpreter.getProd kernel
        let pos = KernelInterpreter.getPos kernel
        if pos = grammar.rules.numberOfStates rule - 1 then
            grammar.indexator.eofIndex, Set.empty
        else
            let lookAheads =
                let nextPositions = grammar.nextPositions.[rule].[pos]
                let hasEpsilonTail = ref false
                let foldFun acc x =
                     if grammar.hasEpsilonTail.[rule].[x] then hasEpsilonTail := true
                     Set.union grammar.followSet.[rule].[x] acc
                nextPositions |> Set.fold foldFun Set.empty
                |> Set.remove grammar.indexator.epsilonIndex
                |> fun x -> if !hasEpsilonTail then Set.union x endLookeheads else x
            grammar.rules.symbol rule pos, lookAheads
