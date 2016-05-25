module Yard.Generators.Common.LR.DFA

open Yard.Generators.Common.DFA.FinalGrammar
open Yard.Generators.Common.LR.Kernels

type KernelInterpreterDFA =
    inherit KernelInterpreter
    (*static member inline nextPos (grammar : FinalGrammarDFA) kernel =
        let rule = KernelInterpreter.getProd kernel
        let pos = KernelInterpreter.getPos kernel
        let nextStates = grammar.nextPositions.[rule].[pos]
        nextStates |> Set.map (fun x -> KernelInterpreter.toKernel(rule, x))*)

    static member inline symbolsAndNextPos (grammar : FinalGrammarDFA) kernel =
        let rule = KernelInterpreter.getProd kernel
        let pos = KernelInterpreter.getPos kernel
        grammar.rules.symbolsAndNextPos rule pos

    static member symbolsAndLookaheads (grammar : FinalGrammarDFA) (kernel, endLookeheads) =
        let rule = KernelInterpreter.getProd kernel
        let pos = KernelInterpreter.getPos kernel
        
        grammar.rules.symbolsAndNextPos rule pos
        |> Array.map 
            (fun (symbol, nextPos) ->
                let follow = grammar.followSet.[rule].[nextPos]
                let lookaheads = 
                    if grammar.hasEpsilonTail.[rule].[nextPos] then 
                        Set.union follow endLookeheads
                    else
                        follow
                (symbol, lookaheads)
            )
