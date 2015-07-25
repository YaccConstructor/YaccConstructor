module Yard.EBNF.DFA.PrintFA

open Yard.EBNF.DFA.GrammarWithNFARightSide
open Yard.EBNF.DFA.Indexator
open Yard.EBNF.DFA.NumberedRules
open Yard.EBNF.DFA.ReverseNumberedRules
open Yard.EBNF.DFA.FinalGrammar
open Yard.Generators.Common
open System.Collections.Generic

let printReverseEBNF (rules : ReverseNumberdRulesEBNF) rule = 
    let printTableNFA (table : HashSet<_>[][]) =
        for i in 0..table.Length - 1 do
            for j in 0.. table.[i].Length - 1 do
                printf "(%A, %A) ->  " i j
                for element in table.[i].[j] do
                    printf "%A " element
                printfn ""
    printfn""
    let tableForRule = rules.getStateTable rule
    printTableNFA tableForRule

let printReverseDFA (rules : ReverseNumberdRulesDFA) rule = 
    let printTableDFA (table : int[][]) = 
        for i in 0..table.Length - 1 do
            for j in 0.. table.[i].Length - 1 do
                printfn "(%A, %A) -> %A" i j table.[i].[j]
    printfn ""
    let table = rules.dfaTable rule
    let tableForRule = fst table
    printTableDFA tableForRule

    let finiteStates = snd table
    printf "Finite states: "
    for state in finiteStates do
        printf "%A " state
    printfn ""

let printAllNFA (rules : NumberedRulesEBNF) (indexator : IndexatorEBNF) = 
    printfn "NFA"
    for i in 0..rules.rulesCount - 1 do
        printfn "--------------"
        let table = rules.table i
        for i in 0..table.Length - 1 do
            for j in 0.. table.[i].Length - 1 do
                printf "(%A, %A) ->  " i j
                for element in table.[i].[j] do
                    printf "%A " element
                printfn ""
        printfn""
        printfn "--------------"

let printAllReverseNFA (rules : NumberedRulesEBNF) (indexator : IndexatorEBNF) = 
    let reverseNumberedRules = ReverseNumberdRulesEBNF(rules, indexator)
    printfn "ReverseNFA"
    for i in 0..reverseNumberedRules.rulesCount - 1 do
        printfn "--------------"
        printReverseEBNF reverseNumberedRules i
        printfn "--------------"

let printAllDFA (rules : ReverseNumberdRulesDFA) (indexator : IndexatorEBNF) = 
    printfn "DFA"
    for i in 0..rules.rulesCount - 1 do
        printfn "--------------"
        printReverseDFA rules i
        printfn "--------------"

let printAll (grammar : FinalGrammarNFA) = 
    let rules = grammar.rules
    let reverseRules = grammar.reverseRules
    let indexator = grammar.indexator
    printfn "eps = %A  eof = %A" indexator.epsilonIndex indexator.eofIndex
    printAllNFA rules indexator
    printAllReverseNFA rules indexator
    printAllDFA reverseRules indexator