namespace Yard.Generators.Common.DFA.FinalGrammar

open Yard.Core.IL
open Yard.Generators.Common.EBNF
open Yard.EBNF.DFA.NumberedRules
open Yard.EBNF.DFA.Epsilon
open Yard.EBNF.DFA.SymbolSets
//open Yard.EBNF.DFA.StateSets

type FinalGrammarDFA(ruleList : Rule.t<Source.t,Source.t> list, caseSensitive) =
    let _indexator = new IndexatorEBNF(ruleList, caseSensitive)
    let _dfaRules = new NumberedRulesDFA(ruleList, _indexator, caseSensitive)
    let _canInferEpsilon,_hasEpsilonTail = epsilonsDFA _dfaRules _indexator
    let _firstSet = firstSetDFA _dfaRules _indexator _canInferEpsilon //
    let _followSet = followSetDFA _dfaRules _indexator _canInferEpsilon _firstSet
    let _errorIndex = _indexator.errorIndex // номер для терминала-еррора

    member this.indexator = _indexator
    member this.rules = _dfaRules
    //member this.EpsilonCyclicNonTerms = _epsilonCyclicNonTerms
    member this.canInferEpsilon = _canInferEpsilon
    member this.hasEpsilonTail = _hasEpsilonTail
    member this.firstSet = _firstSet
    member this.followSet = _followSet
    //member this.epsilonTrees = _epsilonTrees
    //member this.epsilonTailStart = _epsilonTailStart
    member this.startRule = _dfaRules.startRule
    member this.errorIndex = _errorIndex
    //member this.errorRulesExists = _errorRulesExists