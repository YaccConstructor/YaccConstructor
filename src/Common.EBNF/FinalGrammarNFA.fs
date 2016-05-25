namespace Yard.Generators.Common.EBNF.FinalGrammar

open Yard.Core.IL
open Yard.Generators.Common.EBNF
open Yard.Generators.Common.EBNF.Epsilon
open Yard.Generators.Common.EBNF.SymbolSets
open Yard.Generators.Common.EBNF.StateSets

type FinalGrammarNFA(ruleList : Rule.t<Source.t,Source.t> list, caseSensitive, needTranslate, translateToAst) =
    let _indexator = new IndexatorEBNF(ruleList, caseSensitive)
    let _nfaRules = new NumberedRulesEBNF (ruleList, _indexator, caseSensitive, needTranslate, translateToAst)
    let _canInferEpsilon = canInferEpsilonNFA _nfaRules _indexator
    let _hasEpsilonTail = hasEpsilonTail _nfaRules _canInferEpsilon
    let _firstSet = firstSetNFA _nfaRules _indexator _canInferEpsilon //
    let _followSet = followSetNFA _nfaRules _indexator _canInferEpsilon _firstSet
    let _epsilonReachable = epsilonReachable _nfaRules _indexator
    let _usefulStates = usefulStates _nfaRules _indexator
    let _startPositions = startPositions _nfaRules _epsilonReachable _usefulStates
    let _nextPositions = nextPositions _nfaRules _indexator _epsilonReachable _usefulStates
    //let _epsilonCyclicNonTerms = getEpsilonCyclicNonTerms _numberedRules _indexator _canInferEpsilon // нетермиалы
    //let _epsilonTrees = epsilonTrees _numberedRules _indexator _canInferEpsilon // написать Дмитрию Авдюхину. 
    //let _epsilonTailStart = epsilonTailStart _numberedRules _canInferEpsilon
    let _errorIndex = _indexator.errorIndex // номер для терминала-еррора
    //let _errorRulesExists = _numberedRules.errorRulesExists // проверка на правила с ошибками (error recovery)

    member this.indexator = _indexator
    member this.rules = _nfaRules
    //member this.EpsilonCyclicNonTerms = _epsilonCyclicNonTerms
    member this.canInferEpsilon = _canInferEpsilon
    member this.hasEpsilonTail = _hasEpsilonTail
    member this.firstSet = _firstSet
    member this.followSet = _followSet
    member this.epsilonReachable = _epsilonReachable
    member this.usefulStates = _usefulStates
    member this.startPositions = _startPositions
    member this.nextPositions = _nextPositions
    //member this.epsilonTrees = _epsilonTrees
    //member this.epsilonTailStart = _epsilonTailStart
    member this.startRule = _nfaRules.startRule
    member this.errorIndex = _errorIndex
    //member this.errorRulesExists = _errorRulesExists