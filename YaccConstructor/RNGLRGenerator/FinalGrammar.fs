//  SymbolSets.fs contains description of finally transformed grammar with all information
//
//  Copyright 2011-2012 Avdyukhin Dmitry
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace Yard.Generators.RNGLR.FinalGrammar

open Yard.Core.IL
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.Epsilon
open Yard.Generators.RNGLR.SymbolSets
//open Yard.Generators.RNGLR.Error


type FinalGrammar (ruleList : Rule.t<Source.t,Source.t> list) =
    let _indexator = new Indexator(ruleList)
    let _numberedRules = new NumberedRules(ruleList, _indexator)
    let _canInferEpsilon = canInferEpsilon _numberedRules _indexator
    let _firstSet = firstSet _numberedRules _indexator _canInferEpsilon
    let _followSet = followSet _numberedRules _indexator _canInferEpsilon _firstSet
    let _epsilonCyclicNonTerms = getEpsilonCyclicNonTerms _numberedRules _indexator _canInferEpsilon
    let _epsilonTrees = epsilonTrees _numberedRules _indexator _canInferEpsilon
    let _epsilonTailStart = epsilonTailStart _numberedRules _canInferEpsilon
    let _errorIndex = _indexator.errorIndex
    let _errorRulesExists = _numberedRules.errorRulesExists

    member this.indexator = _indexator
    member this.rules = _numberedRules
    member this.EpsilonCyclicNonTerms = _epsilonCyclicNonTerms
    member this.canInferEpsilon = _canInferEpsilon
    member this.firstSet = _firstSet
    member this.followSet = _followSet
    member this.epsilonTrees = _epsilonTrees
    member this.epsilonTailStart = _epsilonTailStart
    member this.startRule = _numberedRules.startRule
    member this.errorIndex = _errorIndex
    member this.errorRulesExists = _errorRulesExists