//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

namespace Yard.Generators.Common.FinalGrammar

open Yard.Core.IL
open Yard.Generators.Common.Epsilon
open Yard.Generators.Common.SymbolSets
open Yard.Generators.Common

type FinalGrammar (ruleList : Rule.t<Source.t,Source.t> list, caseSensitive) =
    let _indexator = new Indexator(ruleList, caseSensitive)
    let _probability = 
        let getProb body = 
            match body with
            | Production.PSeq (_,_,Some lbl) -> 
                match lbl.weight with 
                | Some w -> w
                | None -> 1.0
            | Production.PSeq (_,_,None) -> 1.0
            | Production.PRef _ -> 1.0
            | x -> 1.0 //failwithf "Incorrect right part of production: %A" x

        ruleList
        |> List.map (fun r -> getProb r.body)
        |> Array.ofList

    let _numberedRules = new NumberedRules(ruleList, _indexator, caseSensitive)
    let _canInferEpsilon = canInferEpsilon _numberedRules _indexator
    let _epsilon_rules = epsilonRules _numberedRules _canInferEpsilon
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
    member this.epsilonRules = _epsilon_rules
    member this.firstSet = _firstSet
    member this.followSet = _followSet
    member this.epsilonTrees = _epsilonTrees
    member this.epsilonTailStart = _epsilonTailStart
    member this.startRule = _numberedRules.startRule
    member this.errorIndex = _errorIndex
    member this.errorRulesExists = _errorRulesExists
    member this.probability = _probability