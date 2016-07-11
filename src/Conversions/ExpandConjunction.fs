//   Copyright 2016 YaccConstructor Software Foundation
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
module Yard.Core.Conversions.ExpandConjunction

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Namer
open Mono.Addins
open TransformAux

let rec extractOneRule (rule:Rule.t<'a,'b>) = 
    //let name = ref None
    let rec expandBody attrs = function
    | PAlt (a,b)  -> let leftBody, leftRules = expandBody attrs a
                     let rightBody, rightRules = expandBody attrs b
                     PAlt(leftBody, rightBody), leftRules @ rightRules
    | PConj (a,b) -> let name = Namer.newName Namer.Names.conjunction
                     PRef(genNewSourceWithRange name a, list2opt <| createParams attrs),
                     (extractOneRule {name=genNewSourceWithRange name a; args=attrs;
                                             body=PAlt(a, b); isStart=false; isPublic=false; metaArgs=[]})
    | a   -> a, []
    let body, rules = expandBody rule.args rule.body
    {rule with body=body}::rules
[<Extension>]
type ExpandConjunction() = 
    inherit Conversion()
        override this.Name = "ExpandConjunction"
        override this.ConvertGrammar (grammar,_) = mapGrammar (List.collect extractOneRule) grammar