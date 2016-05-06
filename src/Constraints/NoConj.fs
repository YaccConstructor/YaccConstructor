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

module Yard.Core.ConstraintsImpl.NoConj

open Yard.Core
open IL
open Production
open Yard.Core.ConstraintsImpl.Common

let private checker grammar =
    let rec isConj = function
        | PAlt (a, b) -> isConj a || isConj b
        | PConj (_,_) -> true
        | _ -> false
    not <| existsRules (fun r -> isConj r.body) grammar

let noConj = new Constraint("NoConj", checker, Conversions.ExpandConjunction.ExpandConjunction()) 

