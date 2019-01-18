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

module YC.Core.ConstraintsImpl.NoAlt

open YC.Core
open IL
open YC.Core.ConstraintsImpl.Common

let private checker grammar =
    let rec isAlt = function
        | PAlt (_,_) -> true
        | _ -> false
    not <| existsRules (fun r -> isAlt r.body) grammar

let noAlt = new Constraint("NoAlt", checker, Conversions.ExpandTopLevelAlt.ExpandTopLevelAlt())