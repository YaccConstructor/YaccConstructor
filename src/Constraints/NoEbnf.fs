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

module YC.Core.ConstraintsImpl.NoEbnf

open YC.Core
open IL
open YC.Core.ConstraintsImpl.Common

let private isEbnf = function
    | PMany _ | POpt _ | PSome _ -> true
    | _ -> false

let private checker = not << existsProd isEbnf
    
let noEbnf = new Constraint("NoEbnf", checker, Conversions.ExpandEbnfStrict.ExpandEbnf())