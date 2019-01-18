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

module YC.Core.ConstraintsImpl.SingleModule

open YC.Core
open YC.Core.IL

let private checker (grammar : Grammar<_,_>) =
    grammar.Length = 1
    
let singleModule = new Constraint("SingleModule", checker, Conversions.Linearize.Linearize())