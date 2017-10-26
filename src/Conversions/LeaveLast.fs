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

module Yard.Core.Conversions.LeaveLast

open Yard.Core
open Yard.Core.IL


open System.Collections.Generic

let leaveLast (ruleList: Rule<_,_> list) = 
    let findedRules = new HashSet<_>()
    List.rev ruleList |> List.filter (fun rule -> findedRules.Add rule.name)
    

type LeaveLast() = 
    inherit Conversion()
        override this.Name = "LeaveLast"
        override this.ConvertGrammar (grammar,_) = mapGrammar leaveLast grammar
