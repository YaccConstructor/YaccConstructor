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

module Yard.Core.ConstraintsImpl.InCNF

open Yard.Core
open IL
open Production
open Yard.Core.ConstraintsImpl.Common

let private checker (grammar : Grammar.t<_,_>) =
    if grammar.Length > 1 then false
    else
        let notStart = 
            match grammar.Head.rules |> List.tryPick (fun r -> if r.isStart then Some r.name.text else None) with
            | None -> fun _ -> true
            | Some start -> fun (x : Source.t) -> x.text <> start
        existsRules (fun r ->
            match r.body with
            | PSeq (elems, _, _) ->
                match elems |> List.map (fun e -> e.rule) with
                | [] -> not r.isStart
                | [PToken _] -> false
                | [PRef (a,_); PRef (b,_)] when notStart a && notStart b -> false
                | _ -> true
            | _ -> true
        ) grammar |> not
    
let inCNF = new Constraint("InCNF", checker, Conversions.CNFandBNF.CNF())