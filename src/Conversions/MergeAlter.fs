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

module Yard.Core.Conversions.MergeAlter

open Yard.Core
open Yard.Core.IL


open System.Collections.Generic

let mergeAlter (ruleList: Rule.t<Source.t, Source.t> list) = 
    let buildAlt (ruleList:Rule.t<Source.t, Source.t> list) = 
        //TODO add check that all merged rules contains same attributes and metaattributes
        let rec buildProduction productionList =
            match productionList with
            | [production] -> production
            | production::t -> Production.PAlt(production,  buildProduction t)
            | [] -> failwith "buildProduction: expected one or more elements"
        match ruleList with
        | rule::t -> { rule with body = ruleList |> List.map (fun ruleInner -> ruleInner.body) |> buildProduction }
        | [] -> failwith "buildAlt: expected one or more elements"
    let findedRules = new HashSet<string>()
    ruleList |>
        List.choose 
            (fun rule ->
                if findedRules.Contains rule.name.text then
                    None
                else
                    findedRules.Add rule.name.text |> ignore
                    Some <| buildAlt (ruleList |> List.filter (fun ruleInner -> rule.name = ruleInner.name))
            )
    



do()


type MergeAlter() = 
    inherit Conversion()
        override this.Name = "MergeAlter"
        override this.ConvertGrammar (grammar,_) = mapGrammar mergeAlter grammar
