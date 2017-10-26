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

//  InitialConvert.fs contains methods, which must be applied to grammar
//    to transform it to appliable for RNGLR or GLL form.
module Yard.Generators.Common.InitialConvert

open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Core.Conversions.TransformAux

let convertRules (ruleList : Rule<_,_> list) =
    let addStartRule (ruleList : Rule<_,_> list) =
        let wasStart = ref false
        ruleList
        |> List.fold
            (fun res rule->
                if not rule.isStart then rule::res
                else
                    if !wasStart then failwith "More than one start rule"
                    wasStart := true
                    let startRule : Rule<_,_> =
                        {
                            isStart = true
                            name = new Source.t("yard_start_rule", rule.name)
                            args = rule.args
                            metaArgs = []
                            isPublic=false
                            isInline = false
                            body = PRef(rule.name, rule.args |> createParams |> list2opt)
                        }
                    startRule::{rule with isStart = false}::res
            )
            []
        |> (fun x -> if not !wasStart then failwith "No startRule was found"
                     x)

    let splitAlters ruleList =
        let rec splitRule (curRule : Rule<_,_>) res = function
            | PAlt (l, r) ->
                let rightRes = splitRule curRule res r
                splitRule curRule rightRes l
            |  x -> {curRule with body = x}::res
        List.fold (fun res rule -> splitRule rule res rule.body) [] ruleList

    let filterNonReachable ruleList =
        let count = new Dictionary<_,_>()
        let inline getCount str = 
            if str = "error" then 1
            elif not <| count.ContainsKey str then 0
            else count.[str]
        ruleList
        |> List.iter
            (fun (rule : Rule<_,_>) ->
                let str = rule.name.text
                count.[str] <- getCount str + 1)
        let rec reachable =
            function
            | PToken _ | PLiteral _ -> true
            | PRef (n, _) -> getCount <| Source.toString n > 0
            | PSeq (s,_,_) -> s |> List.forall (fun elem -> reachable elem.rule)
            | PAlt (x,y) -> reachable x && reachable y
            | x -> failwithf "Unexpected construction %A" x
        let rec inner (ruleList : Rule<_,_> list) =
            let iter = ref false
            let res = 
                ruleList
                |> List.filter
                    (fun rule ->
                        if reachable rule.body then true
                        else
                            iter := true
                            count.[rule.name.text] <- count.[rule.name.text] - 1
                            false)
            if not !iter then res
            else inner res
        inner ruleList
    ruleList |> addStartRule |> splitAlters |> filterNonReachable

let initialConvert (def : Definition<_,_>) =
    if def.grammar.Length > 1 then
        failwith "More than one module. Use 'Linearize' conversion"
    let rules = def.grammar.Head.rules |> convertRules
    {def with grammar = [{def.grammar.Head with rules=rules}]}