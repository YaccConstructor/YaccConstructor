//  InitialConvert.fs contains methods, which must be applied to grammar
//    to transform this to appliable for RNGLR form.
//
//  Copyright 2011-2012 Avdyukhin Dmitry
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Yard.Generators.RNGLR.InitialConvert

open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Core.Convertions.TransformAux

let initialConvert (def : Definition.t<_,_>) =
    let addStartRule (ruleList : Rule.t<_,_> list) =
        let wasStart = ref false
        ruleList
        |> List.fold
            (fun res rule ->
                if not rule._public then rule::res
                else
                    if !wasStart then failwith "More than one start rule"
                    wasStart := true
                    let startRule : Rule.t<_,_> =
                        {_public = true; name = new Source.t("yard_start_rule", rule.name); args = rule.args;
                         metaArgs = []; body = PRef(createSource rule.name.text, rule.args |> createParams |> list2opt)}
                    startRule::{rule with _public = false}::res
            )
            []
        |> (fun x -> if not !wasStart then failwith "No startRule was found"
                     x)

    let splitAlters ruleList =
        let rec splitRule (curRule : Rule.t<_,_>) res = function
            | PAlt (l, r) ->
                let leftRes = splitRule curRule res l
                splitRule curRule leftRes r
            |  x -> {curRule with body = x}::res
        List.fold (fun res rule -> splitRule rule res rule.body) [] ruleList

    let filterNonReachable ruleList =
        let count = new Dictionary<_,_>()
        let inline getCount str = 
            if not <| count.ContainsKey str then 0
            else count.[str]
        ruleList
        |> List.iter
            (fun (rule : Rule.t<_,_>) ->
                let str = rule.name.text
                count.[str] <- getCount str + 1)
        let rec reachable =
            function
            | PToken _ | PLiteral _ -> true
            | PRef (n, _) -> getCount <| Source.toString n > 0
            | PSeq (s,_) -> s |> List.forall (fun elem -> reachable elem.rule)
            | x -> failwithf "Unexpected construction %A" x
        let rec inner (ruleList : Rule.t<_,_> list) =
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
    {def with grammar = def.grammar |> addStartRule |> splitAlters |> filterNonReachable}