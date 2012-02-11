//  Epsilon.fs contains methods to work with rules, from what epsilon can be dirived.
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

module Yard.Generators.RNGLR.Epsilon

open Yard.Generators.RNGLR
open Yard.Core.IL
open Yard.Core.IL.Production

let emptyName = "empty"
let emptyNum (indexator : Indexator) = indexator.nonTermToIndex emptyName

let eliminateEpsilon (ruleList : Rule.t<_,_> list) =
    let rec eliminateProdEpsilon (*body*) = function
        | PSeq (s,a) ->
            s
            |> List.fold
                (fun acc e ->
                    match e.rule with
                    | PRef (x,_) when (fst x = emptyName) -> acc
                    | PSeq _ as s1 -> {e with rule = eliminateProdEpsilon s1}::acc
                    | _ -> e::acc
                ) []
            |> List.rev
            |> (fun x -> PSeq(s,a))
        | x -> x

    ruleList
    |> List.map (fun x -> {x with body = eliminateProdEpsilon x.body})

let canInferEpsilon (rules : NumberedRules) (indexator : Indexator) =
    let result : bool[] = Array.zeroCreate indexator.fullCount
    let mutable modified = true
    while modified do
        modified <- false
        for i in 0..rules.rulesCount-1 do
            if not result.[rules.leftSide i] then
                let rec checkEpsilon j =
                    if j < 0 then true
                    else
                        let value = rules.symbol i j
                        if not result.[value] then false
                        else checkEpsilon <| j-1
                if checkEpsilon (rules.length i - 1) then
                    modified <- true
                    result.[rules.leftSide i] <- true

    result

let getEpsilonCycles (rules : NumberedRules) (indexator : Indexator) (canInferEpsilon : bool[]) =
    let was : int[] = Array.zeroCreate indexator.fullCount
    let result = ref []
    for i in 0..indexator.nonTermCount-1 do
        if (was.[i] = 0 && canInferEpsilon.[i]) then
            let rec dfs u =
                was.[u] <- 1
                for i in 0..rules.rulesCount-1 do
                    if (rules.leftSide i = u) then
                        let allEpsilon =
                            rules.rightSide i
                            |> Array.fold (fun res v -> res && canInferEpsilon.[v]) true
                        if allEpsilon then
                            rules.rightSide i
                            |> Array.iter
                                (fun v ->
                                    if was.[v] = 1 then
                                        result := (indexator.indexToNonTerm v)::!result
                                    elif was.[v] = 0 then
                                        dfs v
                                )
                was.[u] <- 2
            dfs i
    !result

let epsilonRules (rules : NumberedRules) (indexator : Indexator) (canInferEpsilon : bool[]) =
    let result : bool[] = Array.zeroCreate rules.rulesCount
    for i in 0..rules.rulesCount-1 do
        result.[i] <-
            rules.rightSide i
            |> Array.fold (fun res v -> res && canInferEpsilon.[v]) true
    result

let epsilonTrees (rules : NumberedRules) (indexator : Indexator) (canInferEpsilon : bool[]) =
    let allEpsilon = epsilonRules rules indexator canInferEpsilon
    let result : MultiAST[] = Array.zeroCreate indexator.nonTermCount
    let was : int[] = Array.zeroCreate indexator.nonTermCount
    for i in 0..indexator.nonTermCount-1 do
(*        let rec collectAsts (production : int[]) acc num =
            if num = production.Length then acc
            else
                let newAcc =
                    [for child in result.[production.[num]] do
                        for prev in acc do
                            yield child::prev]
                collectAsts production newAcc (num + 1)*)
        if (was.[i] = 0 && canInferEpsilon.[i]) then
            let rec dfs u =
                was.[u] <- 1
                for i in 0..rules.rulesCount-1 do
                    if (rules.leftSide i = u && allEpsilon.[i]) then
                        rules.rightSide i
                        |> Array.iter (fun v -> if was.[v] = 0 then dfs v)
                        let asts =
                            rules.rightSide i
                            |> Array.map (fun num -> result.[num])
                        result.[u] <- {ruleNumber = i; children = asts}::result.[u]
                was.[u] <- 2
            dfs i
    result

let epsilonTailStart (rules : NumberedRules) (canInferEpsilon : bool[]) =
    let result : int[] = Array.zeroCreate rules.rulesCount
    for i = 0 to rules.rulesCount-1 do
        let rec inner pos =
            if pos < 0 then 0
            elif not canInferEpsilon.[rules.symbol i pos] then pos+1
            else inner <| pos-1
        result.[i] <- inner <| rules.length i - 1
    result