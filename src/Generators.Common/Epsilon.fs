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

module Yard.Generators.Common.Epsilon

open Yard.Generators.Common
open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode
open Yard.Core.IL
open Yard.Core.IL.Production

let emptyName = "empty"
let emptyNum (indexator : Indexator) = indexator.nonTermToIndex emptyName

let canInferEpsilon (rules : NumberedRules) (indexator : Indexator) =
    let result : bool[] = Array.zeroCreate indexator.fullCount
    let mutable modified = true
    result.[indexator.errorIndex] <- true
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

let getEpsilonCyclicNonTerms (rules : NumberedRules) (indexator : Indexator) (canInferEpsilon : bool[]) =
    let was : int[] = Array.zeroCreate indexator.fullCount
    let result = ref []
    for i in 0..indexator.nonTermCount-1 do
        if (was.[i] = 0 && canInferEpsilon.[i]) then
            let rec dfs path u =
                was.[u] <- 1
                for rule in rules.rulesWithLeftSide u do
                    let allEpsilon =
                        rules.rightSide rule
                        |> Array.fold (fun res v -> res && canInferEpsilon.[v]) true
                    if allEpsilon then
                        rules.rightSide rule
                        |> Array.iter
                            (fun v ->
                                if was.[v] = 1 then
                                    let cycle =
                                        path |> Seq.takeWhile (fun v' -> v' <> v)
                                        |> List.ofSeq
                                        |> (fun l -> v::l@[v])
                                        |> List.map indexator.indexToNonTerm
                                    result := cycle::!result
                                elif was.[v] = 0 then
                                    dfs (v::path) v
                            )
                was.[u] <- 2
            dfs [i] i
    !result

let epsilonRules (rules : NumberedRules) (canInferEpsilon : bool[]) =
    let result : bool[] = Array.zeroCreate rules.rulesCount
    for i in 0..rules.rulesCount-1 do
        result.[i] <-
            rules.rightSide i
            |> Array.fold (fun res v -> res && canInferEpsilon.[v]) true
    result

let epsilonTrees (rules : NumberedRules) (indexator : Indexator) (canInferEpsilon : bool[]) =
    let allEpsilon = epsilonRules rules canInferEpsilon
    let result : Tree<_> [] = Array.zeroCreate indexator.nonTermCount
    let pos = Array.zeroCreate indexator.nonTermCount
    for u = 0 to indexator.nonTermCount - 1 do
        if canInferEpsilon.[u] then
            let order = new ResizeArray<_>()
            let res = new ResizeArray<_>()
            for j = 0 to indexator.nonTermCount-1 do
                pos.[j] <- -1
            pos.[u] <- 0
            order.Add u
            res.Add (new AST (Unchecked.defaultof<_>, null))
            let mutable i = 0
            while i < order.Count do
                let v = order.[i]
                let children = new ResizeArray<_>()
                for rule in rules.rulesWithLeftSide v do
                    if allEpsilon.[rule] then
                        let nodes =
                            rules.rightSide rule
                            |> Array.map
                                (fun w ->
                                    if pos.[w] = -1 then
                                        pos.[w] <- order.Count
                                        order.Add w
                                        res.Add (new AST (Unchecked.defaultof<_>, null))
                                    res.[pos.[w]] :> AstNode)
                        children.Add <| new Family(rule, new Nodes(nodes))
                if v = indexator.errorIndex 
                then
                    let nodes = Array.zeroCreate 0
                    children.Add <| new Family(rules.rulesCount, new Nodes(nodes))
                let first = children.[0]
                children.RemoveAt 0
                res.[i].first <- first
                res.[i].other <- if children.Count > 0 then children.ToArray() else null
                i <- i + 1
            result.[u] <- new Tree<_>(null, res.[0], null)
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

    