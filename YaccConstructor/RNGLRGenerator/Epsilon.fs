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
open Yard.Generators.RNGLR.AST
open Yard.Core.IL
open Yard.Core.IL.Production

let emptyName = "empty"
let emptyNum (indexator : Indexator) = indexator.nonTermToIndex emptyName

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

let getEpsilonCyclicNonTerms (rules : NumberedRules) (indexator : Indexator) (canInferEpsilon : bool[]) =
    let was : int[] = Array.zeroCreate indexator.fullCount
    let result = ref []
    for i in 0..indexator.nonTermCount-1 do
        if (was.[i] = 0 && canInferEpsilon.[i]) then
            let rec dfs u =
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
    (*
    printfn "%A" allEpsilon
    for i in 0..indexator.nonTermCount-1 do
        printfn "%d %s: %A" i (indexator.indexToNonTerm i) (rules.rulesWithLeftSide i)
    for i in 0..rules.rulesCount-1 do
        printfn "%d: %d -> %A" i (rules.leftSide i) (rules.rightSide i)
    *)
    let result : Tree<_> [] = Array.zeroCreate indexator.nonTermCount
    let pos = Array.zeroCreate indexator.nonTermCount
    for u = 0 to indexator.nonTermCount-1 do
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
                                    box res.[pos.[w]])
                        children.Add <| new Family(rule, nodes)
                let first = children.[0]
                children.RemoveAt 0
                res.[i].first <- first
                res.[i].other <- if children.Count > 0 then children.ToArray() else null
                i <- i + 1
            result.[u] <- new Tree<_>(null, res.[0])
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