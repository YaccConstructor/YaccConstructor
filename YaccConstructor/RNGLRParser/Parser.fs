//  Parser.fs contains methods, needed to build an AST
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

module Yard.Generators.RNGLR.Parser

open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open System.Collections.Generic

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType>
    | Error of int * string

let buildAst<'TokenType when 'TokenType:equality> (parserSource : ParserSource<'TokenType>) (tokens : seq<'TokenType>) =
    let tokensArr = tokens |> Array.ofSeq
    let tokenNums =
        tokens
        |> Seq.map parserSource.TokenToNumber
        |> Seq.takeWhile (fun t -> t <> parserSource.EofIndex)
        |> (fun s -> seq {yield! s; yield parserSource.EofIndex})
        |> Seq.toArray
    let tokensCount = tokenNums.Length - 1
    let startState = 0
    let startRule = parserSource.LeftSide.[parserSource.StartRule]
    if tokensCount = 0 then
        if parserSource.AccStates.[startState] then
            let res = Array.create 1 <| Epsilon (startRule)
            new Tree<_>(res, 0) |> Success
        else
            Error (0, "This grammar cannot accept empty string")
    else
        /// Here will be collected all nodes in AST
        let nodes = new ResizeArray<_>()
        // Must be number of non-terminals, but doesn't matter
        let nonTermsCountLimit = max (Array.max parserSource.Rules) (Array.max parserSource.LeftSide)
        let epsilons = Array.zeroCreate nonTermsCountLimit
        for i = 0 to nonTermsCountLimit-1 do
            epsilons.[i] <- nodes.Count
            nodes.Add <| Epsilon i
            
        let reductions = new Queue<_>(10)
        let astNodes = new Dictionary<_,_>(10, HashIdentity.Structural)
        let statesCount = parserSource.Gotos.Length
        let pushes = Array.zeroCreate (statesCount * 2 + 10)
        let pBeg, pEnd = ref 0, ref 0
        let inline nextInd n =
            if !n + 1 <> pushes.Length then n := !n + 1
            else n := 0
        let usedStates : int[] = Array.zeroCreate statesCount
        let curLevelCount = ref 0
        let stateToVirtex : Virtex<_,_>[] = Array.zeroCreate statesCount

        let inline addVirtex state num (edgeOpt : option<Edge<_,int>>) =
            let dict = stateToVirtex
            let mutable v = Unchecked.defaultof<Virtex<_,_>>
            if dict.[state] = null then
                //printfn "v(%d,%d)" state num
                v <- new Virtex<_,_>(state, num)
                dict.[state] <- v
                if num < tokensCount && parserSource.Gotos.[state].[tokenNums.[num]].IsSome then
                    pushes.[!pBeg] <- (v, parserSource.Gotos.[state].[tokenNums.[num]].Value)
                    nextInd pBeg
                for prod in parserSource.ZeroReduces.[state].[tokenNums.[num]] do
                    reductions.Enqueue (v, prod, 0, None) |> ignore
                usedStates.[!curLevelCount] <- state
                incr curLevelCount
            else v <- dict.[state]
            if edgeOpt.IsSome then 
                for (prod, pos) in parserSource.Reduces.[state].[tokenNums.[num]] do
                    //printf "%A %A %d %d\n" v.label v.outEdges prod pos
                    reductions.Enqueue (v, prod, pos, edgeOpt)
            v

        // init, by adding the first virtex in the first set
        ignore <| addVirtex startState 0 None

        let makeReductions num =
            while reductions.Count > 0 do
                let virtex, prod, pos, edgeOpt = reductions.Dequeue()
                let nonTerm = parserSource.LeftSide.[prod]
                let compareChildren (ast1 : int[]) (ast2 : int[]) =
                    let n = ast1.Length
                    if ast2.Length <> n then false
                    else
                        Array.forall2 (=) ast1 ast2

                //let inline addEpsilon node nonTerm = (getEpsilon node) := nonTerm

                let inline addChildren node (path : int[]) prod =
                    let family = getFamily node
                    let astExists = 
                        family.Value
                        |> List.exists
                            (function (number,children) -> number = prod && compareChildren children path)
                    if not astExists then
                        //printf "Children: %A\n" path
                        family := (prod, path)::family.Value
                    //else
                    //    printf "Rejected: %A\n" path
                let handlePath (path : int list) (final : Virtex<_,_>) =
                    let ast = ref -1
                    let state = parserSource.Gotos.[fst final.label].[nonTerm].Value
                    //printf "r %A->%A->%A (prod: %d,%d)\n" virtex.label final.label (state,num) prod pos
                    let newVirtex = addVirtex state num None
                    //printf "nv: %A:\n" newVirtex.label
                    //for e in newVirtex.outEdges do
                    //    printf "el1: %A %A\n" e.label e.dest.label
                    if not <| newVirtex.outEdges.Exists (fun e -> if not (e.dest.label = final.label) then false
                                                                  else ast := e.label
                                                                       true)
                    then
                        ast := nodes.Count
                        let edge = new Edge<int*int, int>(final, !ast)
                        nodes.Add <| NonTerm (ref [])
                        newVirtex.addEdge edge
                        if (pos > 0) then
                            for (prod, pos) in parserSource.Reduces.[state].[tokenNums.[num]] do
                                //printf "%A %A %d %d\n" newVirtex.label newVirtex.outEdges prod pos
                                reductions.Enqueue (newVirtex, prod, pos, Some edge)
                    if path = [] then nodes.[!ast] <- Epsilon parserSource.LeftSide.[prod]
                    else addChildren nodes.[!ast] (path |> List.toArray) prod
                    //for e in newVirtex.outEdges do
                    //    printf "el2: %A %A\n" e.label e.dest.label

                let rec walk length (virtex : Virtex<_,_>) path =
                    if length = 0 then handlePath path virtex
                    else
                        virtex.outEdges |> ResizeArray.iter
                            (fun e -> walk (length - 1) e.dest (e.label::path))
                
                if pos = 0 then
                    handlePath [] virtex
                else 
                    let epsilonPart =
                        let mutable res = []
                        for i = parserSource.Length.[prod] - 1 downto pos do
                            res <- (epsilons.[parserSource.Rules.[parserSource.RulesStart.[prod] + i]]) ::res
                        res
                    //printf "pl: %A\n" edgeOpt.Value.label
                    walk (pos - 1) (edgeOpt.Value : Edge<_,_>).dest (edgeOpt.Value.label::epsilonPart)

        let shift num =
            if num <> tokensCount then
                let newAstNode = Term tokensArr.[num]
                for i = 0 to !curLevelCount-1 do
                    stateToVirtex.[usedStates.[i]] <- null
                curLevelCount := 0
                let curBeg = !pBeg
                while curBeg <> !pEnd do
                    let virtex, state = pushes.[!pEnd]
                    let edge = new Edge<_,_>(virtex, nodes.Count)
                    nodes.Add newAstNode
                    //printf "p %A\n" (virtex.label, state)
                    let newVirtex = addVirtex state (num + 1) (Some edge)
                    newVirtex.addEdge edge
                    nextInd pEnd
        let mutable errorIndex = -1
        for i = 0 to tokensCount do
            if errorIndex = -1 then
                if !curLevelCount = 0 then
                    errorIndex <- i
                else
                    astNodes.Clear()
                    let symbol = tokenNums.[i]
                    makeReductions i
                    shift i
                    ()
        if errorIndex <> -1 then Error (errorIndex - 1, "Parse error")
        else
            let root = ref None
            printfn "accs: %A" [for i = 0 to parserSource.AccStates.Length-1 do
                                    if parserSource.AccStates.[i] then yield i]
            let addTreeTop res = NonTerm (ref [parserSource.StartRule, [|res|]])
            for i = 0 to !curLevelCount-1 do
                printf "%d " usedStates.[i]
                if parserSource.AccStates.[usedStates.[i]] then
                    root := Some nodes.Count
                    stateToVirtex.[usedStates.[i]].outEdges.[0].label
                    |> addTreeTop
                    |> nodes.Add
            printfn ""
            match !root with
            | None -> Error (tokensCount, "There is no accepting state")
            | Some res -> Success <| new Tree<_>(nodes.ToArray(), res)
