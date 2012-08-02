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
open Microsoft.FSharp.Text.Lexing

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType>
    | Error of int * 'TokenType * string

let buildAst<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : seq<'TokenType>) =
    let enum = tokens.GetEnumerator()
    let startState = 0
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let inline getEpsilon i = -1-i
    if not <| enum.MoveNext() || parserSource.EofIndex = parserSource.TokenToNumber enum.Current then
        if parserSource.AcceptEmptyInput then
            new Tree<_>([||], getEpsilon startNonTerm) |> Success
        else
            Error (0, Unchecked.defaultof<'TokenType>, "This grammar cannot accept empty string")
    else
        let curToken = ref enum.Current
        let curNum = ref (parserSource.TokenToNumber enum.Current)
        /// Here all nodes in AST will be collected
        let nodes = new ResizeArray<_>()
        // Must be number of non-terminals, but doesn't matter
        let nonTermsCountLimit = max (Array.max parserSource.Rules) (Array.max parserSource.LeftSide)
            
        let reductions = new Queue<_>(10)
        let statesCount = parserSource.Gotos.Length
        let pushes = Array.zeroCreate (statesCount * 2 + 10)
        let pBeg, pEnd = ref 0, ref 0
        let inline nextInd n =
            if !n + 1 <> pushes.Length then n := !n + 1
            else n := 0
        let usedStates : int[] = Array.zeroCreate statesCount
        let curLevelCount = ref 0
        let stateToVertex : Vertex<_,_>[] = Array.zeroCreate statesCount

        let inline addVertex state num (edgeOpt : option<Edge<_,int>>) =
            let dict = stateToVertex
            let mutable v = null
            if dict.[state] = null then
                //printfn "v(%d,%d)" state num
                v <- new Vertex<_,_>(state, num)
                dict.[state] <- v
                if parserSource.Gotos.[state].[!curNum].IsSome then
                    pushes.[!pBeg] <- (v, parserSource.Gotos.[state].[!curNum].Value)
                    nextInd pBeg
                for prod in parserSource.ZeroReduces.[state].[!curNum] do
                    reductions.Enqueue (v, prod, 0, None) |> ignore
                usedStates.[!curLevelCount] <- state
                incr curLevelCount
            else v <- dict.[state]
            if edgeOpt.IsSome then 
                for (prod, pos) in parserSource.Reduces.[state].[!curNum] do
                    //printf "%A %A %d %d\n" v.label v.outEdges prod pos
                    reductions.Enqueue (v, prod, pos, edgeOpt)
            v

        ignore <| addVertex startState 0 None

        let makeReductions num =
            while reductions.Count > 0 do
                let vertex, prod, pos, edgeOpt = reductions.Dequeue()
                let nonTerm = parserSource.LeftSide.[prod]

                let inline addChildren node (path : int[]) prod =
                    let family = getFamily node
                    let astExists = 
                        family
                        |> ResizeArray.exists
                            (function (number,children) -> number = prod && Array.forall2 (=) children path)
                    if not astExists then
                        family.Add (prod, path)

                let handlePath (path : int[]) (final : Vertex<_,_>) =
                    let ast = ref -1
                    let state = parserSource.Gotos.[fst final.label].[nonTerm].Value
                    let newVertex = addVertex state num None
                    if not <| newVertex.outEdges.Exists (fun e -> if not (e.dest.label = final.label) then false
                                                                  else ast := e.label
                                                                       true)
                    then
                        ast := nodes.Count
                        let edge = new Edge<int*int, int>(final, !ast)
                        nodes.Add <| NonTerm (new ResizeArray<_>(1))
                        newVertex.addEdge edge
                        if (pos > 0) then
                            for (prod, pos) in parserSource.Reduces.[state].[!curNum] do
                                reductions.Enqueue (newVertex, prod, pos, Some edge)
                    addChildren nodes.[!ast] (Microsoft.FSharp.Collections.Array.copy path) prod

                let rec walk remainLength (vertex : Vertex<_,_>) path =
                    if remainLength = 0 then handlePath path vertex
                    else
                        vertex.outEdges |> ResizeArray.iter
                            (fun e ->
                                path.[remainLength - 1] <- e.label
                                walk (remainLength - 1) e.dest path)
                
                if pos = 0 then
                    let state = parserSource.Gotos.[fst vertex.label].[nonTerm].Value
                    let newVertex = addVertex state num None
                    if newVertex.outEdges |> ResizeArray.forall (fun e -> e.dest.label <> vertex.label) then
                        let edge = new Edge<int*int, int>(vertex, getEpsilon parserSource.LeftSide.[prod])
                        newVertex.addEdge edge
                else 
                    let path = Array.zeroCreate parserSource.Length.[prod]
                    for i = path.Length - 1 downto pos do
                        path.[i] <- getEpsilon parserSource.Rules.[parserSource.RulesStart.[prod] + i]
                    path.[pos - 1] <- edgeOpt.Value.label
                    walk (pos - 1) (edgeOpt.Value : Edge<_,_>).dest path

        let curInd = ref 0
        let isEnd = ref false
        let shift num =
            if !curNum = parserSource.EofIndex then isEnd := true
            else
                let newAstNode = Term enum.Current
                if enum.MoveNext() then
                    curToken := enum.Current
                    curNum := parserSource.TokenToNumber enum.Current
                else
                    curNum := parserSource.EofIndex
                for i = 0 to !curLevelCount-1 do
                    stateToVertex.[usedStates.[i]] <- null
                curLevelCount := 0
                let curBeg = !pBeg
                while curBeg <> !pEnd do
                    let vertex, state = pushes.[!pEnd]
                    let edge = new Edge<_,_>(vertex, nodes.Count)
                    nodes.Add newAstNode
                    //printf "p %A\n" (vertex.label, state)
                    let newVertex = addVertex state num (Some edge)
                    newVertex.addEdge edge
                    nextInd pEnd
        let mutable errorIndex = -1
        while errorIndex = -1 && not !isEnd do
            if !curLevelCount = 0 then
                errorIndex <- !curInd
            else
                makeReductions !curInd
                incr curInd
                shift !curInd
                ()
        if errorIndex <> -1 then Error (errorIndex - 1, !curToken, "Parse error")
        else
            let root = ref None
            printfn "accs: %A" [for i = 0 to parserSource.AccStates.Length-1 do
                                    if parserSource.AccStates.[i] then yield i]
            let addTreeTop res =
                let children = new ResizeArray<_>(1)
                children.Add (parserSource.StartRule, [|res|])
                NonTerm children
            for i = 0 to !curLevelCount-1 do
                printf "%d " usedStates.[i]
                if parserSource.AccStates.[usedStates.[i]] then
                    root := Some nodes.Count
                    stateToVertex.[usedStates.[i]].outEdges.[0].label
                    |> addTreeTop
                    |> nodes.Add
            printfn ""
            match !root with
            | None -> Error (!curInd, Unchecked.defaultof<'TokenType>, "There is no accepting state")
            | Some res -> Success <| new Tree<_>(nodes.ToArray(), res)
