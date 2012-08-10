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

[<Struct>]
type Item =
    val prod : int
    val pos : int
    new (_prod, _pos) = {prod = _prod; pos = _pos}

[<AllowNullLiteral>]
type Vertex (state : int, level : int) =
    let out = new ResizeArray<Edge>(4)
    member this.Level = level
    member this.OutEdges = out
    member this.State = state
    member this.addEdge (edge : Edge) =
        let mutable i = out.Count - 1
        out.Add edge
        while i >= 0 && (out.[i].Dest : Vertex).Level < edge.Dest.Level do
            out.[i+1] <- out.[i]
            i <- i - 1
        out.[i+1] <- edge

    member this.FindIndex state level =
        let mutable i = out.Count - 1
        while i >= 0 && out.[i].Dest.Level <= level && (out.[i].Dest.Level <> level || out.[i].Dest.State <> state) do
            i <- i - 1
        if i >= 0 && out.[i].Dest.Level = level && out.[i].Dest.State = state then i
        else -1
    member this.Edge i = out.[i]

and Edge (destination : Vertex, ast : int) =
    member this.Dest = destination
    member this.Ast = ast

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
        //printfn "%A" !curToken
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
        let stateToVertex : Vertex[] = Array.zeroCreate statesCount

        let inline addVertex state num (edgeOpt : option<Edge>) =
            //printfn "v: %d %d" num state
            let dict = stateToVertex
            let mutable v = null
            if dict.[state] = null then
                //printfn "v(%d,%d)" state num
                v <- new Vertex(state, num)
                dict.[state] <- v
                let push = parserSource.Gotos.[state].[!curNum]
                if push <> 0 then
                    pushes.[!pBeg] <- (v, push)
                    nextInd pBeg
                let arr = parserSource.ZeroReduces.[state].[!curNum]
                if arr <> null then
                    for prod in arr do
                        reductions.Enqueue (v, prod, 0, None) |> ignore
                usedStates.[!curLevelCount] <- state
                incr curLevelCount
            else v <- dict.[state]
            if edgeOpt.IsSome then 
                let arr = parserSource.Reduces.[state].[!curNum]
                if arr <> null then
                    for (prod, pos) in arr do
                        //printf "%A %A %d %d\n" v.label v.outEdges prod pos
                        reductions.Enqueue (v, prod, pos, edgeOpt)
            v

        ignore <| addVertex startState 0 None

        let makeReductions num =
            while reductions.Count > 0 do
                let vertex, prod, pos, edgeOpt = reductions.Dequeue()
                //printfn "r: %A %A: %A %A" vertex.Level vertex.State prod pos
                let nonTerm = parserSource.LeftSide.[prod]

                let inline addChildren node (path : int[]) prod =
                    let family = getFamily node
                    let astExists = 
                        family
                        |> ResizeArray.exists
                            (function (number,children) -> number = prod && Array.forall2 (=) children path)
                    if not astExists then
                        family.Add (prod, path)

                let handlePath (path : int[]) (final : Vertex) =
                    //uses.[prod] <- uses.[prod] + 1
                    let state = parserSource.Gotos.[final.State].[nonTerm]
                    //printfn "f: %d %d" final.Level final.State
                    let ast = ref -1
                    let newVertex = addVertex state num None
                    let ast = 
                        match newVertex.FindIndex final.State final.Level with
                        | -1 -> 
                            let edge = new Edge(final, nodes.Count)
                            nodes.Add <| NonTerm (new ResizeArray<_>(1))
                            newVertex.addEdge edge
                            if (pos > 0) then
                                let arr = parserSource.Reduces.[state].[!curNum]
                                if arr <> null then
                                    for (prod, pos) in arr do
                                        reductions.Enqueue (newVertex, prod, pos, Some edge)
                            edge.Ast
                        | x -> (newVertex.Edge x).Ast
                    addChildren nodes.[ast] (Microsoft.FSharp.Collections.Array.copy path) prod

                let rec walk remainLength (vertex : Vertex) path =
                    if remainLength = 0 then handlePath path vertex
                    else
                        //printfn "  m: %d %d" vertex.Level vertex.State
                        vertex.OutEdges |> ResizeArray.iter
                            (fun e ->
                                path.[remainLength - 1] <- e.Ast
                                walk (remainLength - 1) e.Dest path)
                
                if pos = 0 then
                    let state = parserSource.Gotos.[vertex.State].[nonTerm]
                    let newVertex = addVertex state num None
                    if newVertex.FindIndex vertex.State vertex.Level = -1 then
                        let edge = new Edge(vertex, getEpsilon parserSource.LeftSide.[prod])
                        newVertex.addEdge edge
                else 
                    let path = Array.zeroCreate parserSource.Length.[prod]
                    for i = path.Length - 1 downto pos do
                        path.[i] <- getEpsilon parserSource.Rules.[parserSource.RulesStart.[prod] + i]
                    path.[pos - 1] <- edgeOpt.Value.Ast
                    walk (pos - 1) (edgeOpt.Value : Edge).Dest path

        let curInd = ref 0
        let isEnd = ref false
        let shift num =
            if !curNum = parserSource.EofIndex then isEnd := true
            else
                let newAstNode = Term enum.Current
                if enum.MoveNext() then
                    curToken := enum.Current
                    //printfn "%A" !curToken
                    curNum := parserSource.TokenToNumber enum.Current
                else
                    curNum := parserSource.EofIndex
                for i = 0 to !curLevelCount-1 do
                    stateToVertex.[usedStates.[i]] <- null
                curLevelCount := 0
                let curBeg = !pBeg
                let curAst = nodes.Count
                nodes.Add newAstNode
                while curBeg <> !pEnd do
                    let vertex, state = pushes.[!pEnd]
                    //printfn "p: %d %d -> %d" vertex.Level vertex.State state
                    let edge = new Edge(vertex, curAst)
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
        if errorIndex <> -1 then
            Error (errorIndex - 1, !curToken, "Parse error")
        else
            let root = ref None
            //printfn "accs: %A" [for i = 0 to parserSource.AccStates.Length-1 do
            //                        if parserSource.AccStates.[i] then yield i]
            let addTreeTop res =
                let children = new ResizeArray<_>(1)
                children.Add (parserSource.StartRule, [|res|])
                NonTerm children
            for i = 0 to !curLevelCount-1 do
                //printf "%d " usedStates.[i]
                if parserSource.AccStates.[usedStates.[i]] then
                    root := Some nodes.Count
                    (stateToVertex.[usedStates.[i]].Edge 0).Ast
                    |> addTreeTop
                    |> nodes.Add
            match !root with
            | None -> Error (!curInd, Unchecked.defaultof<'TokenType>, "There is no accepting state")
            | Some res -> Success <| new Tree<_>(nodes.ToArray(), res)
