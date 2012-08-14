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
open Yard.Generators.RNGLR.DataStructures

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType>
    | Error of int * 'TokenType * string

[<AllowNullLiteral>]
type private Vertex (state : int, level : int) =
    let mutable out : Edge[] = null
    member this.Level = level
    member this.OutEdges
        with get() = out
        and set v = out <- v
    member this.State = state
    member this.Edge i = out.[i]

and private Edge =
    struct 
        val Dest : Vertex
        val Ast : int
        new (d,a) = {Dest = d; Ast = a}
    end


let inline private less (v' : Vertex) (v : Vertex) = v'.Level < v.Level || (v'.Level = v.Level && v'.State < v.State)
let inline private eq (v' : Vertex) (v : Vertex) = v'.Level = v.Level && v'.State = v.State

let private addSimpleEdge (v : Vertex) (ast : int) (out : ResizeArray<Vertex * int>) =
    let mutable i = out.Count - 1
    out.Add Unchecked.defaultof<_>
    while i >= 0 && (less (fst out.[i]) v) do
        out.[i+1] <- out.[i]
        i <- i - 1
    out.[i+1] <- (v, ast)

let private findSimpleIndex (v : Vertex) (f : int) (out : ResizeArray<Vertex * int>) =
    let mutable i = out.Count - 1
    while i >= 0 && less (fst out.[i]) v do
        i <- i - 1
    while i >= 0 && (let v',f' = out.[i] in eq v' v && f <> f') do
        i <- i - 1
    if i >= 0 && (let v',f' = out.[i] in eq v' v && f = f') then i
    else -1

let private addEdge (v : Vertex) nodes (out : ResizeArray<Vertex * Family * int>) (allNodes : BlockResizeArray<_>) =
    let mutable i = out.Count - 1
    let inline fst (x,_,_) = x
    out.Add Unchecked.defaultof<_>
    while i >= 0 && less (fst out.[i]) v do
        out.[i+1] <- out.[i]
        i <- i - 1
    let num = 
        if i >= 0 && eq (fst out.[i]) v then
            let _,_,n = out.[i] in n
        else
            allNodes.Add Unchecked.defaultof<_>
            allNodes.Count - 1
    out.[i+1] <- (v, nodes, num)
    num

let private findIndex (v : Vertex) (f : Family) (out : ResizeArray<Vertex * Family * int>) =
    let inline fst (x,_,_) = x
    let mutable i = out.Count - 1
    while i >= 0 && less (fst out.[i]) v do
        i <- i - 1
    while i >= 0 && (let v',f',_ = out.[i] in eq v' v && f <> f') do
        i <- i - 1
    if i >= 0 && (let v',f',_ = out.[i] in eq v' v && f = f') then i
    else -1

let buildAst<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : seq<'TokenType>) =
    let enum = tokens.GetEnumerator()
    let startState = 0
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let inline getEpsilon i = -1-i
    if not <| enum.MoveNext() || parserSource.EofIndex = parserSource.TokenToNumber enum.Current then
        if parserSource.AcceptEmptyInput then
            new Tree<_>([||], null, getEpsilon startNonTerm) |> Success
        else
            Error (0, Unchecked.defaultof<'TokenType>, "This grammar cannot accept empty string")
    else
        let curToken = ref enum.Current
        //printfn "%A" !curToken
        let curNum = ref (parserSource.TokenToNumber enum.Current)
        /// Here all nodes in AST will be collected
        let nodes = new BlockResizeArray<_>()
        let tokens = new BlockResizeArray<_>()
        // Must be number of non-terminals, but doesn't matter
        let nonTermsCountLimit = 1 + max (Array.max parserSource.Rules) (Array.max parserSource.LeftSide)
            
        let reductions = new Queue<_>(10)
        let statesCount = parserSource.Gotos.Length
        let edges = Array.init statesCount (fun _ -> new ResizeArray<Vertex * Family * int>())
        let simpleEdges = Array.init statesCount (fun _ -> new ResizeArray<Vertex * int>())

        let pushes = Array.zeroCreate (statesCount * 2 + 10)
        let pBeg, pEnd = ref 0, ref 0
        let inline nextInd n =
            if !n + 1 <> pushes.Length then n := !n + 1
            else n := 0
        let usedStates : int[] = Array.zeroCreate statesCount
        let curLevelCount = ref 0
        let stateToVertex : Vertex[] = Array.zeroCreate statesCount

        let inline addVertex state num (edgeOpt : option<_>) =
            //printfn "v: %d %d" num state
            let dict = stateToVertex
            if dict.[state] = null then
                //printfn "v(%d,%d)" state num
                let v = new Vertex(state, num)
                dict.[state] <- v
                let push = parserSource.Gotos.[state].[!curNum]
                if push <> 0 then
                    pushes.[!pBeg] <- (v, push)
                    nextInd pBeg
                let arr = parserSource.ZeroReduces.[state].[!curNum]
                if arr <> null then
                    for prod in arr do
                        reductions.Enqueue (v, prod, 0, None)
                usedStates.[!curLevelCount] <- state
                incr curLevelCount
            let v = dict.[state]
            if edgeOpt.IsSome then 
                let arr = parserSource.Reduces.[state].[!curNum]
                if arr <> null then
                    for (prod, pos) in arr do
                        //printf "%A %A %d %d\n" v.label v.outEdges prod pos
                        reductions.Enqueue (v, prod, pos, edgeOpt)
            v

        ignore <| addVertex startState 0 None
        let trd (_,_,x) = x
        let makeReductions num =
            while reductions.Count > 0 do
                let vertex, prod, pos, edgeOpt = reductions.Dequeue()
                //printfn "r: %A %A: %A %A" vertex.Level vertex.State prod pos
                let nonTerm = parserSource.LeftSide.[prod]

                let handlePath (path : int[]) (final : Vertex) =
                    //uses.[prod] <- uses.[prod] + 1
                    let state = parserSource.Gotos.[final.State].[nonTerm]
                    //printfn "f: %d %d" final.Level final.State
                    let newVertex = addVertex state num None
                    let family = new Family(prod, Array.copy path)
                    if findIndex final family edges.[state] = -1 then
                        let edge = addEdge final family edges.[state] nodes
                        if (pos > 0 && edge = nodes.Count-1) then
                            let arr = parserSource.Reduces.[state].[!curNum]
                            if arr <> null then
                                for (prod, pos) in arr do
                                    reductions.Enqueue (newVertex, prod, pos, Some (final, edge))
                let rec walk remainLength (vertex : Vertex) path =
                    if remainLength = 0 then handlePath path vertex
                    else
                        //printfn "  m %d: %d %d" remainLength vertex.Level vertex.State
                        if vertex.Level <> num then
                            vertex.OutEdges |> Array.iter
                                (fun e ->
                                    path.[remainLength - 1] <- e.Ast
                                    walk (remainLength - 1) e.Dest path)
                        else
                            simpleEdges.[vertex.State] |> ResizeArray.iter(fun (v,a) ->
                                    path.[remainLength - 1] <- a
                                    walk (remainLength - 1) v path)
                            
                            let mutable i = 0
                            let edges = edges.[vertex.State]
                            let mutable count = 0
                            while i < edges.Count do
                                let (v,_,a) = edges.[i]
                                let mutable j = i+1
                                path.[remainLength - 1] <- a
                                walk (remainLength - 1) v path
                                while j < edges.Count && trd edges.[j] = a do
                                    j <- j + 1
                                i <- j
                
                if pos = 0 then
                    let state = parserSource.Gotos.[vertex.State].[nonTerm]
                    let newVertex = addVertex state num None
                    let ast = getEpsilon parserSource.LeftSide.[prod]
                    if findSimpleIndex vertex ast simpleEdges.[state] = -1 then
                        addSimpleEdge vertex ast simpleEdges.[state]
                else 
                    let path = Array.zeroCreate parserSource.Length.[prod]
                    for i = path.Length - 1 downto pos do
                        path.[i] <- getEpsilon parserSource.Rules.[parserSource.RulesStart.[prod] + i]
                    path.[pos - 1] <- snd edgeOpt.Value
                    walk (pos - 1) (fst edgeOpt.Value) path

        let curInd = ref 0
        let isEnd = ref false
        let attachEdges () =
            let inline trd (_,_,x) = x
            let inline snd (_,x,_) = x
            for s = 0 to !curLevelCount-1 do
                let mutable i = 0
                let vertex = usedStates.[s]
                let edges = edges.[vertex]
                let mutable count = 0
                while i < edges.Count do
                    let k = trd edges.[i]
                    let mutable j = i+1
                    while j < edges.Count && trd edges.[j] = k do
                        j <- j + 1
                    i <- j
                    count <- count + 1
                count <- count + simpleEdges.[vertex].Count
                let vEdges = Array.zeroCreate count
                i <- 0
                count <- 0
                while i < edges.Count do
                    let (v,_,a) = edges.[i]
                    let mutable j = i+1
                    while j < edges.Count && trd edges.[j] = a do
                        j <- j + 1
                    let res = Array.zeroCreate (j - i)
                    for k = i to j-1 do
                        res.[k-i] <- snd edges.[k]
                    vEdges.[count] <- new Edge(v, a)
                    count <- count + 1
                    nodes.Set a (NonTerm res)
                    i <- j

                for i = 0 to simpleEdges.[vertex].Count - 1 do
                    let v, a = simpleEdges.[vertex].[i]
                    vEdges.[count] <- new Edge(v, a)
                    count <- count + 1

                stateToVertex.[vertex].OutEdges <- vEdges
                edges.Clear()
                simpleEdges.[vertex].Clear()

        let shift num =
            if !curNum = parserSource.EofIndex then isEnd := true
            else
                let newAstNode = Term tokens.Count
                tokens.Add enum.Current
                if enum.MoveNext() then
                    curToken := enum.Current
                    curNum := parserSource.TokenToNumber enum.Current
                else
                    curNum := parserSource.EofIndex
                for s = 0 to !curLevelCount-1 do
                    stateToVertex.[usedStates.[s]] <- null
                curLevelCount := 0
                let curBeg = !pBeg
                let curAst = nodes.Count
                nodes.Add newAstNode
                while curBeg <> !pEnd do
                    let vertex, state = pushes.[!pEnd]
                    //printfn "p: %d %d -> %d" vertex.Level vertex.State state
                    //printf "p %A\n" (vertex.label, state)
                    let newVertex = addVertex state num (Some (vertex,curAst))
                    addSimpleEdge vertex curAst simpleEdges.[state]
                    nextInd pEnd
        let mutable errorIndex = -1
        while errorIndex = -1 && not !isEnd do
            if !curLevelCount = 0 then
                errorIndex <- !curInd
            else
                makeReductions !curInd
                attachEdges()
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
                let children = [| new Family(parserSource.StartRule,  [|res|]) |]
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
            | Some res -> Success <| new Tree<_>(nodes.ToArray(), tokens.ToArray(), res)
