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

module Yard.Generators.ARNGLR.Parser

open Yard.Generators.RNGLR
open Yard.Generators.Common.ARNGLR.SimpleAST
open System.Collections.Generic
open Yard.Generators.Common.DataStructures
//open Yard.Generators.RNGLR.Parser 
open Microsoft.FSharp.Collections
open AbstractAnalysis.Common

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
    let passingReductions = new ResizeArray<_>(10)
    member this.Level = level
    member this.OutEdges = out
    member this.State = state
    member this.PassingReductions = passingReductions

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

and VInfo<'TokenType> (vNum, statesCount) =
    member val vNum = vNum with get
    member val reductions = new Queue<_>(10) with get
    member val processedGssVertices = new ResizeArray<Vertex>() with get
    member val unprocessedGssVertices = new ResizeArray<Vertex>() with get


[<AllowNullLiteral>]
type Path (first : int, tail : int, paths : ResizeArray<Path>) =
    let allPaths = paths
    let rec toList (path : Path) result = 
        match path with 
        | null -> result
        | _ -> toList allPaths.[path.Tail] (path.First :: result)
        
    member this.First = first
    member this.Tail = tail
    member this.AddEdge edge = 
        allPaths.Add this
        new Path (edge, allPaths.Count, allPaths)
    member this.ToList = toList this [] 

let buildAstAbstract<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : ParserInputGraph<'TokenType>) =
    let statesCount = parserSource.Gotos.Length
    let startV, finalV, innerGraph =
        let verticesMap = tokens.Vertices |> Seq.map (fun i -> (i, new VInfo<_> (i, statesCount))) |> dict
        let g = new QuickGraph.AdjacencyGraph<_,_>()
        let added = 
            tokens.Edges |> Seq.map (fun e -> new QuickGraph.TaggedEdge<_,_>(verticesMap.[e.Source], verticesMap.[e.Target], e.Tag))
            |> g.AddVerticesAndEdgeRange
        //if added <> tokens.EdgeCount then failwithf "Error while convertion input parser graph to iier representation. Expected edges: %A, actual: %A" tokens.EdgeCount added
        verticesMap.[Seq.min tokens.Vertices], verticesMap.[Seq.max tokens.Vertices], g
    
    let nodes = new BlockResizeArray<_>()
    // Must be number of non-terminals, but doesn't matter
    let nonTermsCountLimit = 1 + (Array.max parserSource.LeftSide)
    let startState = 0
    let inline getEpsilon i = -1-i
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let verticesToProcess = new Queue<_>()
    verticesToProcess.Enqueue (startV)
    let mutable errorIndex = -1
    let verticesSeenBefore = new Dictionary<_,_>()
    for v in innerGraph.Vertices do
        verticesSeenBefore.Add(v, false)
    
    let edgesToTerms = new Dictionary<_,_>()
    let push currentGraphV gssVertex state =
        let reductions = new ResizeArray<_>(10)
        let newUnprocessedGssVs = new ResizeArray<_>(2)
        for e in innerGraph.OutEdges currentGraphV do
        //printfn "v(%d,%d)" state num
            let push = parserSource.Gotos.[state].[parserSource.TokenToNumber e.Tag]
            if push <> 0 
            then
                let findGssVertex (target : VInfo<_>) = 
                    let srcV = target.processedGssVertices |> ResizeArray.tryFind(fun v -> v.State = push)
                    match srcV with
                    | Some v -> v
                    | None -> 
                        let srcV = target.unprocessedGssVertices |> ResizeArray.tryFind(fun v -> v.State = push)
                        match srcV with
                        | Some v -> v
                        | None -> 
                            let v = new Vertex(push, target.vNum)
                            if currentGraphV = target
                            then newUnprocessedGssVs.Add v
                            else target.unprocessedGssVertices.Add v
                            v
                let targetGssV = findGssVertex e.Target

                let mutable edge = Unchecked.defaultof<_>
                if edgesToTerms.ContainsKey e
                then
                    edge <- new Edge(gssVertex, edgesToTerms.[e])
                    targetGssV.addEdge edge
                else 
                    nodes.Add <| Term e.Tag
                    edgesToTerms.Add(e, nodes.Count - 1)
                    edge <- new Edge(gssVertex, nodes.Count - 1)
                    targetGssV.addEdge edge

                for e2 in innerGraph.OutEdges e.Target do
                    let arr = parserSource.Reduces.[push].[parserSource.TokenToNumber e2.Tag]
                    if arr <> null then
                        for (prod, pos) in arr do
                            //printf "%A %A %d %d\n" v.label v.outEdges prod pos
                            reductions.Add(gssVertex, prod, pos, Some edge, e.Target)

            let arr = parserSource.ZeroReduces.[state].[parserSource.TokenToNumber e.Tag]
            if arr <> null then
                for prod in arr do
                    reductions.Add(gssVertex, prod, 0, None, e.Target)
        
        let reductionSet = new ResizeArray<_>(10)
        for gssVertex, prod, pos, edge, target in reductions do
            let reductionExists = 
                reductionSet
                |> ResizeArray.exists(function v, p, _pos, e, target -> v = gssVertex && p = prod && _pos = pos && edge = e)
            if not reductionExists then reductionSet.Add(gssVertex, prod, pos, edge, target)
        for gssVertex, prod, pos, edge, target in reductionSet do
            let r = gssVertex, prod, pos, edge
            let reds = target.reductions
            target.reductions.Enqueue(r)
        
        currentGraphV.processedGssVertices.Add(gssVertex)
        newUnprocessedGssVs

    let (*inline*) addVertex (currentGraphV:VInfo<_>) state (edgeOpt : option<Edge>) =
        let mutable v = null
        let mutable isNew = false
        let vOpt = currentGraphV.processedGssVertices |> ResizeArray.tryFind (fun v -> v.State = state)
        match vOpt with
        | None ->
            let vOpt = currentGraphV.unprocessedGssVertices |> ResizeArray.tryFind (fun v -> v.State = state)
            match vOpt with
            | None ->
                v <- new Vertex(state, currentGraphV.vNum)
                isNew <- true
                currentGraphV.unprocessedGssVertices.Add v
            |Some x -> v <- x
        |Some x -> 
            v <- x
            
            let isInUnprocessedList (v:Vertex) =
                currentGraphV.unprocessedGssVertices 
                |> ResizeArray.exists (fun _v -> _v.Level = v.Level && _v.State = v.State ) 
            if not <| isInUnprocessedList v 
            then
                currentGraphV.unprocessedGssVertices.Add v 
        v, isNew
    
    let verticesToRecalc = new ResizeArray<_> (10)

    let inline addChildren node path prod =
        let family = getFamily node
        let astExists = 
            family
            |> ResizeArray.exists
                (function (number,children) -> number = prod && Array.forall2 (=) children path)
        if not astExists then
            family.Add (prod, path)

    let handlePath (path : _[]) (final : Vertex) currentGraphV startV nonTerm pos prod shouldEnqueueVertex =
        let state = parserSource.Gotos.[final.State].[nonTerm]
        let newVertex, isNew = addVertex startV state None
        if shouldEnqueueVertex && isNew then verticesToRecalc.Add startV
        let ast = 
            match newVertex.FindIndex final.State final.Level with
            | -1 -> 
                let edge = new Edge(final, nodes.Count)
                nodes.Add <| NonTerm (new ResizeArray<_>(1))
                newVertex.addEdge edge
                if (pos > 0)
                then
                    for e in innerGraph.OutEdges currentGraphV do
                        let arr = parserSource.Reduces.[state].[parserSource.TokenToNumber e.Tag]
                        if arr <> null then
                            for (prod, pos) in arr do
                                currentGraphV.reductions.Enqueue(newVertex, prod, pos, Some edge)
                edge.Ast
            | x -> (newVertex.Edge x).Ast
        addChildren nodes.[ast] (Microsoft.FSharp.Collections.Array.copy path) prod

    let rec walk remainLength (vertex : Vertex) path currentGraphV startV nonTerm pos prod shouldEnqueueVertex = 
        if remainLength = 0 
        then handlePath path vertex currentGraphV startV nonTerm pos prod shouldEnqueueVertex
        else
            if not (ResizeArray.exists 
                       (fun (_prod, _remainLength, _path, _nonTerm, _pos, _startV) -> 
                                    prod = _prod && remainLength = _remainLength && path = _path && nonTerm = _nonTerm && pos = _pos && startV = _startV)
                       vertex.PassingReductions)
            then vertex.PassingReductions.Add (prod, remainLength, Array.copy path, nonTerm, pos, startV)
            vertex.OutEdges |> ResizeArray.iter
                (fun e ->
                    path.[remainLength - 1] <- e.Ast
                    walk (remainLength - 1) e.Dest path currentGraphV startV nonTerm pos prod shouldEnqueueVertex)

    let makeSingleReduction currentGraphV (vertex : Vertex) prod pos (edgeOpt : Edge option) =
        let nonTerm = parserSource.LeftSide.[prod]

        if pos = 0 then
            let state = parserSource.Gotos.[vertex.State].[nonTerm]
            let newVertex, isNew = addVertex currentGraphV state None
            if newVertex.FindIndex vertex.State vertex.Level = -1 then
                let edge = new Edge(vertex, getEpsilon parserSource.LeftSide.[prod])
                newVertex.addEdge edge
        else 
            let path = Array.zeroCreate pos //parserSource.Length.[prod]
            //for i = path.Length - 1 downto pos do
            //    let blah = parserSource.Rules.[parserSource.RulesStart.[prod] + i]
            //    path.[i] <- getEpsilon (if blah.Length = 0 then -1 else blah.[0]) ///????
            path.[pos - 1] <- edgeOpt.Value.Ast
            walk (pos - 1) (edgeOpt.Value : Edge).Dest path currentGraphV currentGraphV nonTerm pos prod false

    let makeReductions (currentGraphV : VInfo<_>) =
        while currentGraphV.reductions.Count > 0 do
            let vertex, prod, pos, edgeOpt = currentGraphV.reductions.Dequeue()
            makeSingleReduction currentGraphV vertex prod pos edgeOpt


    let handlePassingReductions (graphV : VInfo<_>) =
        verticesToRecalc.RemoveAll(fun _ -> true) |> ignore
        for gssV in graphV.processedGssVertices do
            let passingReductions = gssV.PassingReductions
            for prod, remainLength, path, nonTerm, pos, startV in passingReductions do
                walk remainLength gssV path graphV startV nonTerm pos prod true
        verticesToRecalc

    let processVertex v = 
        makeReductions v
        
        let newGssVs = new ResizeArray<_>(2)
        for gssVertex in v.unprocessedGssVertices do 
            newGssVs.AddRange(push v gssVertex gssVertex.State)
        v.unprocessedGssVertices.RemoveAll(fun _ -> true) |> ignore
        if newGssVs.Count > 0 
        then
            v.unprocessedGssVertices.AddRange(newGssVs)
            verticesToProcess.Enqueue(v)

        if verticesSeenBefore.[v]
        then
            for e in innerGraph.OutEdges(v) do
                if e.Target.reductions.Count > 0 || e.Target.unprocessedGssVertices.Count > 0
                then
                    verticesToProcess.Enqueue (e.Target)

            let toRecalc = handlePassingReductions v
            for vToRecalc in toRecalc do verticesToProcess.Enqueue (vToRecalc)
        else
            for e in innerGraph.OutEdges(v) do
                verticesToProcess.Enqueue (e.Target)


    if tokens.EdgeCount = 0
    then
        if parserSource.AcceptEmptyInput
        then new Tree<_>([||], getEpsilon startNonTerm) |> Success
        else Error (0, Unchecked.defaultof<'TokenType>, "This grammar cannot accept empty string")
    else
        let v = addVertex startV startState None
        while errorIndex = -1 && verticesToProcess.Count > 0 do
            let curV = verticesToProcess.Dequeue()
            processVertex curV
            verticesSeenBefore.[curV] <- true

        if errorIndex <> -1 then
            Error (errorIndex - 1, Unchecked.defaultof<'TokenType>, "Parse error")
        else
            let root = ref None
            //printfn "accs: %A" [for i = 0 to parserSource.AccStates.Length-1 do
            //                        if parserSource.AccStates.[i] then yield i]
            let addTreeTop res =
                let children = new ResizeArray<_>(1)
                children.Add (parserSource.StartRule, [|res|])
                NonTerm children
            for v in innerGraph.Edges |> Seq.filter (fun e -> e.Target = finalV) |> Seq.collect (fun e -> e.Source.processedGssVertices) do
                if parserSource.AccStates.[v.State] then
                    root := Some nodes.Count
                    (v.Edge 0).Ast
                    |> addTreeTop
                    |> nodes.Add
            match !root with
            | None -> Error (-1, Unchecked.defaultof<'TokenType>, "There is no accepting state")
            | Some res -> 
                let tree = new Tree<_>(nodes.ToArray(), res)
                tree.AstToDot parserSource.NumToString parserSource.TokenToNumber parserSource.TokenData.Value  parserSource.LeftSide "../../../Tests/AbstractRNGLR/DOT/sppf.dot"
                Success <| tree