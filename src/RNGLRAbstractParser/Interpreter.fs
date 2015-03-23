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

open AbstractAnalysis.Common
open Microsoft.FSharp.Collections
open System.Collections.Generic
open System.Linq
open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode
open Yard.Generators.Common.DataStructures
open Yard.Generators.RNGLR

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType>
    | Error of int * 'TokenType * string

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
    member val processedGssVertices = new ResizeArray<Vertex>() with get
    member val unprocessedGssVertices = new ResizeArray<Vertex>() with get
    member val reductions = new Queue<_> (10) with get

    member this.GetReduction() = 
        this.reductions.Dequeue()

    member this.AddReduction(r) =
        this.reductions.Enqueue(r)

and [<Struct>]
    Reduction =
    val gssVertex : Vertex
    val prod : int
    val pos : int
    val edge : Edge option
    new (_gssVertex, _prod, _pos, _edge) = {gssVertex = _gssVertex; prod = _prod; pos = _pos; edge = _edge}

and [<AllowNullLiteral>]
    Path (head : AstNode, tail : Path, length : int) = 
    let compare (x : AstNode) (y : AstNode) = 
        match x with
        | :? Epsilon as x' -> match y with | :? Epsilon as y' -> x'.EpsilonNonTerm = y'.EpsilonNonTerm | _ -> false
        | :? Terminal as x' -> match y with | :? Terminal as y' -> x'.TokenNumber = y'.TokenNumber | _ -> false
        | :? AST as xast -> match y with | :? AST as yast -> 
                                                xast.pos = yast.pos 
                                                && xast = yast 
                                         | _ -> false
        | _ -> false

    member this.Head = head
    member this.Tail = tail
    member this.Length = length


    member this.AddEdge edge =
        new Path(edge, this, this.Length + 1)
    
    member this.IsEqual (other : Path) =
        if this = null && other <> null || this <> null && other = null
        then false
        else 
            this = null && other = null || compare this.Head other.Head && this.Tail.IsEqual(other.Tail)

    member this.IsEqual (nodes : Nodes) = 
        nodes.Length = this.Length 
        && let h = ref this in nodes.isForAll(fun x -> let res = compare x h.Value.Head
                                                       h := h.Value.Tail
                                                       res )

    member this.ToArray() =
        let p = ref this
        Array.init this.Length (fun _ -> let cur = p.Value.Head
                                         p := p.Value.Tail
                                         cur)
       
    new (head, tail) = new Path(head, tail, if tail = null then 1 else tail.Length + 1)
    new (edge) = new Path (edge, null, 1)

let buildAstAbstract<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : ParserInputGraph<'TokenType>) =
    let statesCount = parserSource.Gotos.Length
    let startV, finalV, innerGraph =
        let verticesMap = tokens.Vertices |> Seq.map (fun i -> (i, new VInfo<_> (i, statesCount))) |> dict
        let g = new QuickGraph.AdjacencyGraph<_,_>()
        let added = 
            tokens.Edges |> Seq.map (fun e -> new QuickGraph.TaggedEdge<_,_>(verticesMap.[e.Source], verticesMap.[e.Target], e.Tag))
            |> g.AddVerticesAndEdgeRange
        //if added <> tokens.EdgeCount then failwithf "Error while convertion input parser graph to iier representation. Expected edges: %A, actual: %A" tokens.EdgeCount added
        verticesMap.[tokens.InitState], verticesMap.[tokens.FinalState], g
    
    let nodes = new BlockResizeArray<AstNode>()
    let debugNodes = new BlockResizeArray<AstNode>()
    let terminals = new BlockResizeArray<'TokenType>()
    // Must be number of non-terminals, but doesn't matter
    let nonTermsCountLimit = 1 + (Array.max parserSource.LeftSide)
    let startState = 0
    let inline getEpsilon i = new Epsilon(-1-i)
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let verticesToProcess = new Queue<_>()
    verticesToProcess.Enqueue (startV)
    let mutable errorIndex = -1
    let verticesSeenBefore = new Dictionary<_,_>()
    for v in innerGraph.Vertices do
        verticesSeenBefore.Add(v, false)
    
    let customEnqueue (elem : VInfo<_>) = 
        verticesToProcess.Enqueue(elem)

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
                else 
                    terminals.Add e.Tag
                    nodes.Add <| Terminal (terminals.Count - 1)
                    edgesToTerms.Add(e, nodes.Count - 1)
                    edge <- new Edge(gssVertex, nodes.Count - 1)

                if targetGssV.FindIndex gssVertex.State gssVertex.Level = -1
                then
                    targetGssV.addEdge edge

                for e2 in innerGraph.OutEdges e.Target do
                    let arr = parserSource.ZeroReduces.[push].[parserSource.TokenToNumber e2.Tag]
                    if arr <> null then
                        for prod in arr do
                            reductions.Add(new Reduction(targetGssV, prod, 0, None), e.Target)

                    let arr = parserSource.Reduces.[push].[parserSource.TokenToNumber e2.Tag]
                    if arr <> null then
                        for (prod, pos) in arr do
                            //printf "%A %A %d %d\n" v.label v.outEdges prod pos
                            reductions.Add(new Reduction(gssVertex, prod, pos, Some edge), e.Target)

            let arr = parserSource.ZeroReduces.[state].[parserSource.TokenToNumber e.Tag]
            if arr <> null then
                customEnqueue currentGraphV
                for prod in arr do
                    reductions.Add(new Reduction(gssVertex, prod, 0, None), currentGraphV)
        
        let reductionSet = new ResizeArray<Reduction * VInfo<_>>(10)
        for r, target in reductions do
            let reductionExists = 
                reductionSet
                |> ResizeArray.exists(function red, _ -> red = r)
            if not reductionExists then reductionSet.Add(r, target)
        for r, target in reductionSet do
            target.AddReduction(r)
        
        if not <| currentGraphV.processedGssVertices.Contains(gssVertex)
        then 
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
        v, isNew
    
    let verticesToRecalc = new ResizeArray<_> (10)

    let (*inline*) addChildren node (path : Path) prod =
        let ast = getFamily node
        let familyOpt = 
            ast.findFamily (function family -> family.prod = prod && path.IsEqual(family.nodes))
        match familyOpt with 
        | None -> 
            let newFamily = new Family (prod, new Nodes(path.ToArray()))
            if ast.first = Unchecked.defaultof<_> 
            then ast.first <- newFamily 
            else
                if ast.other = null 
                then ast.other <- [|newFamily|]
                else ast.other <- Array.append ast.other [|newFamily|]
        | Some _ -> ()

    let handlePath (path : Path) (final : Vertex) currentGraphV startV nonTerm pos prod shouldEnqueueVertex =
        let state = parserSource.Gotos.[final.State].[nonTerm]
        let newVertex, isNew = addVertex startV state None
        if shouldEnqueueVertex && isNew then verticesToRecalc.Add startV
        let ast = 
            match newVertex.FindIndex final.State final.Level with
            | -1 -> 
                let edge = new Edge(final, nodes.Count)
                nodes.Add <| new AST (Unchecked.defaultof<_>, null)
                newVertex.addEdge edge
                if (pos > 0)
                then
                    for e in innerGraph.OutEdges currentGraphV do
                        let arr = parserSource.Reduces.[state].[parserSource.TokenToNumber e.Tag]
                        if arr <> null then
                            for (prod, pos) in arr do
                                currentGraphV.AddReduction(new Reduction(newVertex, prod, pos, Some edge))
                edge.Ast
            | x -> (newVertex.Edge x).Ast 
        if ast >= 0 
        then addChildren nodes.[ast] path prod
        else 
            let edge = new Edge(final, nodes.Count)
            nodes.Add <| new AST (Unchecked.defaultof<_>, null)
            newVertex.addEdge edge
            if (pos > 0)
            then
                for e in innerGraph.OutEdges startV do
                    let arr = parserSource.Reduces.[state].[parserSource.TokenToNumber e.Tag]
                    if arr <> null then
                        for (prod, pos) in arr do
                            startV.AddReduction(new Reduction(newVertex, prod, pos, Some edge))
            addChildren nodes.[nodes.Count - 1] path prod

    let rec walk remainLength (vertex : Vertex) path currentGraphV startV nonTerm pos prod shouldEnqueueVertex = 
        if remainLength = 0 
        then handlePath path vertex currentGraphV startV nonTerm pos prod shouldEnqueueVertex
        else
            if not (ResizeArray.exists 
                       (fun (_prod, _remainLength, _path : Path, _nonTerm, _pos, _startV) ->
                            prod = _prod 
                            && remainLength = _remainLength 
                            && path.IsEqual(_path)
                            && nonTerm = _nonTerm 
                            && pos = _pos 
                            && startV = _startV)
                       vertex.PassingReductions)
            then vertex.PassingReductions.Add (prod, remainLength, path, nonTerm, pos, startV)
            vertex.OutEdges |> ResizeArray.iter
                (fun e ->
                    let newPath = path.AddEdge(if e.Ast < 0 then new Epsilon(e.Ast) :> AstNode else nodes.[e.Ast])
                    walk (remainLength - 1) e.Dest newPath currentGraphV startV nonTerm pos prod shouldEnqueueVertex)

    let makeSingleReduction currentGraphV (reduction : Reduction) =
        let nonTerm = parserSource.LeftSide.[reduction.prod]

        if reduction.pos = 0 then
            let state = parserSource.Gotos.[reduction.gssVertex.State].[nonTerm]
            let newVertex, isNew = addVertex currentGraphV state None
            if newVertex.FindIndex reduction.gssVertex.State reduction.gssVertex.Level = -1 then
                let edge = new Edge(reduction.gssVertex, -parserSource.LeftSide.[reduction.prod]-1)
                newVertex.addEdge edge
        else 
            let path = new Path(nodes.[reduction.edge.Value.Ast])
            walk (reduction.pos - 1) (reduction.edge.Value : Edge).Dest path currentGraphV currentGraphV nonTerm reduction.pos reduction.prod false

    let makeReductions (currentGraphV : VInfo<_>) =
        while currentGraphV.reductions.Count > 0 do
            let r = currentGraphV.GetReduction()
            makeSingleReduction currentGraphV r


    let handlePassingReductions (graphV : VInfo<_>) =
        verticesToRecalc.Clear()
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
        v.unprocessedGssVertices.Clear()
        if newGssVs.Count > 0 
        then
            v.unprocessedGssVertices.AddRange(newGssVs)
            customEnqueue v
        if verticesSeenBefore.[v]
        then
            for e in innerGraph.OutEdges(v) do
                let toRecalc = handlePassingReductions e.Target
                for vToRecalc in toRecalc do customEnqueue vToRecalc
                
                if e.Target.reductions.Count > 0 || e.Target.unprocessedGssVertices.Count > 0
                then
                    customEnqueue e.Target
            let toRecalc = handlePassingReductions v
            for vToRecalc in toRecalc do customEnqueue vToRecalc
        else
            for e in innerGraph.OutEdges(v) do
                customEnqueue e.Target

    if tokens.EdgeCount = 0
    then
        if parserSource.AcceptEmptyInput
        then new Tree<_>([||], getEpsilon startNonTerm, parserSource.Rules) |> Success
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
                let children = new Family(parserSource.StartRule, new Nodes([|res|]))
                new AST(children, null)
            for v in innerGraph.Edges |> Seq.filter (fun e -> e.Target = finalV) |> Seq.collect (fun e -> e.Source.processedGssVertices) do
                if parserSource.AccStates.[v.State]
                then
                    root := Some nodes.Count
                    let nonEpsilonEdge = v.OutEdges.FirstOrDefault(fun x -> x.Ast >= 0)
                    if nonEpsilonEdge <> Unchecked.defaultof<_>
                    then
                        nodes.[nonEpsilonEdge.Ast]
                        |> addTreeTop
                        |> nodes.Add
            match !root with
            | None -> Error (-1, Unchecked.defaultof<'TokenType>, "There is no accepting state")
            | Some res -> 
                let tree = new Tree<_>(terminals.ToArray(), nodes.[res], parserSource.Rules)
                tree.AstToDot parserSource.NumToString parserSource.TokenToNumber parserSource.TokenData parserSource.LeftSide "../../../Tests/AbstractRNGLR/DOT/sppf.dot"
                Success <| tree