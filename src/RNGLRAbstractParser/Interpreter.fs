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
open FSharpx.Collections.Experimental


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
        if i >= 0 && out.[i].Dest.Level = level && out.[i].Dest.State = state 
        then i
        else -1

    member this.Edge i = out.[i]

and Edge (destination : Vertex, ast : int) =
    member this.Dest = destination
    member this.Ast = ast

and VInfo<'TokenType> (vNum) =
    member val vNum = vNum with get
    member val processedGssVertices = new ResizeArray<Vertex>() with get
    member val unprocessedGssVertices = new ResizeArray<Vertex>() with get
    member val passingReductions = new ResizeArray<Vertex * Edge>() with get
    member val reductions = new Queue<_> (10) with get

    member this.GetReduction() = 
        this.reductions.Dequeue()

    member this.AddReduction(r) =
        if not <| this.reductions.Contains(r)
        then this.reductions.Enqueue(r)

and [<Struct>]
    Reduction =
    val gssVertex : Vertex
    val prod : int
    val pos : int
    val edge : Edge option
    new (_gssVertex, _prod, _pos, _edge) = {gssVertex = _gssVertex; prod = _prod; pos = _pos; edge = _edge}

and [<AllowNullLiteral>]
    Prefix<'TokenType> (head : QuickGraph.TaggedEdge<_,'TokenType>, tail : ResizeArray<Prefix<_>>) = 

    member this.Head = head
    member this.Tail = tail

    member this.AddEdge edge =
        new Prefix<_>(edge, this)

    new (head, prefix : Prefix<_>) =
        let curLevel = new ResizeArray<Prefix<_>>()
        curLevel.Add(prefix)
        new Prefix<_> (head, curLevel)

    new (edge) = new Prefix<_> (edge, new ResizeArray<Prefix<_>>())

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

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType> * Dictionary<string,ResizeArray<Prefix<'TokenType>>>
    | Error of Dictionary<string,ResizeArray<Prefix<'TokenType>>>


(*let buildAstAbstractOnline<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : ParserInputGraph<'TokenType>) =
    let startV, finalV, innerGraph =
        let verticesMap = Array.zeroCreate (Seq.max tokens.Vertices + 1)            
        for i in tokens.Vertices do
             verticesMap.[i] <- new VInfo<_> (i)
        let g = new QuickGraph.AdjacencyGraph<_,_>()
        tokens.Edges 
        |> Seq.map (fun e -> new QuickGraph.TaggedEdge<_,_>(verticesMap.[e.Source], verticesMap.[e.Target], e.Tag))
        |> g.AddVerticesAndEdgeRange
        |> ignore
        verticesMap.[tokens.InitState], verticesMap.[tokens.FinalState], g
    
    let nodes = new BlockResizeArray<AstNode>()
    let terminals = new BlockResizeArray<'TokenType>()
    let startState = 0
    let (*inline*) getEpsilon i = new Epsilon(-1-i)
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let verticesToProcess = new Queue<_>()
    verticesToProcess.Enqueue (startV)
    let mutable errorIndex = -1
    let outEdgesInnerGraph = Array.init (Seq.max tokens.Vertices + 1) (fun _ -> [||])
    for v in innerGraph.Vertices do
        outEdgesInnerGraph.[v.vNum] <- innerGraph.OutEdges v |> Array.ofSeq
    let gssVertexesToPrefixes = new Dictionary<_,_>()
    let passedPrefixes = new Dictionary<_,ResizeArray<Prefix<'TokenType>>>()
    let notPassedPrefixes = new Dictionary<_,ResizeArray<Prefix<'TokenType>>>()
    for v in innerGraph.Vertices do
        for e in outEdgesInnerGraph.[v.vNum] do
            passedPrefixes.Add(e, new ResizeArray<Prefix<'TokenType>>())
            notPassedPrefixes.Add(e, new ResizeArray<Prefix<'TokenType>>())
    let errorEdges = new Dictionary<QuickGraph.TaggedEdge<_,_>,_>()
    
    let drawDot (tokenToNumber : _ -> int) (tokens : BlockResizeArray<_>) (leftSide : int[])
        (initNodes : seq<Vertex>) (numToString : int -> string) (errInd: int) (path : string) =
        use out = new System.IO.StreamWriter (path)
        use outErrors = new System.IO.StreamWriter ("../../../Tests/AbstractRNGLR/DOT/errors.txt")
        for e in errorEdges.Keys do
            let tokenStr = tokenToNumber e.Tag |> parserSource.NumToString
            let tokenData = match parserSource.TokenData with | None -> "" | Some f -> f e.Tag|> string
            outErrors.WriteLine (tokenStr + tokenData)
        outErrors.Close()
        let was = new Dictionary<_,_>()
        let levels = new Dictionary<_,_>()
        out.WriteLine "digraph GSS {"
        let print s = out.WriteLine ("    " + s)
        let curNum = ref 0
        print "rankdir=RL"
        let getAstString astInd =
            if astInd >= 0 
            then
                match nodes.[astInd] with
                | :? Terminal as i -> tokens.[i.TokenNumber] |> tokenToNumber |> numToString |> sprintf "%s"    
                | :? AST as ast -> 
                    let nonT = 
                        if ast.first.prod < leftSide.Length then ast.first.prod
                        else errInd
                    numToString leftSide.[nonT]
                | _ -> failwith "Unexpected ast"
            else "eps"

        let rec dfs (u : Vertex) =
            was.Add (u, !curNum)
            if not <| levels.ContainsKey u.Level then
                levels.[u.Level] <- [!curNum]
            else
                levels.[u.Level] <- !curNum :: levels.[u.Level]
            (*let rec printPrefixes (prefixes:ResizeArray<Prefix<_>>) (prevStr : string) =
                let mutable curStr = prevStr + "["
                if ResizeArray.isEmpty(prefixes) then curStr + "]"
                else
                    for prefix in prefixes do
                        if prefix = null
                        then
                            curStr <- curStr + "NULL, "
                        else
                            let tokenStr = tokenToNumber prefix.Head.Tag |> parserSource.NumToString
                            let tokenData = match parserSource.TokenData with | None -> "" | Some f -> f prefix.Head.Tag|> string
                            curStr <- curStr + tokenStr + tokenData + ":"
                            curStr <- printPrefixes prefix.Tail curStr + ", "
                    curStr + "]"
                        
            print <| sprintf "%d [label=\"%d_%d_%s\"]" !curNum u.Level u.State (printPrefixes gssVertexesToPrefixes.[u] "Prefixes:")*)
            print <| sprintf "%d [label=\"%d_%d\"]" !curNum u.Level u.State 
            incr curNum
            u.OutEdges |> ResizeArray.iter (handleEdge u)
            

        and handleEdge u (e : Edge) =
            let v = e.Dest
            if not <| was.ContainsKey v then
                dfs v
            print <| sprintf "%d -> %d [label=\"%s\"]" was.[u] was.[v] (getAstString e.Ast)

        for v in initNodes do
            if not <| was.ContainsKey v then
                dfs v
        
        for level in levels do
            print <| sprintf "{rank=same; %s}" (level.Value |> List.map (fun (u : int) -> string u) |> String.concat " ")

        out.WriteLine "}"
        out.Close()
    
    let customEnqueue (elem : VInfo<_>) =
        if verticesToProcess.Count = 0 || ((verticesToProcess.ElementAt (verticesToProcess.Count - 1)).vNum = elem.vNum |> not)
        then
            verticesToProcess.Enqueue(elem)

    let addNonZeroReduction (gssVertex : Vertex) token gssEdge (innerGraphV : VInfo<_>) =
        let arr = parserSource.Reduces.[gssVertex.State].[parserSource.TokenToNumber token]
        if arr <> null 
        then
            for (prod, pos) in arr do
                innerGraphV.AddReduction(new Reduction(gssVertex, prod, pos, Some gssEdge))

    let addZeroReduction (gssVertex : Vertex) token (innerGraphV : VInfo<_>) shouldEnqueue =
        let arr = parserSource.ZeroReduces.[gssVertex.State].[parserSource.TokenToNumber token]
        if arr <> null
        then
            for prod in arr do
                innerGraphV.AddReduction(new Reduction(gssVertex, prod, 0, None))
    
    let (*inline*) addVertex (currentGraphV:VInfo<_>) state (listToAddUnprocessedGssV : ResizeArray<_>) =
        let mutable v = null
        let mutable isNew = false
        let vOpt = currentGraphV.processedGssVertices |> ResizeArray.tryFind (fun v -> v.State = state)
        match vOpt with
        | Some x -> v <- x
        | None ->
            let vOpt = currentGraphV.unprocessedGssVertices |> ResizeArray.tryFind (fun v -> v.State = state)
            match vOpt with
            | Some x -> v <- x
            | None ->
                v <- new Vertex(state, currentGraphV.vNum)
                isNew <- true
                gssVertexesToPrefixes.Add(v, new ResizeArray<Prefix<_>>())
                listToAddUnprocessedGssV.Add v
                for e in outEdgesInnerGraph.[currentGraphV.vNum] do
                    addZeroReduction v e.Tag currentGraphV false
        v, isNew

    let extPrefix (oldPrefixes:ResizeArray<Prefix<_>>) edge =
        new Prefix<_>(edge, oldPrefixes)

    let rec addPrefix (curLevel:ResizeArray<Prefix<_>>) (prefixToAdd:Prefix<_>) = 
        if prefixToAdd = null
        then
            if not (ResizeArray.exists
                        (fun (prefix:Prefix<_>) -> prefix = null)
                        curLevel)
            then curLevel.Add(null)
        else if not <| curLevel.Contains(prefixToAdd)
        then
            let nextCommon = ResizeArray.tryFind
                                (fun (pr:Prefix<_>) -> pr <> null && pr.Head = prefixToAdd.Head)
                                curLevel
            match nextCommon with
            | Some oldPrefix -> if ResizeArray.isEmpty(prefixToAdd.Tail)
                                then
                                    addPrefix oldPrefix.Tail null
                                else
                                    for newPrefix in prefixToAdd.Tail do
                                        addPrefix oldPrefix.Tail newPrefix
            | None -> curLevel.Add(prefixToAdd)


    let addEdge (startV:VInfo<_>) isNew (newVertex:Vertex) edge isNotEps (prefixesToAdd:ResizeArray<Prefix<_>>) =
        if not isNew && newVertex.PassingReductions.Count > 0
        then startV.passingReductions.Add((newVertex, edge))
        customEnqueue(startV)
        newVertex.addEdge edge
        let prefixes = gssVertexesToPrefixes.[newVertex]
        for prefixToAdd in prefixesToAdd do
            addPrefix prefixes prefixToAdd

        if isNotEps
        then
            for e in outEdgesInnerGraph.[startV.vNum] do
                addNonZeroReduction newVertex e.Tag edge startV

    let edgesToTerms = new Dictionary<_,_>()
    let push (currentGraphV:VInfo<_>) (gssVertex : Vertex) =
        let newUnprocessedGssVs = new ResizeArray<_>(2)
        for e in outEdgesInnerGraph.[currentGraphV.vNum] do
            let push = parserSource.Gotos.[gssVertex.State].[parserSource.TokenToNumber e.Tag]
            if push <> 0 
            then
                for gssPrefix in gssVertexesToPrefixes.[gssVertex] do
                    addPrefix passedPrefixes.[e] gssPrefix
                let tailGssV, isNew = addVertex e.Target push (if currentGraphV.vNum = e.Target.vNum then newUnprocessedGssVs else e.Target.unprocessedGssVertices)

                if not <| edgesToTerms.ContainsKey e
                then
                    terminals.Add e.Tag
                    nodes.Add <| Terminal (terminals.Length - 1)
                    edgesToTerms.Add(e, nodes.Length - 1)
                let edge = new Edge(gssVertex, edgesToTerms.[e])

                let ind = tailGssV.FindIndex gssVertex.State gssVertex.Level 
                if ind = -1 || (tailGssV.Edge ind).Ast <> edgesToTerms.[e]
                then
                    let prefixesToAdd = new ResizeArray<Prefix<_>>()

                    prefixesToAdd.Add(extPrefix gssVertexesToPrefixes.[gssVertex] e)
                    addEdge e.Target isNew tailGssV edge true prefixesToAdd
            else
                for gssPrefix in gssVertexesToPrefixes.[gssVertex] do
                    addPrefix notPassedPrefixes.[e] gssPrefix

        if not <| currentGraphV.processedGssVertices.Contains(gssVertex)
        then 
            currentGraphV.processedGssVertices.Add(gssVertex)
        newUnprocessedGssVs

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

    let handlePath (path : Path) (final : Vertex) startV nonTerm pos prod shouldEnqueueVertex (startGssV : Vertex) =
        let state = parserSource.Gotos.[final.State].[nonTerm]
        let newVertex, isNew = addVertex startV state startV.unprocessedGssVertices
        if shouldEnqueueVertex && isNew 
        then
            verticesToRecalc.Add startV
        let ast = 
            match newVertex.FindIndex final.State final.Level with
            | -1 -> 
                let edge = new Edge(final, nodes.Length)
                nodes.Add <| new AST (Unchecked.defaultof<_>, null)
                addEdge  startV isNew newVertex edge (pos > 0) gssVertexesToPrefixes.[startGssV]
                edge.Ast
            | x -> (newVertex.Edge x).Ast 
        if ast >= 0 
        then addChildren nodes.[ast] path prod
        else 
            let edge = new Edge(final, nodes.Length)
            nodes.Add <| new AST (Unchecked.defaultof<_>, null)
            addEdge  startV isNew newVertex edge (pos > 0) gssVertexesToPrefixes.[startGssV]
            addChildren nodes.[nodes.Length - 1] path prod

    let rec walk remainLength (vertex : Vertex) path startV nonTerm pos prod shouldEnqueueVertex (startGssV : Vertex) = 
        if remainLength = 0 
        then handlePath path vertex startV nonTerm pos prod shouldEnqueueVertex startGssV
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
                    walk (remainLength - 1) e.Dest newPath startV nonTerm pos prod shouldEnqueueVertex startGssV)

    let makeSingleReduction currentGraphV (reduction : Reduction) =
        let nonTerm = parserSource.LeftSide.[reduction.prod]

        if reduction.pos = 0 then
            let state = parserSource.Gotos.[reduction.gssVertex.State].[nonTerm]
            let newVertex, isNew = addVertex currentGraphV state currentGraphV.unprocessedGssVertices
            if newVertex.FindIndex reduction.gssVertex.State reduction.gssVertex.Level = -1 
            then
                let edge = new Edge(reduction.gssVertex, -parserSource.LeftSide.[reduction.prod]-1)
                addEdge currentGraphV isNew newVertex edge false gssVertexesToPrefixes.[reduction.gssVertex]
        else 
            let path = new Path(nodes.[reduction.edge.Value.Ast])
            walk (reduction.pos - 1) (reduction.edge.Value : Edge).Dest path currentGraphV nonTerm reduction.pos reduction.prod false reduction.gssVertex

    let makeReductions (currentGraphV : VInfo<_>) =
        while currentGraphV.reductions.Count > 0 do
            let r = currentGraphV.GetReduction()
            makeSingleReduction currentGraphV r


    let handlePassingReductions (graphV : VInfo<_>) =
        let copyPR = ResizeArray.copy(graphV.passingReductions)
        for (v, e) in copyPR do
            let passingReductions = v.PassingReductions
            for prod, remainLength, path, nonTerm, pos, startV in passingReductions do
                let newPath = path.AddEdge(if e.Ast < 0 then new Epsilon(e.Ast) :> AstNode else nodes.[e.Ast])
                walk (remainLength - 1) e.Dest newPath startV nonTerm pos prod true v
            graphV.passingReductions.Remove((v, e)) |> ignore

    let checkEdges (v:VInfo<_>) =
       for e in outEdgesInnerGraph.[v.vNum] do
            for npprefix in notPassedPrefixes.[e] do
                if not <| passedPrefixes.[e].Contains(npprefix)
                then
                    if not <| errorEdges.ContainsKey(e)
                    then
                        errorEdges.Add(e, new ResizeArray<Prefix<'TokenType>>())
                    addPrefix errorEdges.[e] npprefix
            passedPrefixes.[e].Clear()
            notPassedPrefixes.[e].Clear()

    let buildErrors (errdict:Dictionary<QuickGraph.TaggedEdge<_,_>,ResizeArray<Prefix<'TokenType>>>) =
        let outErrors = new Dictionary<string,ResizeArray<Prefix<'TokenType>>>()
        for e in errdict.Keys do
            let tokenStr = parserSource.TokenToNumber e.Tag |> parserSource.NumToString
            let tokenData = match parserSource.TokenData with | None -> "" | Some f -> f e.Tag|> string
            outErrors.Add(tokenStr + tokenData, errdict.[e])
        outErrors
    let processVertex v =
        makeReductions v

        let newGssVs = new ResizeArray<_>(2)
        for gssVertex in v.unprocessedGssVertices do 
            newGssVs.AddRange(push v gssVertex)
        if not <| ResizeArray.isEmpty(v.unprocessedGssVertices)
        then
            checkEdges v
        v.unprocessedGssVertices.Clear()
        if newGssVs.Count > 0 
        then
            v.unprocessedGssVertices.AddRange(newGssVs)

        if v.passingReductions.Count > 0
        then
            handlePassingReductions v

    if tokens.EdgeCount = 0
    then
        if parserSource.AcceptEmptyInput
        then (new Tree<_>([||], getEpsilon startNonTerm, parserSource.Rules), buildErrors errorEdges) |> Success
        else Error (buildErrors errorEdges)
    else
        let startGssV,_ = addVertex startV startState startV.unprocessedGssVertices
        gssVertexesToPrefixes.[startGssV].Add(null)
        while errorIndex = -1 && verticesToProcess.Count > 0 do
            let curV = verticesToProcess.Dequeue()
            processVertex curV
        
        for e in innerGraph.Edges |> Seq.filter (fun e -> e.Target = finalV) do
            errorEdges.Remove(e) |> ignore
        if errorIndex <> -1 then
            Error (buildErrors errorEdges)
        else
            let root = ref None
            let addTreeTop res =
                let children = new Family(parserSource.StartRule, new Nodes([|res|]))
                new AST(children, null)
            for e in innerGraph.Edges |> Seq.filter (fun e -> e.Target = finalV) do
                for gssVertex in e.Source.processedGssVertices do
                    if parserSource.AccStates.[gssVertex.State]
                    then
                        for gssPrefix in gssVertexesToPrefixes.[gssVertex] do
                            addPrefix passedPrefixes.[e] gssPrefix
                        root := Some nodes.Length
                        let nonEpsilonEdge = gssVertex.OutEdges.FirstOrDefault(fun x -> x.Ast >= 0)
                        if nonEpsilonEdge <> Unchecked.defaultof<_>
                        then
                            nodes.[nonEpsilonEdge.Ast]
                            |> addTreeTop
                            |> nodes.Add
                    else
                        for gssPrefix in gssVertexesToPrefixes.[gssVertex] do
                            addPrefix notPassedPrefixes.[e] gssPrefix
                for npprefix in notPassedPrefixes.[e] do
                    if not <| passedPrefixes.[e].Contains(npprefix)
                    then
                        if not <| errorEdges.ContainsKey(e)
                        then
                            errorEdges.Add(e, new ResizeArray<Prefix<'TokenType>>())
                        addPrefix errorEdges.[e] npprefix
                passedPrefixes.[e].Clear()
                notPassedPrefixes.[e].Clear()

            match !root with
            | None -> 
                let states = 
                    innerGraph.Vertices 
                    |> Seq.filter (fun v -> innerGraph.OutEdges(v) 
                                            |> Seq.exists(fun e -> e.Target.processedGssVertices.Count = 0 (*&& e.Target.unprocessedGssVertices.Count = 0*)
                                                                   && v.processedGssVertices.Count <> 0 (*&& v.unprocessedGssVertices.Count <> 0*)))
                    |> Seq.map (fun v -> string v.vNum)
                    |> String.concat "; "
                Error (buildErrors errorEdges)
            | Some res -> 
                try 
                    let tree = new Tree<_>(terminals.ToArray(), nodes.[res], parserSource.Rules, Some parserSource.LeftSide, Some parserSource.NumToString)
//                    tree.AstToDot parserSource.NumToString parserSource.TokenToNumber parserSource.TokenData parserSource.LeftSide "../../../Tests/AbstractRNGLR/DOT/sppf.dot"
//
//                    let gssInitVertices = 
//                       innerGraph.Edges |> Seq.collect (fun e -> e.Source.processedGssVertices)
//
//                    drawDot parserSource.TokenToNumber terminals parserSource.LeftSide gssInitVertices parserSource.NumToString parserSource.ErrorIndex "../../../Tests/AbstractRNGLR/DOT/gss.dot"

                    Success <| (tree, buildErrors errorEdges)
                with
                e -> Error (buildErrors errorEdges)*)

let buildAstAbstract<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : ParserInputGraph<'TokenType>) =
    let startV, finalV, innerGraph =
        let verticesMap = Array.zeroCreate (Seq.max tokens.Vertices + 1)            
        for i in tokens.Vertices do
             verticesMap.[i] <- new VInfo<_> (i)
        let g = new QuickGraph.AdjacencyGraph<_,_>()
        tokens.Edges 
        |> Seq.map (fun e -> new QuickGraph.TaggedEdge<_,_>(verticesMap.[e.Source], verticesMap.[e.Target], e.Tag))
        |> g.AddVerticesAndEdgeRange
        |> ignore
        verticesMap.[tokens.InitState], verticesMap.[tokens.FinalState], g
    
    let nodes = new BlockResizeArray<AstNode>()
    let terminals = new BlockResizeArray<'TokenType>()
    let startState = 0
    let (*inline*) getEpsilon i = new Epsilon(-1-i)
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let verticesToProcess = new Queue<_>()
    verticesToProcess.Enqueue (startV)
    let mutable errorIndex = -1
    let outEdgesInnerGraph = Array.init (Seq.max tokens.Vertices + 1) (fun _ -> [||])
    for v in innerGraph.Vertices do
        outEdgesInnerGraph.[v.vNum] <- innerGraph.OutEdges v |> Array.ofSeq
    let gssVertexesToPrefixes = new Dictionary<_,_>()
    
    let drawDot (tokenToNumber : _ -> int) (tokens : BlockResizeArray<_>) (leftSide : int[])
        (initNodes : seq<Vertex>) (numToString : int -> string) (errInd: int) (path : string) =
        use out = new System.IO.StreamWriter (path)
        let was = new Dictionary<_,_>()
        let levels = new Dictionary<_,_>()
        out.WriteLine "digraph GSS {"
        let print s = out.WriteLine ("    " + s)
        let curNum = ref 0
        print "rankdir=RL"
        let getAstString astInd =
            if astInd >= 0 
            then
                match nodes.[astInd] with
                | :? Terminal as i -> tokens.[i.TokenNumber] |> tokenToNumber |> numToString |> sprintf "%s"    
                | :? AST as ast -> 
                    let nonT = 
                        if ast.first.prod < leftSide.Length then ast.first.prod
                        else errInd
                    numToString leftSide.[nonT]
                | _ -> failwith "Unexpected ast"
            else "eps"

        let rec dfs (u : Vertex) =
            was.Add (u, !curNum)
            if not <| levels.ContainsKey u.Level then
                levels.[u.Level] <- [!curNum]
            else
                levels.[u.Level] <- !curNum :: levels.[u.Level]
            (*let rec printPrefixes (prefixes:ResizeArray<Prefix<_>>) (prevStr : string) =
                let mutable curStr = prevStr + "["
                if ResizeArray.isEmpty(prefixes) then curStr + "]"
                else
                    for prefix in prefixes do
                        if prefix = null
                        then
                            curStr <- curStr + "NULL, "
                        else
                            let tokenStr = tokenToNumber prefix.Head.Tag |> parserSource.NumToString
                            let tokenData = match parserSource.TokenData with | None -> "" | Some f -> f prefix.Head.Tag|> string
                            curStr <- curStr + tokenStr + tokenData + ":"
                            curStr <- printPrefixes prefix.Tail curStr + ", "
                    curStr + "]"
                        
            print <| sprintf "%d [label=\"%d_%d_%s\"]" !curNum u.Level u.State (printPrefixes gssVertexesToPrefixes.[u] "Prefixes:")*)
            print <| sprintf "%d [label=\"%d_%d\"]" !curNum u.Level u.State 
            incr curNum
            u.OutEdges |> ResizeArray.iter (handleEdge u)
            

        and handleEdge u (e : Edge) =
            let v = e.Dest
            if not <| was.ContainsKey v then
                dfs v
            print <| sprintf "%d -> %d [label=\"%s\"]" was.[u] was.[v] (getAstString e.Ast)

        for v in initNodes do
            if not <| was.ContainsKey v then
                dfs v
        
        for level in levels do
            print <| sprintf "{rank=same; %s}" (level.Value |> List.map (fun (u : int) -> string u) |> String.concat " ")

        out.WriteLine "}"
        out.Close()
    
    let customEnqueue (elem : VInfo<_>) =
        if verticesToProcess.Count = 0 || ((verticesToProcess.ElementAt (verticesToProcess.Count - 1)).vNum = elem.vNum |> not)
        then
            verticesToProcess.Enqueue(elem)

    let addNonZeroReduction (gssVertex : Vertex) token gssEdge (innerGraphV : VInfo<_>) =
        let arr = parserSource.Reduces.[gssVertex.State].[parserSource.TokenToNumber token]
        if arr <> null 
        then
            for (prod, pos) in arr do
                innerGraphV.AddReduction(new Reduction(gssVertex, prod, pos, Some gssEdge))

    let addZeroReduction (gssVertex : Vertex) token (innerGraphV : VInfo<_>) shouldEnqueue =
        let arr = parserSource.ZeroReduces.[gssVertex.State].[parserSource.TokenToNumber token]
        if arr <> null
        then
            for prod in arr do
                innerGraphV.AddReduction(new Reduction(gssVertex, prod, 0, None))
    
    let (*inline*) addVertex (currentGraphV:VInfo<_>) state (listToAddUnprocessedGssV : ResizeArray<_>) =
        let mutable v = null
        let mutable isNew = false
        let vOpt = currentGraphV.processedGssVertices |> ResizeArray.tryFind (fun v -> v.State = state)
        match vOpt with
        | Some x -> v <- x
        | None ->
            let vOpt = currentGraphV.unprocessedGssVertices |> ResizeArray.tryFind (fun v -> v.State = state)
            match vOpt with
            | Some x -> v <- x
            | None ->
                v <- new Vertex(state, currentGraphV.vNum)
                isNew <- true
                gssVertexesToPrefixes.Add(v, new ResizeArray<Prefix<_>>())
                listToAddUnprocessedGssV.Add v
                for e in outEdgesInnerGraph.[currentGraphV.vNum] do
                    addZeroReduction v e.Tag currentGraphV false
        v, isNew

    let extPrefix (oldPrefixes:ResizeArray<Prefix<_>>) edge =
        new Prefix<_>(edge, oldPrefixes)

    let rec addPrefix (curLevel:ResizeArray<Prefix<_>>) (prefixToAdd:Prefix<_>) = 
        if prefixToAdd = null
        then
            if not (ResizeArray.exists
                        (fun (prefix:Prefix<_>) -> prefix = null)
                        curLevel)
            then curLevel.Add(null)
        else if not <| curLevel.Contains(prefixToAdd)
        then
            let nextCommon = ResizeArray.tryFind
                                (fun (pr:Prefix<_>) -> pr <> null && pr.Head = prefixToAdd.Head)
                                curLevel
            match nextCommon with
            | Some oldPrefix -> if ResizeArray.isEmpty(prefixToAdd.Tail)
                                then
                                    addPrefix oldPrefix.Tail null
                                else
                                    for newPrefix in prefixToAdd.Tail do
                                        addPrefix oldPrefix.Tail newPrefix
            | None -> curLevel.Add(prefixToAdd)


    let addEdge (startV:VInfo<_>) isNew (newVertex:Vertex) edge isNotEps (prefixesToAdd:ResizeArray<Prefix<_>>) =
        if not isNew && newVertex.PassingReductions.Count > 0
        then startV.passingReductions.Add((newVertex, edge))
        customEnqueue(startV)
        newVertex.addEdge edge
        

        if isNotEps
        then
            let prefixes = gssVertexesToPrefixes.[newVertex]
            for prefixToAdd in prefixesToAdd do
                addPrefix prefixes prefixToAdd
            for e in outEdgesInnerGraph.[startV.vNum] do
                addNonZeroReduction newVertex e.Tag edge startV
        else
            gssVertexesToPrefixes.[newVertex] <- prefixesToAdd

    let edgesToTerms = new Dictionary<_,_>()
    let push (currentGraphV:VInfo<_>) (gssVertex : Vertex) =
        let newUnprocessedGssVs = new ResizeArray<_>(2)
        for e in outEdgesInnerGraph.[currentGraphV.vNum] do
            let push = parserSource.Gotos.[gssVertex.State].[parserSource.TokenToNumber e.Tag]
            if push <> 0 
            then
                let tailGssV, isNew = addVertex e.Target push (if currentGraphV.vNum = e.Target.vNum then newUnprocessedGssVs else e.Target.unprocessedGssVertices)

                if not <| edgesToTerms.ContainsKey e
                then
                    terminals.Add e.Tag
                    nodes.Add <| Terminal (terminals.Length - 1)
                    edgesToTerms.Add(e, nodes.Length - 1)
                let edge = new Edge(gssVertex, edgesToTerms.[e])

                let ind = tailGssV.FindIndex gssVertex.State gssVertex.Level 
                if ind = -1 || (tailGssV.Edge ind).Ast <> edgesToTerms.[e]
                then
                    let prefixesToAdd = new ResizeArray<Prefix<_>>()

                    prefixesToAdd.Add(extPrefix gssVertexesToPrefixes.[gssVertex] e)
                    addEdge e.Target isNew tailGssV edge true prefixesToAdd

        if not <| currentGraphV.processedGssVertices.Contains(gssVertex)
        then 
            currentGraphV.processedGssVertices.Add(gssVertex)
        newUnprocessedGssVs

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

    let handlePath (path : Path) (final : Vertex) startV nonTerm pos prod shouldEnqueueVertex (startGssV : Vertex) =
        let state = parserSource.Gotos.[final.State].[nonTerm]
        let newVertex, isNew = addVertex startV state startV.unprocessedGssVertices
        if shouldEnqueueVertex && isNew 
        then
            verticesToRecalc.Add startV
        let ast = 
            match newVertex.FindIndex final.State final.Level with
            | -1 -> 
                let edge = new Edge(final, nodes.Length)
                nodes.Add <| new AST (Unchecked.defaultof<_>, null)
                addEdge  startV isNew newVertex edge (pos > 0) gssVertexesToPrefixes.[startGssV]
                edge.Ast
            | x -> (newVertex.Edge x).Ast 
        if ast >= 0 
        then addChildren nodes.[ast] path prod
        else 
            let edge = new Edge(final, nodes.Length)
            nodes.Add <| new AST (Unchecked.defaultof<_>, null)
            addEdge  startV isNew newVertex edge (pos > 0) gssVertexesToPrefixes.[startGssV]
            addChildren nodes.[nodes.Length - 1] path prod

    let rec walk remainLength (vertex : Vertex) path startV nonTerm pos prod shouldEnqueueVertex (startGssV : Vertex) = 
        if remainLength = 0 
        then handlePath path vertex startV nonTerm pos prod shouldEnqueueVertex startGssV
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
                    walk (remainLength - 1) e.Dest newPath startV nonTerm pos prod shouldEnqueueVertex startGssV)

    let makeSingleReduction currentGraphV (reduction : Reduction) =
        let nonTerm = parserSource.LeftSide.[reduction.prod]

        if reduction.pos = 0 then
            let state = parserSource.Gotos.[reduction.gssVertex.State].[nonTerm]
            let newVertex, isNew = addVertex currentGraphV state currentGraphV.unprocessedGssVertices
            if newVertex.FindIndex reduction.gssVertex.State reduction.gssVertex.Level = -1 
            then
                let edge = new Edge(reduction.gssVertex, -parserSource.LeftSide.[reduction.prod]-1)
                addEdge currentGraphV isNew newVertex edge false gssVertexesToPrefixes.[reduction.gssVertex]
        else 
            let path = new Path(nodes.[reduction.edge.Value.Ast])
            walk (reduction.pos - 1) (reduction.edge.Value : Edge).Dest path currentGraphV nonTerm reduction.pos reduction.prod false reduction.gssVertex

    let makeReductions (currentGraphV : VInfo<_>) =
        while currentGraphV.reductions.Count > 0 do
            let r = currentGraphV.GetReduction()
            makeSingleReduction currentGraphV r


    let handlePassingReductions (graphV : VInfo<_>) =
        let copyPR = ResizeArray.copy(graphV.passingReductions)
        for (v, e) in copyPR do
            let passingReductions = v.PassingReductions
            for prod, remainLength, path, nonTerm, pos, startV in passingReductions do
                let newPath = path.AddEdge(if e.Ast < 0 then new Epsilon(e.Ast) :> AstNode else nodes.[e.Ast])
                walk (remainLength - 1) e.Dest newPath startV nonTerm pos prod true v
            graphV.passingReductions.Remove((v, e)) |> ignore

    let buildErrors (errdict:Dictionary<QuickGraph.TaggedEdge<_,_>,ResizeArray<Prefix<'TokenType>>>) =
        let outErrors = new Dictionary<string,ResizeArray<Prefix<'TokenType>>>()
        for e in errdict.Keys do
            let tokenStr = parserSource.TokenToNumber e.Tag |> parserSource.NumToString
            let tokenData = match parserSource.TokenData with | None -> "" | Some f -> f e.Tag|> string
            outErrors.Add(tokenStr + tokenData, errdict.[e])
        outErrors

    let collectErrors (starts:ResizeArray<VInfo<_>>)=
        let was = new ResizeArray<_>()
        let vertexesToPrefixes = new Dictionary<_,_>()
        let errorEdges = new Dictionary<QuickGraph.TaggedEdge<_,_>,_>()

        let collectPrefixes (v:VInfo<_>) =
            if not <| vertexesToPrefixes.ContainsKey(v) then
                vertexesToPrefixes.Add(v, new ResizeArray<_>())
                for gssV in v.processedGssVertices do
                    for gssPrefix in gssVertexesToPrefixes.[gssV] do
                        addPrefix vertexesToPrefixes.[v] gssPrefix

        let rec dfs (v:VInfo<_>) =
            was.Add(v)
            collectPrefixes v
            for e in outEdgesInnerGraph.[v.vNum] do
                collectPrefixes e.Target
                let targetPrefixes = vertexesToPrefixes.[e.Target] |> Seq.filter(fun pr -> if pr = null then false else pr.Head = e) |> Seq.collect(fun pr -> pr.Tail)
                for prefix in vertexesToPrefixes.[v] do
                    if not <| targetPrefixes.Contains(prefix) then
                        if not <| errorEdges.ContainsKey(e) then
                            errorEdges.Add(e, new ResizeArray<_>())
                        addPrefix errorEdges.[e] prefix

                if not <| was.Contains(e.Target) then
                    dfs(e.Target)

        for v in starts do
            dfs(v)

        for e in innerGraph.Edges |> Seq.filter (fun e -> e.Target = finalV) do
            errorEdges.Remove(e) |> ignore
            let accPrefixes = new ResizeArray<_>()
            let notAccPrefixes = new ResizeArray<_>()
            for gssVertex in e.Source.processedGssVertices do
                if parserSource.AccStates.[gssVertex.State]
                then
                    for gssPrefix in gssVertexesToPrefixes.[gssVertex] do
                        addPrefix accPrefixes gssPrefix
                else
                    for gssPrefix in gssVertexesToPrefixes.[gssVertex] do
                        addPrefix notAccPrefixes gssPrefix
            for naprefix in notAccPrefixes do
               if not <| accPrefixes.Contains(naprefix)
                then
                    if not <| errorEdges.ContainsKey(e)
                    then
                        errorEdges.Add(e, new ResizeArray<Prefix<'TokenType>>())
                    addPrefix errorEdges.[e] naprefix

        buildErrors errorEdges

    let processVertex v =
        makeReductions v

        let newGssVs = new ResizeArray<_>(2)
        for gssVertex in v.unprocessedGssVertices do 
            newGssVs.AddRange(push v gssVertex)
        v.unprocessedGssVertices.Clear()
        if newGssVs.Count > 0 
        then
            v.unprocessedGssVertices.AddRange(newGssVs)

        if v.passingReductions.Count > 0
        then
            handlePassingReductions v

    if tokens.EdgeCount = 0
    then
        if parserSource.AcceptEmptyInput
        then (new Tree<_>([||], getEpsilon startNonTerm, parserSource.Rules), new Dictionary<_,_>()) |> Success
        else Error (new Dictionary<_,_>())
    else
        let startGssV,_ = addVertex startV startState startV.unprocessedGssVertices
        gssVertexesToPrefixes.[startGssV].Add(null)
        while errorIndex = -1 && verticesToProcess.Count > 0 do
            let curV = verticesToProcess.Dequeue()
            processVertex curV

        let starts = new ResizeArray<VInfo<_>>()
        starts.Add(startV)
        let errorsDict = collectErrors starts

        if errorIndex <> -1 then
            Error (errorsDict)
        else
            let root = ref None
            let addTreeTop res =
                let children = new Family(parserSource.StartRule, new Nodes([|res|]))
                new AST(children, null)
            for e in innerGraph.Edges |> Seq.filter (fun e -> e.Target = finalV) do
                for gssVertex in e.Source.processedGssVertices do
                    if parserSource.AccStates.[gssVertex.State]
                    then
                        root := Some nodes.Length
                        let nonEpsilonEdge = gssVertex.OutEdges.FirstOrDefault(fun x -> x.Ast >= 0)
                        if nonEpsilonEdge <> Unchecked.defaultof<_>
                        then
                            nodes.[nonEpsilonEdge.Ast]
                            |> addTreeTop
                            |> nodes.Add

            match !root with
            | None -> 
                let states = 
                    innerGraph.Vertices 
                    |> Seq.filter (fun v -> innerGraph.OutEdges(v) 
                                            |> Seq.exists(fun e -> e.Target.processedGssVertices.Count = 0 (*&& e.Target.unprocessedGssVertices.Count = 0*)
                                                                   && v.processedGssVertices.Count <> 0 (*&& v.unprocessedGssVertices.Count <> 0*)))
                    |> Seq.map (fun v -> string v.vNum)
                    |> String.concat "; "
                Error (errorsDict)
            | Some res -> 
                try 
                    let tree = new Tree<_>(terminals.ToArray(), nodes.[res], parserSource.Rules, Some parserSource.LeftSide, Some parserSource.NumToString)
//                    tree.AstToDot parserSource.NumToString parserSource.TokenToNumber parserSource.TokenData parserSource.LeftSide "../../../Tests/AbstractRNGLR/DOT/sppf.dot"
//
//                    let gssInitVertices = 
//                       innerGraph.Edges |> Seq.collect (fun e -> e.Source.processedGssVertices)
//
//                    drawDot parserSource.TokenToNumber terminals parserSource.LeftSide gssInitVertices parserSource.NumToString parserSource.ErrorIndex "../../../Tests/AbstractRNGLR/DOT/gss.dot"

                    Success <| (tree, errorsDict)
                with
                e -> Error (errorsDict)