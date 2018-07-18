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

let buildAstAbstract<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : SimpleInputGraph<'TokenType>) =
    if parserSource.ErrorRulesExists then
        let edges = tokens.Edges |> Seq.map(fun edge -> (edge.Source, edge.Target, edge.Tag)) |> Seq.toList 
        for tokenEdge in edges do
            let source, target, token = tokenEdge
            if parserSource.TokenToNumber token <> parserSource.EofIndex then
                let errorEdge = new ParserEdge<_>(source, target, parserSource.CreateErrorToken token)
                tokens.AddEdge errorEdge |> ignore

    let isErrorToken (token : 'TokenType) = 
        parserSource.ErrorRulesExists && parserSource.TokenToNumber token = parserSource.ErrorIndex 

    let startVList, finalVList, innerGraph =
        let verticesMap = Array.zeroCreate (Seq.max tokens.Vertices + 1)            
        for i in tokens.Vertices do
             verticesMap.[i] <- new VInfo<_> (i)
        let g = new QuickGraph.AdjacencyGraph<_,_>()
        tokens.Edges 
        |> Seq.map (fun e -> new QuickGraph.TaggedEdge<_,_>(verticesMap.[e.Source], verticesMap.[e.Target], e.Tag))
        |> g.AddVerticesAndEdgeRange
        |> ignore
        [ for initS in tokens.InitStates -> verticesMap.[initS] ],
        [ for finalS in tokens.FinalStates -> verticesMap.[finalS] ],
        g        
        //verticesMap.[tokens.InitState], verticesMap.[tokens.FinalState], g
            
    let nodes = new BlockResizeArray<AstNode>()
    let terminals = new BlockResizeArray<'TokenType>()
    let startState = 0
    let inline getEpsilon i = new Epsilon(-1-i)
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let verticesToProcess = new Queue<_>()
    
    for startV in startVList do
        verticesToProcess.Enqueue (startV)
    
    let mutable errorIndex = -1
    let outEdgesInnerGraph = Array.init (Seq.max tokens.Vertices + 1) (fun _ -> [||])
    for v in innerGraph.Vertices do
        outEdgesInnerGraph.[v.vNum] <- innerGraph.OutEdges v |> Array.ofSeq
    
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

    let addNonZeroReduction (gssVertex : Vertex) token gssEdge (innerGraphV : VInfo<_>)=
        let arr = parserSource.Reduces.[gssVertex.State].[parserSource.TokenToNumber token |> int]
        if arr <> null 
        then
            for (prod, pos) in arr do
                innerGraphV.AddReduction(new Reduction(gssVertex, prod, pos, Some gssEdge))

    let addZeroReduction (gssVertex : Vertex) token (innerGraphV : VInfo<_>) shouldEnqueue =
        let arr = parserSource.ZeroReduces.[gssVertex.State].[parserSource.TokenToNumber token |> int]
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
                listToAddUnprocessedGssV.Add v
                for e in outEdgesInnerGraph.[currentGraphV.vNum] do
                    addZeroReduction v e.Tag currentGraphV false
        v, isNew

    let addEdge (startV:VInfo<_>) isNew (newVertex:Vertex) edge isNotEps =
        if (*incomingEdges.[startV.vNum] > 1 &&*) not isNew && newVertex.PassingReductions.Count > 0
        then startV.passingReductions.Add((newVertex, edge))
        customEnqueue(startV)
        newVertex.addEdge edge
        if isNotEps
        then
            for e in outEdgesInnerGraph.[startV.vNum] do
                addNonZeroReduction newVertex e.Tag edge startV

    let edgesToTerms = new Dictionary<_,_>()
    let push (currentGraphV:VInfo<_>) (gssVertex : Vertex) =
        let newUnprocessedGssVs = new ResizeArray<_>(2)
        for e in outEdgesInnerGraph.[currentGraphV.vNum] do
            let push = parserSource.Gotos.[gssVertex.State].[parserSource.TokenToNumber e.Tag |> int]
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
                then addEdge e.Target isNew tailGssV edge true
               

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

    let handlePath (path : Path) (final : Vertex) startV nonTerm pos prod shouldEnqueueVertex =
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
                addEdge  startV isNew newVertex edge (pos > 0)
                edge.Ast
            | x -> (newVertex.Edge x).Ast 
        if ast >= 0 
        then addChildren nodes.[ast] path prod
        else 
            let edge = new Edge(final, nodes.Length)
            nodes.Add <| new AST (Unchecked.defaultof<_>, null)
            addEdge  startV isNew newVertex edge (pos > 0)
            addChildren nodes.[nodes.Length - 1] path prod

    let rec walk remainLength (vertex : Vertex) path startV nonTerm pos prod shouldEnqueueVertex = 
        if remainLength = 0 
        then handlePath path vertex startV nonTerm pos prod shouldEnqueueVertex
        else
            //if (*incomingEdges.[vertex.Level] > 1 &&*) not (vertex.PassingReductions.Contains((prod, remainLength, path, nonTerm, pos, startV)))
            vertex.PassingReductions.Add ((prod, remainLength, path, nonTerm, pos, startV)) |> ignore
            vertex.OutEdges |> ResizeArray.iter
                (fun e ->
                    let newPath = path.AddEdge(if e.Ast < 0 then new Epsilon(e.Ast) :> AstNode else nodes.[e.Ast])
                    walk (remainLength - 1) e.Dest newPath startV nonTerm pos prod shouldEnqueueVertex)

    let makeSingleReduction currentGraphV (reduction : Reduction) =
        let nonTerm = parserSource.LeftSide.[reduction.prod]

        if reduction.pos = 0 then
            let state = parserSource.Gotos.[reduction.gssVertex.State].[nonTerm]
            let newVertex, isNew = addVertex currentGraphV state currentGraphV.unprocessedGssVertices
            if newVertex.FindIndex reduction.gssVertex.State reduction.gssVertex.Level = -1 
            then
                let edge = new Edge(reduction.gssVertex, -parserSource.LeftSide.[reduction.prod]-1)
                addEdge currentGraphV isNew newVertex edge false
        else 
            let path = new Path(nodes.[reduction.edge.Value.Ast])
            walk (reduction.pos - 1) (reduction.edge.Value : Edge).Dest path currentGraphV nonTerm reduction.pos reduction.prod false

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
                walk (remainLength - 1) e.Dest newPath startV nonTerm pos prod true
            graphV.passingReductions.Remove((v, e)) |> ignore

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
        then new Tree<_>([||], getEpsilon startNonTerm, parserSource.Rules) |> Success
        else Error (0, Unchecked.defaultof<'TokenType>, "This grammar cannot accept empty string")
    else
        for startV in startVList do
            addVertex startV startState startV.unprocessedGssVertices |> ignore 

        while errorIndex = -1 && verticesToProcess.Count > 0 do
            let curV = verticesToProcess.Dequeue()
            processVertex curV

        if errorIndex <> -1 then
            Error (errorIndex - 1, Unchecked.defaultof<'TokenType>, "Parse error")
        else
            let root = ref None
            let addTreeTop resList =
                let children = resList |> Array.map (fun x -> new Family(parserSource.StartRule, new Nodes([|x|])))
                new AST(children)

            let vertices = innerGraph.Edges |> Seq.filter (fun e -> List.exists (fun f -> f = e.Target) finalVList) |> Seq.collect (fun e -> e.Source.processedGssVertices)
            
            let rootList = new BlockResizeArray<_>()
            for v in vertices do
                if parserSource.AccStates.[v.State]
                then
                    let rL = v.OutEdges 
                                    |> Seq.filter (fun e -> e.Dest.State = startState && e.Ast >= 0 && e <> Unchecked.defaultof<_>) 
                                    |> Seq.map (fun e -> e.Ast)
                    for ast in rL do
                        rootList.Add nodes.[ast]
//                        
            //assert (rootList.Length = 1) // single acc state

            root := Some nodes.Length
            rootList.ToArray() |> addTreeTop |> nodes.Add

//            for ast in rootList do
//                root := Some nodes.Length
//                [|ast|]
//                |> addTreeTop
//                |> nodes.Add

//            // old
//            for v in vertices do
//                if parserSource.AccStates.[v.State]
//                then
//                    root := Some nodes.Length //!
//                    let nonEpsilonEdge = v.OutEdges.FirstOrDefault(fun x -> x.Ast >= 0)
//                    if nonEpsilonEdge <> Unchecked.defaultof<_>
//                    then
//                        [|nodes.[nonEpsilonEdge.Ast]|]
//                        |> addTreeTop
//                        |> nodes.Add

            match !root with
            | None -> 
                let states = 
                    innerGraph.Vertices 
                    |> Seq.filter (fun v -> innerGraph.OutEdges(v) 
                                            |> Seq.exists(fun e -> e.Target.processedGssVertices.Count = 0 (*&& e.Target.unprocessedGssVertices.Count = 0*)
                                                                   && v.processedGssVertices.Count <> 0 (*&& v.unprocessedGssVertices.Count <> 0*)))
                    |> Seq.map (fun v -> string v.vNum)
                    |> String.concat "; "
                Error (-1, Unchecked.defaultof<'TokenType>, "There is no accepting state. Possible errors: (" + states + ")")
            | Some res -> 
                try 
                    let tree = new Tree<_>(terminals.ToArray(), nodes.[res], parserSource.Rules, Some parserSource.LeftSide, Some parserSource.NumToString, isErrorToken = isErrorToken)
                    try 
                        tree.AstToDot parserSource.NumToString parserSource.TokenToNumber parserSource.TokenData parserSource.LeftSide "./data/AbstractRNGLR/DOT/sppf.dot"
                    with 
                        e -> printfn "Could not print AST: %s" e.Message
//
//                    let gssInitVertices = 
//                    innerGraph.Edges |> Seq.filter (fun e -> List.exists (fun f -> f = e.Target) finalVList) |> Seq.collect (fun e -> e.Source.processedGssVertices)
//
//                    drawDot parserSource.TokenToNumber terminals parserSource.LeftSide gssInitVertices parserSource.NumToString parserSource.ErrorIndex "../../../Tests/AbstractRNGLR/DOT/gss.dot"

                    Success <| tree
                with
                e -> Error (-1, Unchecked.defaultof<'TokenType>, e.Message)
////  Parser.fs contains methods, needed to build an AST
////
////  Copyright 2011-2012 Avdyukhin Dmitry
////
////  This file is part of YaccConctructor.
////
////  YaccConstructor is free software:you can redistribute it and/or modify
////  it under the terms of the GNU General Public License as published by
////  the Free Software Foundation, either version 3 of the License, or
////  (at your option) any later version.
////
////  This program is distributed in the hope that it will be useful,
////  but WITHOUT ANY WARRANTY; without even the implied warranty of
////  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
////  GNU General Public License for more details.
////
////  You should have received a copy of the GNU General Public License
////  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

//module Yard.Generators.ARNGLR.Parser
//
//open AbstractAnalysis.Common
//open Microsoft.FSharp.Collections
//open System.Collections.Generic
//open System.Linq
//open Yard.Generators.Common.AST
//open Yard.Generators.Common.AstNode
//open Yard.Generators.Common.DataStructures
//open Yard.Generators.RNGLR
//open FSharpx.Collections.Experimental
//
//type ParseResult<'TokenType> =
//    | Success of Tree<'TokenType>
//    | Error of int * 'TokenType * string
//
//[<AllowNullLiteral>]
//type Vertex (state : int, level : int) =
//    let out = new ResizeArray<Edge>(4)
//    let passingReductions = new HashSet<_>()
//    member this.Level = level
//    member this.OutEdges = out
//    member this.State = state
//    member this.PassingReductions = passingReductions
//
//    member this.addEdge (edge : Edge) =
//        let mutable i = out.Count - 1
//        out.Add edge
//        while i >= 0 && (out.[i].Dest : Vertex).Level < edge.Dest.Level do
//            out.[i+1] <- out.[i]
//            i <- i - 1
//        out.[i+1] <- edge
//
//    member this.FindIndex state level =
//        let mutable i = out.Count - 1
//        while i >= 0 && out.[i].Dest.Level <= level && (out.[i].Dest.Level <> level || out.[i].Dest.State <> state) do
//            i <- i - 1
//        if i >= 0 && out.[i].Dest.Level = level && out.[i].Dest.State = state 
//        then i
//        else -1
//
//    member this.Edge i = out.[i]
//
//and Edge (destination : Vertex, ast : int) =
//    member this.Dest = destination
//    member this.Ast = ast
//
//and VInfo<'TokenType> (vNum) =
//    member val vNum = vNum with get
//    member val processedGssVertices = new ResizeArray<Vertex>() with get
//    member val unprocessedGssVertices = new ResizeArray<Vertex>() with get
//    member val passingReductions = new ResizeArray<Vertex * Edge>() with get
//    member val reductions = new Queue<_> (10) with get
//
//    member this.GetReduction() = 
//        this.reductions.Dequeue()
//
//    member this.AddReduction(r) =
//        if not <| this.reductions.Contains(r)
//        then this.reductions.Enqueue(r)
//
//and [<Struct>]
//    Reduction =
//    val gssVertex : Vertex
//    val prod : int
//    val pos : int
//    val edge : Edge option
//    new (_gssVertex, _prod, _pos, _edge) = {gssVertex = _gssVertex; prod = _prod; pos = _pos; edge = _edge}
//
//
//and [<Struct>]
//    Reduction2<'TokenType> =    
//    val prod : int
//    val remainLength: int
//    val path : list<AstNode>
//    val nonTerm: int
//    val pos : int    
//    new (_prod, _remainLength, _path, _nonTerm, _pos) = {prod = _prod; remainLength = _remainLength; path = _path; nonTerm = _nonTerm; pos = _pos}
//    
//let buildAstAbstract<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : SimpleInputGraph<'TokenType>) =    
//    
//    let incomingEdges = new Dictionary<_,_>()
//    tokens.Vertices |> Seq.iter(fun v -> incomingEdges.Add(v,0))
//    tokens.Edges
//    |> Seq.iter (fun e -> incomingEdges.[e.Target] <- incomingEdges.[e.Target] + 1)
//
//    let startV, finalV, innerGraph =
//        let verticesMap = Array.zeroCreate (Seq.max tokens.Vertices + 1)            
//        for i in tokens.Vertices do
//             verticesMap.[i] <- new VInfo<_> (i)
//        let g = new QuickGraph.AdjacencyGraph<_,_>()
//        tokens.Edges 
//        |> Seq.map (fun e -> new QuickGraph.TaggedEdge<_,_>(verticesMap.[e.Source], verticesMap.[e.Target], e.Tag))
//        |> g.AddVerticesAndEdgeRange
//        |> ignore
//        verticesMap.[tokens.InitState], verticesMap.[tokens.FinalState], g
//    
//    let nodes = new BlockResizeArray<AstNode>()
//    let terminals = new BlockResizeArray<'TokenType>()
//    let startState = 0
//    let inline getEpsilon i = new Epsilon(-1-i)
//    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
//    let verticesToProcess = new Queue<_>()
//    verticesToProcess.Enqueue (startV)
//    let mutable errorIndex = -1
//    let outEdgesInnerGraph = Array.init (Seq.max tokens.Vertices + 1) (fun _ -> [||])
//    for v in innerGraph.Vertices do
//        outEdgesInnerGraph.[v.vNum] <- innerGraph.OutEdges v |> Array.ofSeq
//    
//    let drawDot (tokenToNumber : _ -> int) (tokens : BlockResizeArray<_>) (leftSide : int[])
//        (initNodes : seq<Vertex>) (numToString : int -> string) (errInd: int) (path : string) =
//        use out = new System.IO.StreamWriter (path)
//        let was = new Dictionary<_,_>()
//        let levels = new Dictionary<_,_>()
//        out.WriteLine "digraph GSS {"
//        let print s = out.WriteLine ("    " + s)
//        let curNum = ref 0
//        print "rankdir=RL"
//        let getAstString astInd =
//            if astInd >= 0 
//            then
//                match nodes.[astInd] with
//                | :? Terminal as i -> tokens.[i.TokenNumber] |> tokenToNumber |> numToString |> sprintf "%s"    
//                | :? AST as ast -> 
//                    let nonT = 
//                        if ast.first.prod < leftSide.Length then ast.first.prod
//                        else errInd
//                    numToString leftSide.[nonT]
//                | _ -> failwith "Unexpected ast"
//            else "eps"
//
//        let rec dfs (u : Vertex) =
//            was.Add (u, !curNum)
//            if not <| levels.ContainsKey u.Level then
//                levels.[u.Level] <- [!curNum]
//            else
//                levels.[u.Level] <- !curNum :: levels.[u.Level]
//            print <| sprintf "%d [label=\"%d_%d\"]" !curNum u.Level u.State 
//            incr curNum
//            u.OutEdges |> ResizeArray.iter (handleEdge u)
//            
//
//        and handleEdge u (e : Edge) =
//            let v = e.Dest
//            if not <| was.ContainsKey v then
//                dfs v
//            print <| sprintf "%d -> %d [label=\"%s\"]" was.[u] was.[v] (getAstString e.Ast)
//
//        for v in initNodes do
//            if not <| was.ContainsKey v then
//                dfs v
//        
//        for level in levels do
//            print <| sprintf "{rank=same; %s}" (level.Value |> List.map (fun (u : int) -> string u) |> String.concat " ")
//
//        out.WriteLine "}"
//        out.Close()
//    
//    let customEnqueue (elem : VInfo<_>) =
//        if verticesToProcess.Count = 0 || ((verticesToProcess.ElementAt (verticesToProcess.Count - 1)).vNum = elem.vNum |> not)
//        then
//            verticesToProcess.Enqueue(elem)
//
//    let addNonZeroReduction (gssVertex : Vertex) token gssEdge (innerGraphV : VInfo<_>)=
//        let arr = parserSource.Reduces.[gssVertex.State].[parserSource.TokenToNumber token]
//        if arr <> null 
//        then
//            for (prod, pos) in arr do
//                innerGraphV.AddReduction(new Reduction(gssVertex, prod, pos, Some gssEdge))
//
//    let addZeroReduction (gssVertex : Vertex) token (innerGraphV : VInfo<_>) shouldEnqueue =
//        let arr = parserSource.ZeroReduces.[gssVertex.State].[parserSource.TokenToNumber token]
//        if arr <> null
//        then
//            for prod in arr do
//                innerGraphV.AddReduction(new Reduction(gssVertex, prod, 0, None))
//    
//    let inline addVertex (currentGraphV:VInfo<_>) state (listToAddUnprocessedGssV : ResizeArray<_>) =
//        let mutable v = null
//        let mutable isNew = false
//        let vOpt = currentGraphV.processedGssVertices |> ResizeArray.tryFind (fun v -> v.State = state)
//        match vOpt with
//        | Some x -> v <- x
//        | None ->
//            let vOpt = currentGraphV.unprocessedGssVertices |> ResizeArray.tryFind (fun v -> v.State = state)
//            match vOpt with
//            | Some x -> v <- x
//            | None ->
//                v <- new Vertex(state, currentGraphV.vNum)
//                isNew <- true
//                listToAddUnprocessedGssV.Add v
//                for e in outEdgesInnerGraph.[currentGraphV.vNum] do
//                    addZeroReduction v e.Tag currentGraphV false
//        v, isNew
//
//    let addEdge (startV:VInfo<_>) isNew (newVertex:Vertex) edge isNotEps =
//        if (*incomingEdges.[startV.vNum] > 1 &&*) not isNew && newVertex.PassingReductions.Count > 0
//        then startV.passingReductions.Add((newVertex, edge))
//        customEnqueue startV
//        newVertex.addEdge edge
//        if isNotEps
//        then
//            for e in outEdgesInnerGraph.[startV.vNum] do
//                addNonZeroReduction newVertex e.Tag edge startV
//
//    let edgesToTerms = new Dictionary<_,_>()
//    let push (currentGraphV:VInfo<_>) (gssVertex : Vertex) =
//        let newUnprocessedGssVs = new ResizeArray<_>(2)
//        for e in outEdgesInnerGraph.[currentGraphV.vNum] do
//            let push = parserSource.Gotos.[gssVertex.State].[parserSource.TokenToNumber e.Tag]
//            if push <> 0 
//            then
//                let tailGssV, isNew = addVertex e.Target push (if currentGraphV.vNum = e.Target.vNum then newUnprocessedGssVs else e.Target.unprocessedGssVertices)
//
//                if not <| edgesToTerms.ContainsKey e
//                then
//                    terminals.Add e.Tag
//                    nodes.Add <| Terminal (terminals.Length - 1)
//                    edgesToTerms.Add(e, nodes.Length - 1)
//                let edge = new Edge(gssVertex, edgesToTerms.[e])
//
//                let ind = tailGssV.FindIndex gssVertex.State gssVertex.Level 
//                if ind = -1 || (tailGssV.Edge ind).Ast <> edgesToTerms.[e]
//                then addEdge e.Target isNew tailGssV edge true
//               
//        if not <| currentGraphV.processedGssVertices.Contains(gssVertex)
//        then 
//            currentGraphV.processedGssVertices.Add(gssVertex)
//        newUnprocessedGssVs
//
//    let verticesToRecalc = new ResizeArray<_> (10)
//
//    let inline comparePaths (p1:list<_>) (p2:Nodes) =
//        let compare (x : AstNode) (y : AstNode) = 
//            match x with
//            | :? Epsilon as x' -> match y with | :? Epsilon as y' -> x'.EpsilonNonTerm = y'.EpsilonNonTerm | _ -> false
//            | :? Terminal as x' -> match y with | :? Terminal as y' -> x'.TokenNumber = y'.TokenNumber | _ -> false
//            | :? AST as xast -> match y with | :? AST as yast -> 
//                                                    xast.pos = yast.pos 
//                                                    && xast = yast 
//                                             | _ -> false
//            | _ -> false
//        p1.Length = p2.Length
//        && let h = ref p1 in p2.isForAll(fun x -> let res = compare x h.Value.Head
//                                                  h := h.Value.Tail
//                                                  res )
//
//    let inline addChildren node path prod =
//        let ast = getFamily node
//        let familyOpt = 
//            ast.findFamily (function family -> family.prod = prod && (comparePaths path family.nodes))
//        match familyOpt with 
//        | None -> 
//            let newFamily = new Family (prod, new Nodes(path |> List.toArray))
//            if ast.first = Unchecked.defaultof<_> 
//            then ast.first <- newFamily 
//            else
//                if ast.other = null 
//                then ast.other <- [|newFamily|]
//                else ast.other <- Array.append ast.other [|newFamily|]
//        | Some _ -> ()
//
//    let inline handlePath path (final : Vertex) startV nonTerm pos prod shouldEnqueueVertex =
//        let state = parserSource.Gotos.[final.State].[nonTerm]
//        let newVertex, isNew = addVertex startV state startV.unprocessedGssVertices
//        if shouldEnqueueVertex && isNew 
//        then verticesToRecalc.Add startV
//        let ast = 
//            match newVertex.FindIndex final.State final.Level with
//            | -1 -> 
//                let edge = new Edge(final, nodes.Length)
//                nodes.Add <| new AST (Unchecked.defaultof<_>, null) 
//                addEdge startV isNew newVertex edge (pos > 0)
//                edge.Ast
//            | x -> (newVertex.Edge x).Ast 
//        if ast >= 0 
//        then addChildren nodes.[ast] path prod
//        else 
//            let edge = new Edge(final, nodes.Length)
//            nodes.Add <| new AST (Unchecked.defaultof<_>, null)
//            addEdge  startV isNew newVertex edge (pos > 0)
//            addChildren nodes.[nodes.Length - 1] path prod
//
//    let rec walk remainLength (vertex : Vertex) path startV nonTerm pos prod shouldEnqueueVertex = 
//        if remainLength = 0 
//        then handlePath path vertex startV nonTerm pos prod shouldEnqueueVertex
//        else
//            let newReduction = new Reduction2<_>(prod, remainLength, path, nonTerm, pos)            
//            vertex.PassingReductions.Add (newReduction) |> ignore
//            vertex.OutEdges |> ResizeArray.iter
//                (fun e ->
//                    let newPath = (if e.Ast < 0 then new Epsilon(e.Ast) :> AstNode else nodes.[e.Ast])::path
//                    walk (remainLength - 1) e.Dest newPath startV nonTerm pos prod shouldEnqueueVertex)
//
//    let makeSingleReduction currentGraphV (reduction : Reduction) =
//        let nonTerm = parserSource.LeftSide.[reduction.prod]
//
//        if reduction.pos = 0 then
//            let state = parserSource.Gotos.[reduction.gssVertex.State].[nonTerm]
//            let newVertex, isNew = addVertex currentGraphV state currentGraphV.unprocessedGssVertices
//            if newVertex.FindIndex reduction.gssVertex.State reduction.gssVertex.Level = -1 
//            then
//                let edge = new Edge(reduction.gssVertex, -parserSource.LeftSide.[reduction.prod]-1)
//                addEdge currentGraphV isNew newVertex edge false
//        else 
//            let path = [nodes.[reduction.edge.Value.Ast]]
//            walk (reduction.pos - 1) (reduction.edge.Value : Edge).Dest path currentGraphV nonTerm reduction.pos reduction.prod false
//
//    let makeReductions (currentGraphV : VInfo<_>) =
//        while currentGraphV.reductions.Count > 0 do
//            let r = currentGraphV.GetReduction()
//            makeSingleReduction currentGraphV r
//
//
//    let handlePassingReductions (graphV : VInfo<_>) =
//        let copyPR = ResizeArray.copy(graphV.passingReductions)
//        for (v, e) in copyPR do
//            let passingReductions = v.PassingReductions
//            for p in passingReductions do
//                let newPath = (if e.Ast < 0 then new Epsilon(e.Ast) :> AstNode else nodes.[e.Ast])::p.path
//                walk (p.remainLength - 1) e.Dest newPath startV p.nonTerm p.pos p.prod true
//            graphV.passingReductions.Remove((v, e)) |> ignore
//
//    let processVertex v = 
//        makeReductions v
//
//        let newGssVs = new ResizeArray<_>(2)
//        for gssVertex in v.unprocessedGssVertices do 
//            newGssVs.AddRange(push v gssVertex)
//        v.unprocessedGssVertices.Clear()
//        if newGssVs.Count > 0 
//        then
//            v.unprocessedGssVertices.AddRange(newGssVs)
//
//        if v.passingReductions.Count > 0
//        then
//            handlePassingReductions v
//
//    if tokens.EdgeCount = 0
//    then
//        if parserSource.AcceptEmptyInput
//        then new Tree<_>([||], getEpsilon startNonTerm, parserSource.Rules) |> Success
//        else Error (0, Unchecked.defaultof<'TokenType>, "This grammar cannot accept empty string")
//    else
//        let _,_ = addVertex startV startState startV.unprocessedGssVertices
//        while errorIndex = -1 && verticesToProcess.Count > 0 do
//            let curV = verticesToProcess.Dequeue()
//            processVertex curV
//
//        if errorIndex <> -1 then
//            Error (errorIndex - 1, Unchecked.defaultof<'TokenType>, "Parse error")
//        else
//            let root = ref None
//            let addTreeTop res =
//                let children = new Family(parserSource.StartRule, new Nodes([|res|]))
//                new AST(children, null)
//            for v in innerGraph.Edges |> Seq.filter (fun e -> e.Target = finalV) |> Seq.collect (fun e -> e.Source.processedGssVertices) do
//                if parserSource.AccStates.[v.State]
//                then
//                    root := Some nodes.Length
//                    let nonEpsilonEdge = v.OutEdges.FirstOrDefault(fun x -> x.Ast >= 0)
//                    if nonEpsilonEdge <> Unchecked.defaultof<_>
//                    then
//                        nodes.[nonEpsilonEdge.Ast]
//                        |> addTreeTop
//                        |> nodes.Add
//            match !root with
//            | None -> 
//                let states = 
//                    innerGraph.Vertices 
//                    |> Seq.filter (fun v -> innerGraph.OutEdges(v) 
//                                            |> Seq.exists(fun e -> e.Target.processedGssVertices.Count = 0 (*&& e.Target.unprocessedGssVertices.Count = 0*)
//                                                                   && v.processedGssVertices.Count <> 0 (*&& v.unprocessedGssVertices.Count <> 0*)))
//                    |> Seq.map (fun v -> string v.vNum)
//                    |> String.concat "; "
//                Error (-1, Unchecked.defaultof<'TokenType>, "There is no accepting state. Possible errors: (" + states + ")")
//            | Some res -> 
//                try 
//                    let tree = new Tree<_>(terminals.ToArray(), nodes.[res], parserSource.Rules, Some parserSource.LeftSide, Some parserSource.NumToString)
//                    tree.AstToDot parserSource.NumToString parserSource.TokenToNumber parserSource.TokenData parserSource.LeftSide "../../../Tests/AbstractRNGLR/DOT/sppf.dot"
//
//                    let gssInitVertices = 
//                       innerGraph.Edges |> Seq.collect (fun e -> e.Source.processedGssVertices)
//
//                    drawDot parserSource.TokenToNumber terminals parserSource.LeftSide gssInitVertices parserSource.NumToString parserSource.ErrorIndex "../../../Tests/AbstractRNGLR/DOT/gss.dot"
//                    
//                    (*let orderedNodes = tree.Order
//                    printfn "O = %A" orderedNodes.Length
//                    orderedNodes
//                    |> Array.iter (fun n -> 
//                        n)*)
//                    Success <| tree
//                with
//                e -> Error (-1, Unchecked.defaultof<'TokenType>, e.Message)