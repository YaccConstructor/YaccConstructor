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

module Yard.Generators.RNGLR.EBNF.DFA.Parser

open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode
open System.Collections.Generic
open Yard.Generators.Common.DataStructures
open Yard.Generators.RNGLR.EBNF.DFA
open Microsoft.FSharp.Collections
open FSharpx.Collections.Experimental
open Yard.Generators.Common

// Custom graph structure. For optimization and needed (by algorithm) relation with AST

[<AllowNullLiteral>]
type Vertex  =
    val mutable OutEdges : UsualOne<Edge>
    /// Number of token, processed when the vertex was created
    val Level : int
    /// Usual LALR state
    val State : int
    new (state, level) = {OutEdges = Unchecked.defaultof<_>; State = state; Level = level}

and Edge =
    struct
        /// AST on the edge
        val Ast : AstNode
        /// End of the vertex (begin is not needed)
        val Dest : Vertex
        new (d,a) = {Dest = d; Ast = a}
    end

type ParserDebugFuns<'TokenType> = {
    drawGSSDot : string -> unit
    /// If you need more then one last token
    lastTokens : int -> 'TokenType[]
}

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType> * array<'TokenType> * Dictionary<Family, ErrorNode<'TokenType>>
    | Error of int * array<'TokenType> * string * ParserDebugFuns<'TokenType> * Dictionary<Family, ErrorNode<'TokenType>>

/// Compare vertex like a pair: (level, state)
let inline private less (v' : Vertex) (v : Vertex) = v'.Level < v.Level || (v'.Level = v.Level && v'.State < v.State)
let inline private eq (v' : Vertex) (v : Vertex) = v'.Level = v.Level && v'.State = v.State

/// Add edges, what must be unique (after shift or epsilon-edges).
/// All edges are sorted by destination ascending.
let private addSimpleEdge (v : Vertex) (ast : AstNode) (out : ResizeArray<Vertex * AstNode>) =
    let mutable i = out.Count - 1
    while i >= 0 && less (fst out.[i]) v do
        i <- i - 1
    out.Insert (i+1, (v, ast))

/// Check if edge with specified destination and AST already exists
let private containsSimpleEdge (v : Vertex) (f : AstNode) (out : ResizeArray<Vertex * AstNode>) =
    let mutable i = out.Count - 1
    while i >= 0 && less (fst out.[i]) v do
        i <- i - 1
    while i >= 0 && (let v',f' = out.[i] in eq v' v && f <> f') do
        i <- i - 1
    i >= 0 && (let v',f' = out.[i] in eq v' v && f = f')

/// Add or extend edge with specified destination and family.
/// All edges are sorted by destination ascending.
let private addEdge (v : Vertex) (family : Family) (out : ResizeArray<Vertex * Family * AST>) isError =
    let mutable i = out.Count - 1
    let inline fst3 (x,_,_) = x
    let inline snd3 (_,x,_) = x
    while i >= 0 && less (fst3 out.[i]) v do
        i <- i - 1

    let isCreated = not (i >= 0 && eq (fst3 out.[i]) v)

    let ast = 
        if isError
        then new AST(family, null)
        else 
            if not isCreated 
            then let _,_,n = out.[i] in n
            else new AST (family, null)
    let newVal = v, family, ast
    if isCreated || family.prod = (snd3 out.[i]).prod then
        out.Insert (i+1, newVal)
    elif family.prod < (snd3 out.[i]).prod then
        out.[i] <- newVal
        let mutable j = i-1
        while j >= 0 && eq (fst3 out.[j])  (fst3 out.[i]) do
            j <- j-1

        out.RemoveRange(j+1, i-j-1)
    isCreated, ast

/// Check if edge with specified destination and family already exists
let private containsEdge (v : Vertex) (f : Family) (out : ResizeArray<Vertex * Family * AST>) =
    let inline fst3 (x,_,_) = x
    let mutable i = out.Count - 1
    while i >= 0 && less (fst3 out.[i]) v do
        i <- i - 1
    while i >= 0 && (let v',f',_ = out.[i] in eq v' v && f <> f') do
        i <- i - 1
    i >= 0 && (let v',f',_ = out.[i] in eq v' v && f = f')

let drawDot (tokenToNumber : _ -> int) (tokens : BlockResizeArray<_>) (leftSide : int[])
        (initNodes : seq<Vertex>) (numToString : int -> string) (errInd: int) (path : string) =
    use out = new System.IO.StreamWriter (path)
    let was = new Dictionary<_,_>()
    let levels = new Dictionary<_,_>()
    out.WriteLine "digraph GSS {"
    let print s = out.WriteLine ("    " + s)
    let curNum = ref 0
    print "rankdir=RL"
    let getAstString (ast : AstNode) =
        match ast with
        | :? Terminal as i -> tokens.[i.TokenNumber] |> tokenToNumber |> numToString |> sprintf "%s"    
        | :? Epsilon as i -> "eps " + numToString (-i.EpsilonNonTerm-1)
        | :? AST as ast -> 
            let nonT = 
                if ast.first.prod < leftSide.Length then ast.first.prod
                else errInd
            numToString leftSide.[nonT]
        | _ -> failwith "Unexpected ast"

    let rec dfs (u : Vertex) =
        was.Add (u, !curNum)
        if not <| levels.ContainsKey u.Level then
            levels.[u.Level] <- [!curNum]
        else
            levels.[u.Level] <- !curNum :: levels.[u.Level]
        print <| sprintf "%d [label=\"%d\"]" !curNum u.State
        incr curNum
        if u.OutEdges.first <> Unchecked.defaultof<_> then
            handleEdge u u.OutEdges.first
            if u.OutEdges.other <> null then
                u.OutEdges.other |> Array.iter (handleEdge u)

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

let buildAstReadBack<'TokenType> (parserSource : ParserSourceReadBack<'TokenType>) (tokens : seq<'TokenType>) =
    let enum = tokens.GetEnumerator()
    // Change if it doesn't equal to zero. Now it's true according to states building algorithm
    let startState = 0
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let nonTermsCountLimit = 1 + (Array.max parserSource.LeftSide)
    let getEpsilon =
        let epsilons = Array.init nonTermsCountLimit (fun i -> new Epsilon(-i-1))
        fun i -> epsilons.[i]
    /// info about errors
    let errDict = new Dictionary<Family, ErrorNode<_>>()
                              
    // If input stream is empty or consists only of RNGLR_EOF token
    if not <| enum.MoveNext() || parserSource.EofIndex = parserSource.TokenToNumber enum.Current then
        if parserSource.AcceptEmptyInput 
        then
            Success (new Tree<_>(null, (getEpsilon startNonTerm) :> AstNode, null), [||], errDict)
        else
            Error (0, [||], "This grammar cannot accept empty string",
                    {
                        drawGSSDot = fun _ -> ()
                        lastTokens = fun _ -> [||]
                    }, 
                    errDict)
    else                                     
        // Currently processed token
        let curToken = ref enum.Current
        let curNum = ref (parserSource.TokenToNumber enum.Current)
        /// Here all tokens from the input will be collected
        let tokens = new BlockResizeArray<_>()
        let reductions = new Stack<_>(10)
        let statesCount = parserSource.Gotos.Length
        // New edges can be created only from last level.
        /// Temporary storage for edges data (after all reductions real edges will be created).
        let edges = Array.init statesCount (fun _ -> new ResizeArray<Vertex * Family * AST>())
        let simpleEdges = Array.init statesCount (fun _ -> new ResizeArray<Vertex * AstNode>())

        let pushes = new Stack<_> (statesCount * 2 + 10)
        /// Stores states, used on current level. Instead statesCount must be number of non-terminals, but doesn't matter
        let usedStates = new Stack<_>(statesCount)
        let stateToVertex : Vertex[] = Array.zeroCreate statesCount
        //DEBUG
        let totalWalkProd = Array.zeroCreate parserSource.LeftSide.Length

        let addVertex state level (edgeOpt : option<Vertex * AstNode>) =
            let dict = stateToVertex
            if dict.[state] = null then
                let v = new Vertex(state, level)
                dict.[state] <- v
                let push = parserSource.Gotos.[state].[!curNum]
                // Push to init state is impossible
                if push <> 0 then
                    pushes.Push (v, push)
                let arr = parserSource.ZeroReduces.[state].[!curNum]
                if arr <> null then
                    for prod in arr do
                        reductions.Push (v, prod, -1, None)
                usedStates.Push state
            let v = dict.[state]
            if edgeOpt.IsSome && not (isEpsilon <| snd edgeOpt.Value) then 
                let arr = parserSource.Reduces.[state].[!curNum]
                if arr <> null then
                    for (prod, pos) in arr do
                        reductions.Push (v, prod, pos, edgeOpt)
            v

        ignore <| addVertex startState 0 None
        let inline trd (_,_,x) = x
        let makeReductions num recovery =
            while reductions.Count > 0 do
                let vertex, prod, pos, edgeOpt = reductions.Pop()
                let nonTerm = parserSource.LeftSide.[prod]

                if pos = -1 then
                    let state = parserSource.Gotos.[vertex.State].[nonTerm]
                    // goto init state is impossible
                    if state <> startState then
                        let newVertex = addVertex state num None
                        let ast = getEpsilon parserSource.LeftSide.[prod]
                        if not <| containsSimpleEdge vertex ast simpleEdges.[state] then
                            addSimpleEdge vertex ast simpleEdges.[state]
                else 
                    //Debug
                    printfn "    %s \t\t\t\tfrom lr %d (pos %d) to lr %d" (parserSource.LeftSide.[prod] |> parserSource.NumToString) vertex.State pos (fst edgeOpt.Value).State
                    let readBackAutoma, finishStates = parserSource.Dfas.[prod]
                    let rightEnd = readBackAutoma.[pos]
                    
                    let matchSymbolAndGssEdgeAst (symbol : int) (ast : AstNode) =
                        match parserSource.IndexToSymbolType symbol, ast with
                        | SymbolType.Nonterminal, (:? AstNode.AST as x) ->
                            symbol = parserSource.LeftSide.[x.first.prod]
                        | SymbolType.Nonterminal, (:? AstNode.Epsilon as x) ->
                            symbol = - (x.EpsilonNonTerm + 1)
                        | SymbolType.Terminal, (:? AstNode.Terminal as x) ->
                            symbol = parserSource.TokenToNumber tokens.[x.TokenNumber]
                        | _ -> false

                    let handlePath (path : AstNode[]) (final : Vertex) =
                        if final = null
                        then recovery()//pushes.Clear()
                        else
                            let state = parserSource.Gotos.[final.State].[nonTerm]
                            // if path.Length = 0, then it's zero-reduction
                            // goto init state is impossible
                            if path.Length > 0 && state <> startState then
                            
                                let newVertex = addVertex state num None
                    
                                let family = new Family(prod, new Nodes(path))
                                if not <| containsEdge final family edges.[state] then
                                    let isCreated, edgeLabel = addEdge final family edges.[state] false
                                    if pos >= 0 && isCreated && not (isEpsilon edgeLabel) then
                                        //Debug
                                        printfn "    \t handled from (%d, %d) to %d" final.Level final.State state
                                        let arr = parserSource.Reduces.[state].[!curNum]
                                        let edgeOpt = Some (final, edgeLabel :> AstNode)
                                        if arr <> null then
                                            for (prod, pos) in arr do
                                                reductions.Push (newVertex, prod, pos, edgeOpt)

                    let rec walk (readBackVertex : Vertex<_,_>) (vertex : Vertex) path =
                        //Debug
                        totalWalkProd.[prod] <- totalWalkProd.[prod] + 1
                        if finishStates.Contains readBackVertex.label then handlePath (path |> List.rev |> List.toArray) vertex
                        for readBackEdge in readBackVertex.outEdges do
                            if vertex.Level <> num then
                                if vertex.OutEdges.other <> null then
                                    for gssEdge in vertex.OutEdges.other do
                                        if matchSymbolAndGssEdgeAst readBackEdge.label gssEdge.Ast then
                                            walk readBackEdge.dest gssEdge.Dest (gssEdge.Ast::path)
                                if matchSymbolAndGssEdgeAst readBackEdge.label vertex.OutEdges.first.Ast then
                                    walk readBackEdge.dest vertex.OutEdges.first.Dest (vertex.OutEdges.first.Ast::path)
                            else
                                simpleEdges.[vertex.State] |> ResizeArray.iter(fun (v,a) ->
                                        //path.[remainLength - 1] <- a
                                        if matchSymbolAndGssEdgeAst readBackEdge.label a then
                                            walk readBackEdge.dest v (a::path))
                            
                                let mutable i = 0
                                let edges = edges.[vertex.State]
                                let mutable count = 0
                                while i < edges.Count do
                                    let (v,_,a) = edges.[i]
                                    if matchSymbolAndGssEdgeAst readBackEdge.label a then
                                        let mutable j = i+1
                                        walk readBackEdge.dest v (a :> AstNode :: path)
                                        while j < edges.Count && trd edges.[j] = a do
                                            j <- j + 1
                                        i <- j
                        //else recovery() //pushes.Clear()
                    for edge in rightEnd.outEdges do
                        if matchSymbolAndGssEdgeAst edge.label (snd edgeOpt.Value) then
                            walk edge.dest (fst edgeOpt.Value) [snd edgeOpt.Value]

        let curInd = ref 0
        let isEnd = ref false
        let attachEdges () =
            let inline snd3 (_,x,_) = x
            for vertex in usedStates do
                let mutable i = 0
                let edges = edges.[vertex]
                let mutable count = -1
                while i < edges.Count do
                    let k = trd edges.[i]
                    let mutable j = i+1
                    while j < edges.Count && trd edges.[j] = k do
                        j <- j + 1
                    i <- j
                    count <- count + 1
                count <- count + simpleEdges.[vertex].Count
                let vEdges =
                    if count > 0 then Array.zeroCreate count
                    else null
                let mutable first = Unchecked.defaultof<_>
                i <- 0
                count <- -1
                while i < edges.Count do
                    let (v,_,a) = edges.[i]
                    let mutable j = i+1
                    while j < edges.Count && trd edges.[j] = a do
                        j <- j + 1
                    let other = 
                        if j <> i + 1 then
                            let res = Array.zeroCreate (j - i - 1)
                            for k = i + 1 to j-1 do
                                res.[k-i-1] <- snd3 edges.[k]
                            res
                        else
                            null
                    if count >= 0 then
                        vEdges.[count] <- new Edge(v, a)
                    else
                        first <- new Edge(v, a)
                    count <- count + 1
                    a.first <- snd3 edges.[i]
                    a.other <- other
                    i <- j

                for i = 0 to simpleEdges.[vertex].Count - 1 do
                    let v, a = simpleEdges.[vertex].[i]
                    if count >= 0 then
                        vEdges.[count] <- new Edge(v, a)
                    else
                        first <- new Edge(v, a)
                    count <- count + 1

                stateToVertex.[vertex].OutEdges <- UsualOne<_>(first, vEdges)
                edges.Clear()
                simpleEdges.[vertex].Clear()

        let shift num =
            let newAstNode = AstNode.Terminal(tokens.Length)
            tokens.Add enum.Current
            if enum.MoveNext() then
                curToken := enum.Current
                curNum := parserSource.TokenToNumber enum.Current
                //debug
                printfn "%d  | %d" num !curNum
            else
                curNum := parserSource.EofIndex
            for vertex in usedStates do
                stateToVertex.[vertex] <- null
            (*usedStates.Clear()
            let oldPushes = pushes.ToArray()
            pushes.Clear()*)
            let oldPushes = new Stack<_>()
            for vertex, state in pushes do
                if vertex.State |> usedStates.Contains 
                then 
                    oldPushes.Push (vertex, state)
            pushes.Clear()
            usedStates.Clear()

            for (vertex, state) in oldPushes do
                let newVertex = addVertex state num <| Some (vertex, newAstNode :> AstNode)
                addSimpleEdge vertex newAstNode simpleEdges.[state]
        
        /// returns all the terminals and non-terminals that make the push or reduce
        /// it's used by recovery
        let getExpectedTokens state = 
            let expected = ref <| Set.empty
            let ps = parserSource

            for i = 0 to ps.Gotos.[0].GetLength(0)-1 do
                if ps.Gotos.[state].[i] <> 0 || ps.Reduces.[state].[i] <> null
                then expected := expected.Value.Add i
            
            !expected
        
        /// returns  array that consists of tokens or error non-teminal (and its children)
        let astToTokens (x : AstNode) =            
            let visited = ref 0
            let res = new ResizeArray<_>()
            let rec go (x : AstNode) =                
                incr visited
                if !visited < 100 
                then                    
                    match x  with 
                    | :? Terminal as t -> res.Add x
                    | :? AST as ast ->
                        if ast.other <> null 
                        then
                            for family in ast.other do
                                if family.prod = parserSource.LeftSide.Length
                                then res.Add ast
                                else family.nodes.doForAll go
                            
                        if ast.first.prod = parserSource.LeftSide.Length
                        then res.Add ast
                        else ast.first.nodes.doForAll go
                    | _ -> ()
            go x
            res |> List.ofSeq
        
        /// collects info about error that is needed in the translation
        let createErrorNode (errFamily : Family) errOn (prod : int) (expected : int[]) (recToks : int[]) = 

            let exp = expected |> Array.map (fun i -> parserSource.NumToString i)
            let recToks = recToks |> Array.map (fun i -> parserSource.NumToString i)
            
            try errDict.Add (errFamily, new ErrorNode<'TokenType> (errOn, -1, exp, recToks))
            with _ -> ()

        let containsRecState (oldVertices : Stack<Vertex * _ list>)(temp : Queue<_>) recVertNum recovery =
            let oldVert = oldVertices.ToArray()
            for vertex, path in oldVert do
                if pushes.Count <> recVertNum
                then
                    //if reduce is possible
                    let arr = parserSource.Reduces.[vertex.State].[!curNum]
                    if arr <> null
                    then 
                        if  not (isEpsilon vertex.OutEdges.first.Ast) then
                            let edgeOpt = Some (vertex.OutEdges.first.Dest, vertex.OutEdges.first.Ast)
                            for (prod, pos) in arr do
                                reductions.Push (vertex, prod, pos, edgeOpt)
                        makeReductions !curInd recovery
                        temp.Enqueue path
                    //if shift is possible
                    let push = parserSource.Gotos.[vertex.State].[!curNum]
                    if push <> 0 
                    then
                        if pushes.Count <> 0 
                        then
                            let recVert = fst <| pushes.Peek()
                            if  recVert.State <> vertex.State 
                            then 
                                pushes.Push (vertex, push)
                                temp.Enqueue path
                        else 
                            pushes.Push (vertex, push)
                            temp.Enqueue path

            pushes.Count + reductions.Count >= recVertNum

        ///returns all the vertices from the previous level
        let getPrevVertices (curVertices : Stack<Vertex * _>) = 
            let inline isOldVertex (v : Vertex) = 
                curVertices.ToArray() 
                |> Array.exists (fun (x, y) -> x.Level = v.Level && x.State = v.State) 
                    
            let oldVertices = curVertices.ToArray()
            curVertices.Clear()
            for vertex, path in oldVertices do
                if vertex.OutEdges.first.Dest <> null 
                then
                    if vertex.OutEdges.other <> null 
                    then
                        for edge in vertex.OutEdges.other do 
                            if  not <| isOldVertex edge.Dest
                            then
                                let tmp = astToTokens edge.Ast
                                let newPath = tmp @ path 
                                curVertices.Push (edge.Dest, newPath)

                    if  not <| isOldVertex vertex.OutEdges.first.Dest
                    then
                        let tmp = astToTokens vertex.OutEdges.first.Ast
                        let newPath = tmp @ path 
                        curVertices.Push (vertex.OutEdges.first.Dest, newPath)
            curVertices

        /// creates a family of the unbrowsed tokens
        (*let createErrorFam (unbrowsed : AstNode[])  = 
            let reduceToError (vertex : Vertex) state (unbrowsed : AstNode[])= 
                let prodNumber = parserSource.Rules.Length
                if unbrowsed.Length = 0 
                then 
                    let ast = getEpsilon parserSource.ErrorIndex
                    if not <| containsSimpleEdge vertex ast simpleEdges.[state] 
                    then
                        addSimpleEdge vertex ast simpleEdges.[state]
                        let arr = parserSource.Reduces.[state].[!curNum]
                        if arr <> null  && not (isEpsilon ast)
                        then
                            let edgeOpt = Some (vertex, ast :> AstNode)
                            for (prod, pos) in arr do
                                reductions.Push (vertex, prod, pos, edgeOpt)
                    new Family(prodNumber, new Nodes([|ast|]))
                else
                    let family = new Family(prodNumber, new Nodes(unbrowsed))
                            
                    if not <| containsEdge vertex family edges.[state] 
                    then
                        let _, edgeLabel = addEdge vertex family edges.[state] true
                        let arr = parserSource.Reduces.[state].[!curNum]
                        if arr <> null && not (isEpsilon edgeLabel)
                        then
                            let edgeOpt = Some (vertex, edgeLabel :> AstNode)
                            for (prod, pos) in arr do
                                reductions.Push (vertex, prod, pos, edgeOpt)
                    family
                           
            let state = snd <| pushes.Peek()

            if parserSource.Reduces.[state].[!curNum] <> null
            then // reductions is possible
                let vertex = fst <| pushes.Peek()
                reduceToError vertex state unbrowsed
                                            
            else // if shift is possible
                let oldPushes = pushes.ToArray()
                for state in usedStates do
                    stateToVertex.[state] <- null
                pushes.Clear()
                usedStates.Clear()
                let fam = ref <| new Family ()
                for vertex, state in oldPushes do
                    fam := reduceToError vertex state unbrowsed

                    let astNode = box tokens.Length
                    tokens.Add !curToken
                    addVertex state !curInd None |> ignore
                !fam*)

        let recovery() =
            (*let recVertNumber = 1
            if usedStates.Count <> 0 
            then 
                let prevNum = !curNum
                let expected = ref Set.empty
                let errInd = !curInd

                curNum := parserSource.ErrorIndex
                let temp = new Queue<_>()
                let curVertices = ref <| new Stack<_> (statesCount)

                for vertex in usedStates do
                    expected := !expected + getExpectedTokens vertex
                    let path = []
                    curVertices.Value.Push (stateToVertex.[vertex], path)
                    stateToVertex.[vertex] <- null
                usedStates.Clear()
                //pushes.count may be equal to 1
                //if parser finished in the non-accepting state and it generates the recovery
                pushes.Clear()

                while curVertices.Value.Count <> 0 && not <| containsRecState !curVertices temp recVertNumber (fun () -> ()) do
                    curVertices := getPrevVertices !curVertices
                
                let skipped = Queue<_>()
                let oldPushes = pushes.ToArray() |> Array.rev
                pushes.Clear()

                let arr : array<Vertex * int * array<int>> = Array.zeroCreate oldPushes.Length
                    
                for i in 0..oldPushes.Length-1 do
                    let vertex, state = oldPushes.[i]
                    let recToks = Set.toArray <| getExpectedTokens state      
                    arr.[i] <- vertex, state, recToks
                
                let inline fst3 (s, _, _) = s
                let inline snd3 (_, s, _) = s
                let inline thr  (_, _, s) = s

                let isRecToken num = 
                    let res = ref -1
                    for i in 0..arr.Length-1 do
                        if !res < 0 && Array.exists ((=) num) <| thr arr.[i] 
                        then res := i
                    !res

                curNum := prevNum
                let var = ref <| isRecToken !curNum

                while !var = -1 && !curNum <> parserSource.EofIndex do
                    let newAstNode = new Terminal(tokens.Length)
                    tokens.Add !curToken
                    skipped.Enqueue (newAstNode :> AstNode)
                    if enum.MoveNext() 
                    then
                        curToken := enum.Current
                        curNum := parserSource.TokenToNumber enum.Current
                        incr curInd
                    else             
                        curNum := parserSource.EofIndex
                    var := isRecToken !curNum
                
                if  !var >= 0
                then 
                    let path = temp.ToArray()
                    let need = List.toArray path.[!var]
                    let unbrowsed = Array.append need <| skipped.ToArray()
                    pushes.Push (fst3 arr.[!var], snd3 arr.[!var])
                    let fam = createErrorFam unbrowsed
                    let rec onTok i =
                        if i >= 0
                        then
                            let x = tokens.[i]
                            if box x <> null
                            then Some x
                            else onTok (i-1)
                        else None
                            
                    createErrorNode fam <| onTok errInd <| 0 <| Set.toArray !expected <| thr arr.[!var]*)
            ()

        //let errorRuleExist = parserSource.ErrorRulesExists
        let wasError = ref false

        while not !isEnd && not !wasError do
            if usedStates.Count = 0 && reductions.Count = 0 
            then 
                wasError := true
            else
                //Debug
                printfn "%d %d | %s" !curInd !curNum (!curNum |> parserSource.NumToString)
                makeReductions !curInd recovery
                attachEdges()
                if !curNum = parserSource.EofIndex then
                    //Debug
                    let totalWalk = totalWalkProd |> Array.fold (+) 0
                    let maxWalk = totalWalkProd |> Array.max
                    let maxWalkNonterm = parserSource.LeftSide.[totalWalkProd |> Array.findIndex ((=) maxWalk)]
                    System.Console.WriteLine(sprintf "%d, %d walks, maxWalkNonterm: %d" !curInd totalWalk maxWalkNonterm)
                    System.Console.ReadLine() |> ignore
                    isEnd := true
                elif pushes.Count = 0 then 
                    (*if errorRuleExist 
                    then recovery()
                    else*) wasError := true
                else
                    incr curInd
                    shift !curInd
        
        let isAcceptState() =                
            usedStates.ToArray()
            |> Array.exists (fun state -> parserSource.AccStates.[state])

        // if finish isn't accepting state then error
        if !isEnd && usedStates.Count > 0 && not <| isAcceptState() 
        then
            (*if errorRuleExist 
            then 
                 recovery()
                 makeReductions (!curInd + 1) recovery
                 attachEdges()
            else*) wasError := true

        let lastTokens count =
            [| for i = max 0 (tokens.Length-count) to tokens.Length-1 do
                yield tokens.[i]|]
        let debugFuns () =
            let vertices = usedStates.ToArray() |> Array.map (fun i -> stateToVertex.[i])
            {
                drawGSSDot = drawDot parserSource.TokenToNumber tokens parserSource.LeftSide vertices parserSource.NumToString parserSource.ErrorIndex
                lastTokens = lastTokens
            }
        
        if !wasError 
        then 
            Error (!curInd , [|!curToken|] , "Parse Error", debugFuns (), errDict)
        else
            let root = ref None
            let addTreeTop res =
                let children = new Family(parserSource.StartRule,  new Nodes(res, null, null))
                new AST(children, null)
            for vertex in usedStates do
                if parserSource.AccStates.[vertex] 
                then
                    root :=
                        stateToVertex.[vertex].OutEdges.first.Ast
                        |> addTreeTop
                        |> Some
            match !root with
            | None -> Error (!curInd, [|!curToken|], "Input was fully processed, but it's not complete correct string.", debugFuns (), errDict)
            | Some res -> 
                debugFuns().drawGSSDot "res.dot"
                let tree = new Tree<_> (tokens.ToArray(), res :> AstNode, [||])
                //tree.AstToDot parserSource.NumToString parserSource.TokenToNumber None parserSource.LeftSide "../../../Tests/RNGLR/sppf.dot"
                Success (tree, [||], errDict)