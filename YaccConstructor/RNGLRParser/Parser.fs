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
        val Ast : obj
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
    | Success of Tree<'TokenType>
    | Error of int * 'TokenType * string * ParserDebugFuns<'TokenType>


/// Compare vertex like a pair: (level, state)
let inline private less (v' : Vertex) (v : Vertex) = v'.Level < v.Level || (v'.Level = v.Level && v'.State < v.State)
let inline private eq (v' : Vertex) (v : Vertex) = v'.Level = v.Level && v'.State = v.State

/// Add edges, what must be unique (after shift or epsilon-edges).
/// All edges are sorted by destination ascending.
let private addSimpleEdge (v : Vertex) (ast : obj) (out : ResizeArray<Vertex * obj>) =
    let mutable i = out.Count - 1
    while i >= 0 && less (fst out.[i]) v do
        i <- i - 1
    out.Insert (i+1, (v, ast))

/// Check if edge with specified destination and AST already exists
let private containsSimpleEdge (v : Vertex) (f : obj) (out : ResizeArray<Vertex * obj>) =
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
            else new AST (Unchecked.defaultof<_>, null)
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
    let getAstString (ast : obj) =
        match ast with
        | :? int as i when i >= 0 -> tokens.[i] |> tokenToNumber |> numToString |> sprintf "%s"    
        | :? int as i when i < 0 -> "eps " + numToString (-i-1)
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


let buildAst<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : seq<'TokenType>) =
    let enum = tokens.GetEnumerator()
    // Change if it doesn't equal to zero. Now it's true according to states building algorithm
    let startState = 0
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let nonTermsCountLimit = 1 + (Array.max parserSource.LeftSide)
    let getEpsilon =
        let epsilons = Array.init nonTermsCountLimit (fun i -> box (-i-1))
        fun i -> epsilons.[i]
                          
    // If input stream is empty or consists only of EOF token
    if not <| enum.MoveNext() || parserSource.EofIndex = parserSource.TokenToNumber enum.Current then
        if parserSource.AcceptEmptyInput then
            Success <| new Tree<_>(null, getEpsilon startNonTerm, null)
        else
            Error (0, Unchecked.defaultof<'TokenType>, "This grammar cannot accept empty string",
                    {
                        drawGSSDot = fun _ -> ()
                        lastTokens = fun _ -> [||]
                    })
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
        let simpleEdges = Array.init statesCount (fun _ -> new ResizeArray<Vertex * obj>())

        let pushes = new Stack<_> (statesCount * 2 + 10)
        /// Stores states, used on current level. Instead statesCount must be number of non-terminals, but doesn't matter
        let usedStates = new Stack<_>(statesCount)
        let stateToVertex : Vertex[] = Array.zeroCreate statesCount

        //let newRules = new BlockResizeArray<_>()

        let addVertex state level (edgeOpt : option<Vertex * obj>) =
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
                        reductions.Push (v, prod, 0, None)
                usedStates.Push state
            let v = dict.[state]
            if edgeOpt.IsSome then 
                let arr = parserSource.Reduces.[state].[!curNum]
                if arr <> null then
                    for (prod, pos) in arr do
                        reductions.Push (v, prod, pos, edgeOpt)
            v

        ignore <| addVertex startState 0 None
        let inline trd (_,_,x) = x
        let makeReductions num =
            while reductions.Count > 0 do
                let vertex, prod, pos, edgeOpt = reductions.Pop()
                let nonTerm = parserSource.LeftSide.[prod]

                let handlePath (path : obj[]) (final : Vertex) =
                    let state = parserSource.Gotos.[final.State].[nonTerm]
                    let newVertex = addVertex state num None
                    
                    let family = new Family(prod, new Nodes(Array.copy path))
                    if not <| containsEdge final family edges.[state] then
                        //printfn "%d %d %d %d" state family.prod final.Level final.State
                        let isCreated, edgeLabel = addEdge final family edges.[state] false
                        if (pos > 0 && isCreated) then
                            let arr = parserSource.Reduces.[state].[!curNum]
                            if arr <> null then
                                for (prod, pos) in arr do
                                    reductions.Push (newVertex, prod, pos, Some (final, box edgeLabel))

                let rec walk remainLength (vertex : Vertex) path =
                    if remainLength = 0 then handlePath path vertex
                    else
                        if vertex.Level <> num then
                            if vertex.OutEdges.other <> null then
                                vertex.OutEdges.other |> Array.iter
                                    (fun e ->
                                        path.[remainLength - 1] <- e.Ast
                                        walk (remainLength - 1) e.Dest path)
                            path.[remainLength - 1] <- vertex.OutEdges.first.Ast
                            walk (remainLength - 1) vertex.OutEdges.first.Dest path
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
                                path.[remainLength - 1] <- box a
                                walk (remainLength - 1) v path
                                while j < edges.Count && trd edges.[j] = a do
                                    j <- j + 1
                                i <- j
                
                if pos = 0 then
                    let state = parserSource.Gotos.[vertex.State].[nonTerm]
                    let newVertex = addVertex state num None
                    let ast = getEpsilon parserSource.LeftSide.[prod]
                    if not <| containsSimpleEdge vertex ast simpleEdges.[state] then
                        addSimpleEdge vertex ast simpleEdges.[state]
                else 
                    let path = Array.zeroCreate pos
                    path.[pos - 1] <- snd edgeOpt.Value
                    walk (pos - 1) (fst edgeOpt.Value) path

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
                        vEdges.[count] <- new Edge(v, box a)
                    else
                        first <- new Edge(v, box a)
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
            let newAstNode = box tokens.Count
            tokens.Add enum.Current
            if enum.MoveNext() then
                curToken := enum.Current
                curNum := parserSource.TokenToNumber enum.Current
            else
                curNum := parserSource.EofIndex
            for vertex in usedStates do
                stateToVertex.[vertex] <- null
            (*usedStates.Clear()
            let oldPushes = pushes.ToArray()
            pushes.Clear()*)
            let oldPushes = Stack()
            for vertex, state in pushes do
                if vertex.State |> usedStates.Contains 
                then 
                    oldPushes.Push (vertex, state)
            pushes.Clear()
            usedStates.Clear()

            for (vertex, state) in oldPushes do
                let newVertex = addVertex state num <| Some (vertex, newAstNode)
                addSimpleEdge vertex newAstNode simpleEdges.[state]
        
        let recovery() =
            let recVertNumber = 1
            if usedStates.Count <> 0 
            then 
                let prevNum = !curNum

                curNum := parserSource.ErrorIndex
                let temp = new Queue<_>()
                let curVertices = new Stack<_> (statesCount)

                for vertex in usedStates do
                    let path = []
                    curVertices.Push (stateToVertex.[vertex], path)
                    stateToVertex.[vertex] <- null
                usedStates.Clear()
                
                //pushes.count may be equal to 1
                //if parser finished in the non-accepting state and it generates the recovery
                pushes.Clear()
                
                let containsRecState (oldVertices : Stack<Vertex * _ list>) =
                    let oldVert = oldVertices.ToArray()
                    for vertex, path in oldVert do
                        if pushes.Count <> recVertNumber 
                        then
                            //if reduce is possible
                            let arr = parserSource.Reduces.[vertex.State].[!curNum]
                            if arr <> null
                            then 
                                for (prod, pos) in arr do
                                    let edgeOpt = Some (vertex.OutEdges.first.Dest, vertex.OutEdges.first.Ast)
                                    reductions.Push (vertex, prod, pos, edgeOpt)
                                makeReductions !curInd
                                temp.Enqueue path
                            //if shift is possible
                            let push = parserSource.Gotos.[vertex.State].[!curNum]
                            if push <> 0 
                            then
                                if pushes.Count <> 0 
                                then
                                    let recVert = fst <| pushes.Peek()
                                    if recVert.State <> vertex.State 
                                    then 
                                        pushes.Push (vertex, push)
                                        temp.Enqueue path
                                else 
                                    pushes.Push (vertex, push)
                                    temp.Enqueue path

                    pushes.Count + reductions.Count >= recVertNumber

                let searchRecSymbol (unbrowsed : obj[]) = 
                    let makeErrReductions (vertex : Vertex) state (unbrowsed : obj[]) = 
                        let prodNumber = parserSource.Rules.Length
                        let pos = unbrowsed.Length
                        if pos = 0 
                        then 
                            let ast = getEpsilon parserSource.ErrorIndex
                            if not <| containsSimpleEdge vertex ast simpleEdges.[state] 
                            then
                                addSimpleEdge vertex ast simpleEdges.[state]
                                let arr = parserSource.Reduces.[state].[!curNum]
                                if arr <> null 
                                then
                                    for (prod, pos) in arr do
                                        reductions.Push (vertex, prod, pos, Some (vertex, ast))
                        else    
                            let family = new Family(prodNumber, new Nodes(unbrowsed))
                            if not <| containsEdge vertex family edges.[state] 
                            then
                                let isCreated, edgeLabel = addEdge vertex family edges.[state] true
                                //printfn "isCreated %b" isCreated
                                let arr = parserSource.Reduces.[state].[!curNum]
                                if arr <> null 
                                then
                                    for (prod, pos) in arr do
                                        reductions.Push (vertex, prod, pos, Some (vertex, box edgeLabel))
                           
                    let state = snd <| pushes.Peek()

                    if parserSource.Reduces.[state].[!curNum] <> null
                    then // reductions is possible
                        let vertex = fst <| pushes.Peek()
                        makeErrReductions vertex state unbrowsed
                                            
                    else // if shift is possible
                        let oldPushes = pushes.ToArray()
                        for state in usedStates do
                            stateToVertex.[state] <- null
                        pushes.Clear()
                        usedStates.Clear()
                
                        for vertex, state in oldPushes do
                            makeErrReductions vertex state unbrowsed

                            let astNode = box tokens.Count
                            tokens.Add !curToken
                            addVertex state !curInd None |> ignore

                let getPrevVertices() = 
                    let isNewVertex (v : Vertex) = 
                        let flag = ref true
                        for vert, _ in curVertices do
                            if !flag && vert.Level = v.Level && vert.State = v.State 
                            then
                                flag := false
                        !flag
                
                    let oldVertices = curVertices.ToArray()
                    curVertices.Clear()
                    for vertex, path in oldVertices do
                        if vertex.OutEdges.first.Dest <> null 
                        then
                            if vertex.OutEdges.other <> null 
                            then
                                for edge in vertex.OutEdges.other do 
                                    if  isNewVertex edge.Dest
                                    then
                                        let newPath = edge.Ast :: path 
                                        curVertices.Push (edge.Dest, newPath)

                            if isNewVertex vertex.OutEdges.first.Dest
                            then
                                let newPath = vertex.OutEdges.first.Ast :: path
                                curVertices.Push (vertex.OutEdges.first.Dest, newPath)
            
                while curVertices.Count <> 0 && not <| containsRecState curVertices do
                    getPrevVertices()
                
                let skipped = Queue<_>()
                let oldPushes = pushes.ToArray() |> Array.rev
                pushes.Clear()

                let recPush = Array.zeroCreate oldPushes.Length
                let recVertex = Array.zeroCreate oldPushes.Length
                let recoveryTokens = Array.zeroCreate oldPushes.Length
                    
                for i in 0..oldPushes.Length-1 do
                    let vertex = fst oldPushes.[i]
                    let state = snd oldPushes.[i]
                    recVertex.[i] <- vertex
                    recPush.[i] <- state
                    recoveryTokens.[i] <- parserSource.GetExpectedTokens state      
                
                let isRecToken num = 
                    let res = ref -1
                    for i in 0..recoveryTokens.Length-1 do
                        if !res < 0 && recoveryTokens.[i] |> Array.exists ((=) num) 
                        then
                            res := i
                    !res

                curNum := prevNum
                let var = ref <| isRecToken !curNum

                while !var = -1 && !curNum <> parserSource.EofIndex do
                    let newAstNode = box tokens.Count
                    tokens.Add !curToken
                    skipped.Enqueue newAstNode
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
                    let skippedArray = skipped.ToArray()
                    let unbrowsed = Array.append need skippedArray
                    pushes.Push (recVertex.[!var], recPush.[!var])
                    searchRecSymbol unbrowsed             

        let mutable errorList = []                    
        let errorRuleExist = parserSource.ErrorRulesExists
        let mutable wasError = ref false
        let lastErr = ref -1

        while not !isEnd && not !wasError do
            if usedStates.Count = 0 && reductions.Count = 0
            then
                let errInfo =  !curInd, !curToken 
                errorList <- errInfo :: errorList
                wasError <- ref true
            else
                makeReductions !curInd
                attachEdges()
                if !curNum = parserSource.EofIndex then isEnd := true
                elif pushes.Count = 0 then 
                    if !curInd - !lastErr > 0
                    then
                        let errInfo =  !curInd, !curToken
                        errorList <- errInfo :: errorList
                    if errorRuleExist 
                    then 
                        recovery()
                        lastErr := !curInd
                    else wasError <- ref true
                else
                    incr curInd
                    shift !curInd
        
        let isAcceptState() = 
            let flag = ref false
            for state in usedStates do
                if not !flag 
                then
                    flag := parserSource.AccStates.[state]
            !flag                    

        // if finish isn't accepting state then error
        if !isEnd && usedStates.Count > 0 && not <| isAcceptState() 
        then
            if !curInd - !lastErr > 0
            then
               let errInfo =  !curInd, !curToken
               errorList <- errInfo :: errorList
            if errorRuleExist 
            then 
                 recovery()
                 makeReductions <| !curInd + 1
                 attachEdges()
                 lastErr := !curInd
                 isAcceptState() |> ignore
            else wasError <- ref true

        let lastTokens count =
            [| for i = max 0 (tokens.Count-count) to tokens.Count-1 do
                yield tokens.[i]|]
        let debugFuns () =
            let vertices = usedStates.ToArray() |> Array.map (fun i -> stateToVertex.[i])
            {
                drawGSSDot = drawDot parserSource.TokenToNumber tokens parserSource.LeftSide vertices parserSource.NumToString parserSource.ErrorIndex
                lastTokens = lastTokens
            }
        //(debugFuns ()).drawGSSDot "stack.dot"
        (*if not errorList.IsEmpty 
        then
            errorList <- List.rev errorList
            let tokenToString token = token |> parserSource.TokenToNumber |> parserSource.NumToString
            for i = 0 to errorList.Length-1 do
                printfn "Parse error in %d position in %s token. " <| fst errorList.[i] <| tokenToString (snd errorList.[i])
            printfn "errorList(parser).count = %d" errorList.Length*)
        if !wasError 
        then 
            Error (!curInd , !curToken , "Parse Error", debugFuns ())
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
            | None -> Error (!curInd, !curToken, "Input was fully processed, but it's not complete correct string.", debugFuns ())
            | Some res -> 
            //    debugFuns().drawGSSDot "res.dot"
                Success <| new Tree<_>(tokens.ToArray(), res, parserSource.Rules)