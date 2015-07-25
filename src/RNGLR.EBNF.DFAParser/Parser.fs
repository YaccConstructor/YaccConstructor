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
open System.Collections.Generic
open Yard.Generators.Common.DataStructures
open Yard.Generators.RNGLR.EBNF.DFA
open Microsoft.FSharp.Collections

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
        val Symbol : int
        new (d, s, a) = {Dest = d; Symbol = s; Ast = a}
    end

type ParserDebugFuns<'TokenType> = {
    drawGSSDot : string -> unit
    /// If you need more then one last token
    lastTokens : int -> 'TokenType[]
}

type ParseResultEBNF<'TokenType> =
    | Success of Tree<'TokenType> * Dictionary<Family, ErrorNode>
    | Error of int * array<'TokenType> * string * Dictionary<Family, ErrorNode>

/// Compare vertex like a pair: (level, state)
let inline private less (v' : Vertex) (v : Vertex) = v'.Level < v.Level || (v'.Level = v.Level && v'.State < v.State)
let inline private eq (v' : Vertex) (v : Vertex) = v'.Level = v.Level && v'.State = v.State

//Compare stackSets
let inline private stLLess (s' : int * int) (s : int * int) = 
    let s'x, s'y = s'
    let sx, sy = s
    s'x < sx || (s'x = sx && s'y < sy)

let inline private stLEq (s' : int * int) (s : int * int) = 
    let s'x, s'y = s'
    let sx, sy = s
    s'x = sx && s'y = sy

/// Add edges, what must be unique (after shift or epsilon-edges).
/// All edges are sorted by destination ascending.
let private addSimpleEdge (v : Vertex) (ast : obj) (out : ResizeArray<Vertex * obj>) =
    let inline fst2 (x,_) = x
    let inline snd2 (_,x) = x
    let mutable i = out.Count - 1
    while i >= 0 && less (fst2 out.[i]) v do
        i <- i - 1
//    while i >= 0 && eq (fst2 out.[i]) v do
//        i <- i - 1
    out.Insert (i+1, (v, ast))

/// Check if edge with specified destination and AST already exists
let private containsSimpleEdge (v : Vertex) (f : obj) (out : ResizeArray<Vertex * obj>) =
    let inline fst2 (x,_) = x
    let inline snd2 (_,x) = x
    let mutable i = out.Count - 1
    while i >= 0 && less (fst2 out.[i]) v do
        i <- i - 1
//    while i >= 0 && eq (fst2 out.[i]) v do
//        i <- i - 1
    while i >= 0 && (let v',f' = out.[i] in eq v' v && f <> f') do
        i <- i - 1
    i >= 0 && (let v',f' = out.[i] in eq v' v && f = f')

/// Add or extend edge with specified destination and family.
/// All edges are sorted by destination ascending.
let private addEdge (v : Vertex) (family : Family) (out : ResizeArray<Vertex * Family * AST>) isError =
    let mutable i = out.Count - 1
    let inline fst3 (x,_,_) = x
    let inline snd3 (_,x,_) = x
    let inline trd3 (_,_,x) = x
    while i >= 0 && less (fst3 out.[i]) v do
        i <- i - 1
//    while i >= 0 && eq (fst3 out.[i]) v do
//        i <- i - 1

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
            j <- j - 1
        out.RemoveRange(j+1, i-j-1)
    isCreated, ast

/// Check if edge with specified destination and family already exists
let private containsEdge (v : Vertex) (f : Family) (out : ResizeArray<Vertex * Family * AST>) =
    let inline fst3 (x,_,_) = x
    let inline snd3 (_,x,_) = x
    let mutable i = out.Count - 1
    while i >= 0 && less (fst3 out.[i]) v do
        i <- i - 1
//    while i >= 0 && eq (fst3 out.[i]) v do
//        i <- i - 1
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
        if not (u.Level = 0 && u.State = 0) && u.OutEdges.first <> Unchecked.defaultof<_> then
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

let recoverDfa (dfaList : int[][][]) (amountOfStates : int[]) = 
    let prodCount = dfaList.Length
    let symbCount =
        match prodCount with
        | 0 -> 0
        | _ -> dfaList.[0].Length
    let res = Array.zeroCreate prodCount
    for i = 0 to prodCount - 1 do
        let amount = amountOfStates.[i]
        res.[i] <- Array.zeroCreate amount
        for j = 0 to amount - 1 do
            res.[i].[j] <- Array.zeroCreate symbCount
        for k = 0 to symbCount - 1 do
            let states = dfaList.[i].[k]
            for s = 0 to states.Length - 1 do
                if s % 2 = 0 then res.[i].[states.[s]].[k] <- states.[s + 1]
    res

let recoverFiniteStates (finiteStates : int[][]) = 
    let res : HashSet<int>[] = Array.zeroCreate finiteStates.Length
    for i = 0 to finiteStates.Length - 1 do
        res.[i] <- new HashSet<_>()
        for el in finiteStates.[i] do
            res.[i].Add(el) |> ignore
    res

type ReduceLabel =
    |Reduce
    |StackReduce

let buildAst<'TokenType> (parserSource : ParserSourceEBNF<'TokenType>) (tokens : seq<'TokenType>) =
    let enum = tokens.GetEnumerator()
    // Change if it doesn't equal to zero. Now it's true according to states building algorithm
    let startState = 0
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let nonTermsCountLimit = 1 + (Array.max parserSource.LeftSide)
    let getEpsilon =
        let epsilons = Array.init nonTermsCountLimit (fun i -> box (-i-1))
        fun i -> epsilons.[i]
    /// info about errors
    let errDict = new Dictionary<Family, ErrorNode>()
                              
    let getExpectedGoto i j =
        match parserSource.Gotos.[i].[j] with
            |Some x -> x
            |None -> -1 
            //|None -> failwithf "Expected goto[%d][%s]" i (parserSource.NumToString j)

    // If input stream is empty or consists only of RNGLR_EOF token
    if not <| enum.MoveNext() || parserSource.EofIndex = parserSource.TokenToNumber enum.Current then
        if parserSource.AcceptEmptyInput 
        then
            Success (new Tree<_>(null, getEpsilon startNonTerm, null), errDict)
        else
            Error (0, [||], "This grammar cannot accept empty string",
                    (*{
                        drawGSSDot = fun _ -> ()
                        lastTokens = fun _ -> [||]
                    }, *)
                    errDict)
    else                                     
        // Currently processed token
        let curToken = ref enum.Current
        let curNum = ref (parserSource.TokenToNumber enum.Current)
        let prevNum = ref (parserSource.TokenToNumber enum.Current)
        let mutable initPrevToken = false
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

        let dfaListSource = parserSource.DfaList
        let finiteStatesSource = parserSource.FiniteStates
        let amountOfStates = parserSource.AmountOfStates
        let dfaTabels = recoverDfa dfaListSource amountOfStates
        let finiteStates = recoverFiniteStates finiteStatesSource

        let rec printAllRec (v : Vertex) (wasForPrint : HashSet<_>) =
            if v <> null && not <| wasForPrint.Contains(v.Level, v.State)
            then
                let levelV = v.Level
                let stateV = v.State
                wasForPrint.Add(levelV, stateV) |> ignore
                let outEdgesV = v.OutEdges
                let first = outEdgesV.first
                let other = outEdgesV.other

                printfn "Vertex: lev = %A  state = %A" levelV stateV
                printf "firstS = %A  " first.Symbol
                let dest = first.Dest
                if dest <> null
                then
                    printfn "vert: lev = %A state = %A" first.Dest.Level first.Dest.State
                else
                    printfn "vert: null"
                if other <> null
                then
                    for e in other do
                        printf "otherS = %A  " e.Symbol
                        let d = e.Dest
                        if d <> null
                        then
                            printfn "vert: lev = %A state = %A" e.Dest.Level e.Dest.State
                        else
                            printfn "vert: null"
                printfn "-----------"

                let simpleEdgesV = simpleEdges.[stateV]
                let edgesV = edges.[stateV]
                printfn "Call first edge\n***********"
                printAllRec first.Dest wasForPrint
                if other <> null
                then
                    for e in other do
                        printfn "Call other edge\n***********"
                        printAllRec e.Dest wasForPrint
                for d in simpleEdgesV do
                    let (v,_) = d
                    printfn "Call simple edge\n***********"
                    printAllRec v wasForPrint
                for t in edgesV do
                    let (v,_,_) = t
                    printfn "Call edge edge\n***********"
                    printAllRec v wasForPrint
            else
                if v <> null
                then
                    printfn "\nWas: %A %A" v.Level v.State

        let printAll (v : Vertex) =
            let wasForPrint = new HashSet<_>()
            printAllRec v wasForPrint

        let printStateToVertex () =
            for v in stateToVertex do
                printAll v

        let printVer (v : Vertex) = 
            if v <> null
            then
                let levelV = v.Level
                let stateV = v.State
                let outEdgesV = v.OutEdges
                let first = outEdgesV.first
                let other = outEdgesV.other

                printfn "Just Vertex"
                printfn "Vertex: lev = %A  state = %A" levelV stateV
                printf "firstS = %A  " first.Symbol
                let dest = first.Dest
                if dest <> null
                then
                    printfn "vert: lev = %A state = %A" first.Dest.Level first.Dest.State
                else
                    printfn "vert: null"
                if other <> null
                then
                    for e in other do
                        printf "otherS = %A  " e.Symbol
                        let d = e.Dest
                        if d <> null
                        then
                            printfn "vert: lev = %A state = %A" e.Dest.Level e.Dest.State
                        else
                            printfn "vert: null"

                let simpleEdgesV = simpleEdges.[stateV]
                let edgesV = edges.[stateV]
                for d in simpleEdgesV do
                    let (v,_) = d
                    printfn "from simple edge| vert: lev = %A state = %A" v.Level v.State
                for t in edgesV do
                    let (v,_,_) = t
                    printfn "from simple edge| vert: lev = %A state = %A" v.Level v.State
                printfn "-----------"


        let addVertex state level (edgeOpt : option<Vertex * int * obj>) =
            let dict = stateToVertex
            if dict.[state] = null then
                let v = new Vertex(state, level)
                dict.[state] <- v
                
                match parserSource.Gotos.[state].[!curNum] with
                | Some x ->
                    let push = x
                    // Push to init state is impossible
                    if push <> 0 then
                        pushes.Push (v, push)
                | None -> ()
                let arr = parserSource.ZeroReduces.[state].[!curNum]
                if arr <> null then
                    for prod in arr do
                        reductions.Push (v, prod, StackReduce, None)
                usedStates.Push state
            let v = dict.[state]
            if edgeOpt.IsSome then 
                let arr = parserSource.Reduces.[state].[!curNum]
                if arr <> null then
                    for prod in arr do
                        reductions.Push (v, prod, Reduce, edgeOpt)

//            printfn "New Vertex"
//            printAll v
            v

        ignore <| addVertex startState 0 None
        let inline fst2 (x,_) = x
        let inline snd2 (_,x) = x
        let inline trd (_,_,x) = x
        let inline fst3 (x,_,_) = x
        let inline snd3 (_,x,_) = x
        let inline trd4 (_,_,x,_) = x
        let inline frth (_,_,_,x) = x
        let makeReductions num recovery =
            while reductions.Count > 0 do
                let vertex, prod, rLabel, edgeOpt = reductions.Pop()

//                printfn "\n----------------FromRed----------------"
//                printAll vertex
//                printfn "------EdgeOpt----------"
//                match edgeOpt with
//                | Some x -> printfn "vert: lev = %A  state = %A" (fst3 x).Level (fst3 x).State
//                | None   -> printfn ""
//                printfn "\n----------------EndRed----------------"

                let nonTerm = parserSource.LeftSide.[prod]
                let vertexSet = ref Set.empty

                let handlePath (path : obj list) (final : Vertex) =
                    if final = null
                    then recovery()//pushes.Clear()
                    else
                        let state = getExpectedGoto final.State nonTerm
                        if state <> -1
                        then
                            let newVertex = addVertex state num None           
                            let family = new Family(prod, new Nodes(Array.ofList path))
                            if not <| containsEdge final family edges.[state] then
                                let isCreated, edgeLabel = addEdge final family edges.[state] false
                                if (rLabel = Reduce && isCreated) then
                                    let arr = parserSource.Reduces.[state].[!curNum]
                                    if arr <> null then
                                        for prod in arr do
                                            reductions.Push (newVertex, prod, Reduce, Some (final, nonTerm, box edgeLabel))



                let rec walk (vertex : Vertex) prod state (path : obj list) edgeOptWas =
//                    printVer vertex

                    let handle = finiteStates.[prod].Contains state
                    let walkFurther =
                        if state <> -1
                        then
                            let mutable contain = false
                            if vertex.OutEdges.other <> null
                            then
                                contain <- vertex.OutEdges.other |> Array.exists (fun e -> dfaTabels.[prod].[state].[e.Symbol] <> -1)
                            match edgeOpt with
                            | Some t -> contain || (dfaTabels.[prod].[state].[vertex.OutEdges.first.Symbol] <> -1) || ((dfaTabels.[prod].[state].[snd3 t] <> -1) && not <| edgeOptWas)
                            | None   -> contain || (dfaTabels.[prod].[state].[vertex.OutEdges.first.Symbol] <> -1)
                        else
                            false

                    if handle then handlePath path vertex
                    if walkFurther && vertex <> null
                    then
                        let visitVertex (v : Vertex) state symb a edgeOptWas =
                            if v <> null && not <| Set.contains (v.Level, v.State, state) !vertexSet 
                            then
                                vertexSet := Set.add (v.Level, v.State, state) !vertexSet
                                if symb <> -1
                                then
                                    walk v prod dfaTabels.[prod].[state].[symb] (a::path) edgeOptWas

                        if (edgeOpt.IsSome) && not <| edgeOptWas
                        then
                            visitVertex (fst3 edgeOpt.Value) 0 (snd3 edgeOpt.Value) (trd edgeOpt.Value) true

                        if vertex.Level <> num then
                            if vertex.OutEdges.other <> null then
                                vertex.OutEdges.other |> Array.iter
                                    (fun e -> 
                                        visitVertex e.Dest state e.Symbol e.Ast edgeOptWas)
                            let e = vertex.OutEdges.first                    
                            visitVertex e.Dest state e.Symbol e.Ast edgeOptWas
                        else
                            for d in simpleEdges.[vertex.State] do
                                visitVertex (fst d) state -1 (snd d) edgeOptWas
                            let mutable i = 0
                            let edges = edges.[vertex.State]
                            while i < edges.Count do
                                let (v,_,a) = edges.[i]
                                if v.OutEdges.other <> null 
                                then
                                    v.OutEdges.other |> Array.iter (fun e -> visitVertex e.Dest state e.Symbol e.Ast edgeOptWas)
                                let e = v.OutEdges.first                    
                                visitVertex e.Dest state e.Symbol e.Ast edgeOptWas
                                i <- i + 1
                    else recovery() //pushes.Clear()

//                printfn "\n----------------StartState---------------"
//                printStateToVertex()
//                printfn "----------------EndState---------------\n"

                match rLabel with
                |StackReduce ->
                    let goto = getExpectedGoto vertex.State nonTerm
                    let newVertex = addVertex goto num None
                    let ast = getEpsilon parserSource.LeftSide.[prod]
                    if not <| containsSimpleEdge vertex ast simpleEdges.[goto] then
                        addSimpleEdge vertex ast simpleEdges.[goto]
                |Reduce ->
                    let path = [trd edgeOpt.Value]
//                    vertexSet := Set.add ((fst3 edgeOpt.Value).Level, (fst3 edgeOpt.Value).State, 0) !vertexSet
                    vertexSet := Set.add (vertex.Level, vertex.State, 0) !vertexSet
//                    walk (fst2 edgeOpt.Value) prod 0 path
                    walk vertex prod 0 path false

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
                        vEdges.[count] <- new Edge(v, !prevNum, box a)
                    else
                        first <- new Edge(v, !prevNum, box a)
                    count <- count + 1
                    a.first <- snd3 edges.[i]
                    a.other <- other
                    i <- j

                for i = 0 to simpleEdges.[vertex].Count - 1 do
                    let v, a = simpleEdges.[vertex].[i]
                    if count >= 0 then
                        vEdges.[count] <- new Edge(v, !prevNum, a)
                    else
                        first <- new Edge(v, !prevNum, a)
                    count <- count + 1

                stateToVertex.[vertex].OutEdges <- UsualOne<_>(first, vEdges)
                edges.Clear()
                simpleEdges.[vertex].Clear()

        let shift num =
            let newAstNode = box tokens.Count
            tokens.Add enum.Current
            prevNum := !curNum 
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
            let oldPushes = new Stack<_>()
            for vertex, state in pushes do
                if vertex.State |> usedStates.Contains 
                then 
                    oldPushes.Push (vertex, state)
            pushes.Clear()
            usedStates.Clear()

            for (vertex, state) in oldPushes do
                let newVertex = addVertex state num <| Some (vertex, !prevNum, newAstNode)
                addSimpleEdge vertex newAstNode simpleEdges.[state]
        
        /// returns all the terminals and non-terminals that make the push or reduce
        /// it's used by recovery
        let getExpectedTokens state = 
            let expected = ref <| Set.empty
            let ps = parserSource

            for i = 0 to ps.Gotos.[0].GetLength(0)-1 do
                if (ps.Gotos.[state].[i].IsSome && ps.Gotos.[state].[i].Value <> 0) || ps.Reduces.[state].[i] <> null
                then expected := expected.Value.Add i
            
            !expected
        
        /// returns  array that consists of tokens or error non-teminal (and its children)
        let astToTokens (x : obj) =            
            let visited = ref 0
            let rec go (x : obj) =
                let mutable res = []
                incr visited
                if !visited < 100 
                then                    
                    match x : obj with 
                    | :? int as t when t >= 0 -> res <- x :: res
                    | :? Family as fam ->                    
                        for i = 0 to fam.nodes.Length - 1 do
                            res <- res @ go fam.nodes.[i]
                    | :? AST as ast ->
                        if ast.other <> null 
                        then                            
                            for family in ast.other do
                                if family.prod = parserSource.LeftSide.Length
                                then res <- res @ [ast]
                                else res <- res @ go family
                            
                        if ast.first.prod = parserSource.LeftSide.Length
                        then res <- [ast] @ res
                        else res <- go ast.first @ res
                    | _ -> ()
                res
            go x
        
        /// collects info about error that is needed in the translation
        let createErrorNode (errFamily : Family) (errOn : obj) (prod : int) (expected : int[]) (recToks : int[]) = 

            let exp = expected |> Array.map (fun i -> parserSource.NumToString i)
            let recToks = recToks |> Array.map (fun i -> parserSource.NumToString i)
            
            try errDict.Add (errFamily, new ErrorNode (errOn, -1, exp, recToks))
            with _ -> ()

        (*let containsRecState (oldVertices : Stack<Vertex * _ list>)(temp : Queue<_>) recVertNum recovery =
            let oldVert = oldVertices.ToArray()
            for vertex, path in oldVert do
                if pushes.Count <> recVertNum
                then
                    //if reduce is possible
                    let arr = parserSource.Reduces.[vertex.State].[!curNum]
                    if arr <> null
                    then 
                        for prod in arr do
                            //It's an issue what to place there as second arg
                            let edgeOpt = Some (vertex.OutEdges.first.Dest, _, vertex.OutEdges.first.Ast)
                            reductions.Push (vertex, prod, Reduce, edgeOpt)
                        makeReductions !curInd recovery
                        temp.Enqueue path
                    //if shift is possible
                    if parserSource.Gotos.[vertex.State].[!curNum].IsSome && fst parserSource.Gotos.[vertex.State].[!curNum].Value <> 0 
                    then
                        let push = parserSource.Gotos.[vertex.State].[!curNum].Value
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

            pushes.Count + reductions.Count >= recVertNum*)

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
        (*let createErrorFam (unbrowsed : obj[])  = 
            let reduceToError (vertex : Vertex) state (unbrowsed : obj[])= 
                let prodNumber = parserSource.Rules.Length
                if unbrowsed.Length = 0 
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
                    new Family(prodNumber, new Nodes([|ast|]))
                else
                    let family = new Family(prodNumber, new Nodes(unbrowsed))
                            
                    if not <| containsEdge vertex family edges.[state] 
                    then
                        let _, edgeLabel = addEdge vertex family edges.[state] true
                        let arr = parserSource.Reduces.[state].[!curNum]
                        if arr <> null 
                        then
                            for (prod, pos) in arr do
                                reductions.Push (vertex, prod, pos, Some (vertex, box edgeLabel))
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

                    let astNode = box tokens.Count
                    tokens.Add !curToken
                    addVertex state !curInd None |> ignore
                !fam*)

        let recovery() = ()(*
            let recVertNumber = 1
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

                let arr : array<Vertex * int * StackLabel * array<int>> = Array.zeroCreate oldPushes.Length
                    
                for i in 0..oldPushes.Length-1 do
                    let vertex, (state, stackLabel) = oldPushes.[i]
                    let recToks = Set.toArray <| getExpectedTokens state      
                    arr.[i] <- vertex, state, stackLabel, recToks
                
                let inline fst4 (s, _, _, _) = s
                let inline snd4 (_, s, _, _) = s
                let inline thr4  (_, _, s, _) = s
                let inline frth  (_, _, _, s) = s

                let isRecToken num = 
                    let res = ref -1
                    for i in 0..arr.Length-1 do
                        if !res < 0 && Array.exists ((=) num) <| frth arr.[i] 
                        then res := i
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
                    let unbrowsed = Array.append need <| skipped.ToArray()
                    pushes.Push (fst4 arr.[!var], (snd4 arr.[!var], thr4 arr.[!var]))
                    let fam = createErrorFam unbrowsed
                    let rec onTok i =
                        if i >= 0
                        then
                            let x = tokens.[i] 
                            if box x <> null
                            then box x
                            else onTok (i-1)
                        else null
                            
                    createErrorNode fam <| onTok errInd <| 0 <| Set.toArray !expected <| frth arr.[!var]*)

        //let errorRuleExist = parserSource.ErrorRulesExists
        let wasError = ref false

        while not !isEnd && not !wasError do
            if usedStates.Count = 0 && reductions.Count = 0
            then 
                wasError := true
            else
                makeReductions !curInd recovery
                attachEdges()
                if !curNum = parserSource.EofIndex then isEnd := true
                elif pushes.Count = 0 then 
                    (*if errorRuleExist 
                    then recovery()
                    else*) wasError := true
                else
                    incr curInd
                    shift !curInd

//            printfn "\n----------------StartState---------------"
//            printStateToVertex()
//            printfn "----------------EndState---------------\n"
        
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
            [| for i = max 0 (tokens.Count-count) to tokens.Count-1 do
                yield tokens.[i]|]
        let debugFuns () =
            let vertices = usedStates.ToArray() |> Array.map (fun i -> stateToVertex.[i])
            {
                drawGSSDot = drawDot parserSource.TokenToNumber tokens parserSource.LeftSide vertices parserSource.NumToString parserSource.ErrorIndex
                lastTokens = lastTokens
            }
        
        if !wasError 
        then 
            Error (!curInd , [|!curToken|] , "Parse Error", errDict)
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
            | None -> Error (!curInd, [|!curToken|], "Input was fully processed, but it's not complete correct string.", errDict)
            | Some res -> 
                //debugFuns().drawGSSDot "res.dot"
                Success (new Tree<_> (tokens.ToArray(), res, [||]), errDict)