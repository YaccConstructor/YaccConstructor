
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
open Yard.Generators.Common.ARNGLR.AST
open System.Collections.Generic
open Yard.Generators.Common.DataStructures
//open Yard.Generators.RNGLR.Parser 
open Microsoft.FSharp.Collections
open AbstractAnalysis.Common

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
        val Ast : INode
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
    | Success of Yard.Generators.Common.ARNGLR.AST.Tree<'TokenType> * array<'TokenType> * Dictionary<Family, ErrorNode>
    | Error of int * array<'TokenType> * string * ParserDebugFuns<'TokenType> * Dictionary<Family, ErrorNode>

[<Struct>]
type TokenWithInfo<'token> =
    val Token  : 'token
    val NxtLvl : int
    val CurNum : int
    new (t, nl, cn) = {Token = t; NxtLvl = nl; CurNum = cn}

[<Struct>]
type TokensInfo<'token> =
    val Tokens : array<TokenWithInfo<'token>>
    val CurLvl : int
    new (ts, cl) = {Tokens = ts; CurLvl = cl}

type StatesInfo = 
    val Processed :    ResizeArray<Vertex>
    val NotProcessed : ResizeArray<Vertex>
    new (p, np) = {Processed = p; NotProcessed = np}

// Custom graph structure. For optimization and needed (by algorithm) relation with AST

/// Compare vertex like a pair: (level, state)
let inline private less (v' : Vertex) (v : Vertex) = v'.Level < v.Level || (v'.Level = v.Level && v'.State < v.State)
let inline private eq (v' : Vertex) (v : Vertex) = v'.Level = v.Level && v'.State = v.State

let inline fst3 (x,_,_) = x

/// Add edges, what must be unique (after shift or epsilon-edges).
/// All edges are sorted by destination ascending.
let private addSimpleEdge (v : Vertex) (ast : INode) (out : ResizeArray<Vertex * INode>) =
    let mutable i = out.Count - 1
    while i >= 0 && less (fst out.[i]) v do
        i <- i - 1
    out.Insert (i+1, (v, ast))

/// Check if edge with specified destination and AST already exists
let private containsSimpleEdge (v : Vertex) (f : INode) (out : ResizeArray<Vertex * INode>) =
    let mutable i = out.Count - 1
    while i >= 0 && less (fst out.[i]) v do
        i <- i - 1
    while i >= 0 && (let v',f' = out.[i] in eq v' v && f <> f') do
        i <- i - 1
    i >= 0 && (let v',f' = out.[i] in eq v' v && f = f')

/// Add or extend edge with specified destination and family.
/// All edges are sorted by destination ascending.
let private addEdge (v : Vertex) (family : Family) (out : ResizeArray<Vertex * Family * AST>) =
    let mutable i = out.Count - 1
    while i >= 0 && less (fst3 out.[i]) v do
        i <- i - 1

    let isCreated = not (i >= 0 && eq (fst3 out.[i]) v)

    let ast = if not isCreated 
              then let _,_,n = out.[i] in n
              else new AST (Unchecked.defaultof<_>, null)

    out.Insert (i+1, (v, family, ast))
    isCreated, ast

/// Check if edge with specified destination and family already exists
let private containsEdge (v : Vertex) (f : Family) (out : ResizeArray<Vertex * Family * AST>) =
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
    let getAstString (ast : INode) =
        match ast with
        | :? Terminal as t -> tokens.[t.TokenNumber] |> tokenToNumber |> numToString |> sprintf "%s"    
        | :? Epsilon as e -> "eps " + numToString (-e.EpsilonNonTerm - 1)
        | :? AST as ast -> 
            let nonT = 
                if ast.first.prod < leftSide.Length 
                then ast.first.prod
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
        if not <| was.ContainsKey v 
        then dfs v
        print <| sprintf "%d -> %d [label=\"%s\"]" was.[u] was.[v] (getAstString e.Ast)

    for v in initNodes do
        if not <| was.ContainsKey v 
        then dfs v
    
    for level in levels do
        print <| sprintf "{rank=same; %s}" (level.Value |> List.map (fun (u : int) -> string u) |> String.concat " ")

    out.WriteLine "}"
    out.Close()

let buildAstAbstract<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : ParserInputGraph<'TokenType>) = //seq<int*array<'TokenType*int>>) =
    let startVertex = 0
    let verticesToProcess = new Queue<int>()
    let visitedVertices = new Dictionary<int, bool>(tokens.VertexCount);
    let statesToVertices = new Dictionary<int, StatesInfo>(tokens.VertexCount);
    
    for v in tokens.Vertices do
        visitedVertices.Add(v, false)
        statesToVertices.Add(v, new StatesInfo(new ResizeArray<Vertex>(), new ResizeArray<Vertex>()))

    let processChunk currentV =
        if not visitedVertices.[currentV]
        then 
            visitedVertices.[currentV] <- true;
            let outgoingEdges = tokens.OutEdges currentV |> Array.ofSeq
            Some(new TokensInfo<_>(outgoingEdges |> Array.map (fun t -> 
                verticesToProcess.Enqueue(t.Target)
                new TokenWithInfo<_>(t.Tag, t.Target, parserSource.TokenToNumber t.Tag)), currentV))
        else 
            None
    
    let pushesMap = new Dictionary<_,ResizeArray<_>>(10)
    // Change if it doesn't equal to zero. Now it's true according to states building algorithm
    let startState = 0
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let nonTermsCountLimit = 1 + (Array.max parserSource.LeftSide)
    let getEpsilon =
        let epsilons = Array.init nonTermsCountLimit (fun i -> new Epsilon (-i - 1))
        fun i -> epsilons.[i]
    
    let curTokens = processChunk startVertex |> ref
    // If input stream is empty or consists only of EOF token
    if tokens.EdgeCount = 0
       || tokens.OutEdges startVertex |> Seq.forall (fun t -> let x = parserSource.EofIndex = parserSource.TokenToNumber t.Tag in x)
       || (!curTokens).IsNone
    then
        if parserSource.AcceptEmptyInput
        then
            Success (new Tree<_>(null, getEpsilon startNonTerm, null), [||], new Dictionary<_,_>())
        else
            Error (0, Unchecked.defaultof<_>, "This grammar cannot accept empty string",
                    {
                        drawGSSDot = fun _ -> ()
                        lastTokens = fun _ -> [||]
                    }
                    , new Dictionary<_,_>())
    else
        // Currently processed token
        let temp = (!curTokens).Value
        let curTokens = (!curTokens).Value |> ref //enum.Current |> ref
        let curLvl = ref startVertex //(!curTokens).CurLvl
        let errorLvl = ref -1
        let isEOF = ref false
        /// Here all tokens from the input will be collected
        let tempTokens = new BlockResizeArray<_>()
        let reductions = new Stack<_>(10)
        let statesCount = parserSource.Gotos.Length
        // New edges can be created only from last level.
        /// Temporary storage for edges data (after all reductions real edges will be created).
        let edges = Array.init statesCount (fun _ -> new ResizeArray<Vertex * Family * AST>())
        let simpleEdges = Array.init statesCount (fun _ -> new ResizeArray<Vertex * INode>())
        let pushes = ref [||]
        let pushesInitFun l = 
            pushes := Array.init l (fun _ -> new Stack<_> (statesCount * 2 + 10))
        pushesInitFun (!curTokens).Tokens.Length
        /// Stores states, used on current level. Instead statesCount must be number of non-terminals, but doesn't matter
        let usedStates = new Stack<_>(statesCount)
        let stateToVertex : Vertex[] = Array.zeroCreate statesCount

        let addVertex vertData =
            [|
            for (state, inputV, (edgeOpt : option<Vertex * INode>)) in vertData |> Array.sortBy (fun (_,x,_) -> -x) do
                let curNums = if !isEOF 
                              then [|parserSource.EofIndex|] 
                              else (!curTokens).Tokens |> Array.map (fun t -> t.CurNum)
                if stateToVertex.[state] = null 
                then
                    let v = new Vertex(state, inputV)
                    stateToVertex.[state] <- v
                    curNums
                    |> Array.iteri (fun i curNum ->
                        let push = parserSource.Gotos.[state].[curNum]
                        // Push to init state is impossible
                        if push <> 0 
                        then
                            pushes.Value.[i].Push (v, push)
                        let arr = parserSource.ZeroReduces.[state].[curNum]
                        if arr <> null 
                        then
                            for prod in arr do
                                reductions.Push (v, prod, 0, None)
                    )
                    usedStates.Push state
                let v = stateToVertex.[state]
                if edgeOpt.IsSome 
                then
                    for curNum in curNums do
                        let arr = parserSource.Reduces.[state].[curNum]
                        if arr <> null
                        then
                            for (prod, pos) in arr do
                                reductions.Push (v, prod, pos, edgeOpt)
                statesToVertices.[inputV].NotProcessed.Add(v);
                yield v
            |]

        let vertices = addVertex [|startState, 0, None|]
        let inline trd (_,_,x) = x
        let makeReductions () =
            while reductions.Count > 0 do
                let vertex, prod, pos, edgeOpt = reductions.Pop()
                let nonTerm = parserSource.LeftSide.[prod]

                let handlePath (path : INode[]) (final : Vertex) =
                    let state = parserSource.Gotos.[final.State].[nonTerm]
                    let newVertex = (addVertex [|state, !curLvl, None|]).[0]
                    
                    let family = new Family(prod, new Nodes(Array.copy path))
                    if not <| containsEdge final family edges.[state]
                    then
                        let isCreated, edgeLabel = addEdge final family edges.[state]
                        if (pos > 0 && isCreated)
                        then
                            if (!curTokens).Tokens.Length = 0
                            then
                                let arr = parserSource.Reduces.[state].[parserSource.EofIndex]
                                if arr <> null 
                                then
                                    for (prod, pos) in arr do
                                        reductions.Push (newVertex, prod, pos, Some (final, edgeLabel :> INode))
                            else
                                for tok in (!curTokens).Tokens do 
                                    let arr = parserSource.Reduces.[state].[tok.CurNum]
                                    if arr <> null
                                    then
                                        for (prod, pos) in arr do
                                            reductions.Push (newVertex, prod, pos, Some (final, edgeLabel :> INode))

                let rec walk remainLength (vertex : Vertex) path =
                    if remainLength = 0 
                    then handlePath path vertex
                    else
                        if vertex.Level <> !curLvl 
                        then
                            if vertex.OutEdges.other <> null 
                            then
                                vertex.OutEdges.other
                                |> Array.iter (fun e ->
                                                   path.[remainLength - 1] <- e.Ast
                                                   walk (remainLength - 1) e.Dest path)
                            path.[remainLength - 1] <- vertex.OutEdges.first.Ast
                            walk (remainLength - 1) vertex.OutEdges.first.Dest path
                        else
                            simpleEdges.[vertex.State] 
                            |> ResizeArray.iter (fun (v,a) ->
                                                path.[remainLength - 1] <- a
                                                walk (remainLength - 1) v path)
                            
                            let mutable i = 0
                            let edges = edges.[vertex.State]
                            let mutable count = 0
                            while i < edges.Count do
                                let (v,_,a) = edges.[i]
                                let mutable j = i+1
                                path.[remainLength - 1] <- a :> INode
                                walk (remainLength - 1) v path
                                while j < edges.Count && trd edges.[j] = a do
                                    j <- j + 1
                                i <- j
                
                if pos = 0 
                then
                    let state = parserSource.Gotos.[vertex.State].[nonTerm]
                    addVertex [|state, !curLvl, None|] |> ignore
                    let ast = getEpsilon parserSource.LeftSide.[prod]
                    if not <| containsSimpleEdge vertex ast simpleEdges.[state]
                    then
                        addSimpleEdge vertex ast simpleEdges.[state]
                    if not <| containsEdge vertex (new Family(prod, new Nodes([|ast|]))) edges.[state]
                    then
                        addEdge vertex (new Family(prod, new Nodes([|ast|]))) edges.[state] |> ignore
                else 
                    let path = Array.zeroCreate pos
                    path.[pos - 1] <- snd edgeOpt.Value
                    walk (pos - 1) (fst edgeOpt.Value) path

        let curInd = ref 0
        let isEnd = ref false
        let attachEdges () =
            //()
            let inline snd3 (_,x,_) = x
            for state in usedStates do
                let mutable i = 0
                let edgesForState = edges.[state]
                let mutable count = -1
                while i < edgesForState.Count do
                    let ast = trd edgesForState.[i]
                    let mutable j = i + 1
                    while j < edgesForState.Count && trd edgesForState.[j] = ast do
                        j <- j + 1
                    i <- j
                    count <- count + 1
                count <- count + simpleEdges.[state].Count
                let vEdges =
                    if count > 0 
                    then Array.zeroCreate count
                    else null
                let mutable first = Unchecked.defaultof<_>
                i <- 0
                count <- -1
                while i < edgesForState.Count do
                    let (v,_,a) = edgesForState.[i]
                    let mutable j = i + 1
                    while j < edgesForState.Count && trd edgesForState.[j] = a do
                        j <- j + 1
                    let other = 
                        if j <> i + 1 
                        then
                            let res = Array.zeroCreate (j - i - 1)
                            for k = i + 1 to j - 1 do
                                res.[k - i - 1] <- snd3 edgesForState.[k]
                            res
                        else
                            null
                    if count >= 0 
                    then vEdges.[count] <- new Edge(v, a)
                    else first <- new Edge(v, a)
                    count <- count + 1
                    a.first <- snd3 edgesForState.[i]
                    a.other <- other
                    i <- j

                for i = 0 to simpleEdges.[state].Count - 1 do
                    let v, a = simpleEdges.[state].[i]
                    if count >= 0 
                    then vEdges.[count] <- new Edge(v, a)
                    else first <- new Edge(v, a)
                    count <- count + 1

                stateToVertex.[state].OutEdges <- UsualOne<_>(first, vEdges)
                edgesForState.Clear()
                simpleEdges.[state].Clear()

        let clearUsedStates () = 
            for s in usedStates do
                stateToVertex.[s] <- null
            usedStates.Clear()

        let push () = 
            let newAstNodes = (!curTokens).Tokens |> Array.mapi (fun i _ -> new Terminal(tempTokens.Count + i))
            let oldTokens = !curTokens
            (!curTokens).Tokens |> Array.iter (fun t -> tempTokens.Add t.Token)
            let flg = verticesToProcess.Count > 0
            let c = verticesToProcess.Dequeue() |> processChunk
            if flg && c.IsSome 
            then
                curTokens := c.Value
                let t = curTokens.Value
                curLvl := (!curTokens).CurLvl
                isEOF := 
                    (!curTokens).Tokens.Length = 0 
                    || ((!curTokens).Tokens.Length = 1 
                        && parserSource.TokenToNumber (!curTokens).Tokens.[0].Token = parserSource.EofIndex)
            else
                isEOF := verticesToProcess.Count = 0
            let oldPushes = !pushes |> Array.map (fun p -> p.ToArray())
            for i in 0..oldTokens.Tokens.Length-1 do
                let newAstNode = newAstNodes.[i]
                let oldPushes = oldPushes.[i]
                let num = oldTokens.Tokens.[i].NxtLvl
                for (vertex, state) in oldPushes do
                    let newVertex = addVertex [|state, !curLvl,  Some (vertex, newAstNode :> INode)|]
                    addEdge vertex (new Family(0, new Nodes([|newAstNode|]))) edges.[state] |> ignore
//                if pushesMap.ContainsKey num
//                then 
//                    pushesMap.[num].Add((oldPushes,newAstNode)) 
//                else pushesMap.Add(num,new ResizeArray<_>([|oldPushes,newAstNode|]))
            pushesInitFun (!curTokens).Tokens.Length

        let shift () = 
            ()
//            let newAstNodes = (!curTokens).Tokens |> Array.mapi (fun i _ -> new LeafNode(tempTokens.Count + i))
//            let oldTokens = !curTokens
//            (!curTokens).Tokens |> Array.iter (fun t -> tempTokens.Add t.Token)
//            let flg = verticesToProcess.Count > 0
//            let c = verticesToProcess.Dequeue() |> processChunk
//            if flg && c.IsSome 
//            then
//                curTokens := c.Value
//                let t = curTokens.Value
//                curLvl := (!curTokens).CurLvl
//                isEOF := 
//                    (!curTokens).Tokens.Length = 0 
//                    || ((!curTokens).Tokens.Length = 1 
//                        && parserSource.TokenToNumber (!curTokens).Tokens.[0].Token = parserSource.EofIndex)
//            else
//                isEOF := verticesToProcess.Count = 0
//            let oldPushes = !pushes |> Array.map (fun p -> p.ToArray())
//            for i in 0..oldTokens.Tokens.Length-1 do
//                let newAstNode = newAstNodes.[i]
//                let oldPushes = oldPushes.[i]
//                let num = oldTokens.Tokens.[i].NxtLvl
//                if pushesMap.ContainsKey num
//                then 
//                    pushesMap.[num].Add((oldPushes,newAstNode)) 
//                else pushesMap.Add(num,new ResizeArray<_>([|oldPushes,newAstNode|]))
//            pushesInitFun (!curTokens).Tokens.Length
 
        let print (fName:PrintfFormat<_,_,_,_>) =
//#if DEBUG
            let path = fName.Value |> System.IO.Path.GetDirectoryName
            if path |> System.IO.Directory.Exists |> not
            then System.IO.Directory.CreateDirectory path |> ignore
            let vertices = usedStates.ToArray() |> Array.map (fun i -> stateToVertex.[i])
            drawDot parserSource.TokenToNumber tempTokens parserSource.LeftSide vertices parserSource.NumToString parserSource.ErrorIndex
                        <| sprintf fName !curLvl
//#endif
//            ()

        let mutable errorList = []
        let errorRuleExist = parserSource.ErrorRulesExists
        let mutable wasError = false
        let lastErr = ref -1
        let pushesBackup = ref [||]

        let errors = new ResizeArray<_>()
        
        let restorePushes () =
          if pushesMap.ContainsKey !curLvl
          then
              clearUsedStates ()
              for oldPushes,newAstNode in pushesMap.[!curLvl] do
                  for (vertex, state) in oldPushes do
                      let newVertex = addVertex [|state, !curLvl,  Some (vertex, newAstNode :> INode)|]
                      addSimpleEdge vertex newAstNode simpleEdges.[state] |> ignore
              pushesMap.Remove(!curLvl) |> ignore

        let erTok () = 
            //let shiftBase = tokens.Count - (!pushesBackup).Length 
            //let x = !pushesBackup |> Array.mapi (fun i (a:Stack<_>) -> if a.Count = 0  then Some (tokens.Item(shiftBase + i)) else None)
            //rangeToSkip := !curLvl, try pushesMap.Keys |> Seq.min with _ -> !curLvl
            let temp = !curTokens
            let x = !pushesBackup |> Array.mapi (fun i (a:Stack<_>) -> if a.Count = 0 then Some ((!curTokens).Tokens.[i].Token) else None)
            x |> Array.choose id
            |> errors.AddRange
            //(try curTokens.Value.Tokens.[0].Token with _ -> tokens.[tokens.Count-1])

        while not !isEnd && not wasError do
            print @"dot\stack_%A_1"
            if usedStates.Count = 0 && reductions.Count = 0
            then
                if pushesMap.Count = 0
                then
                    let errInfo = !curTokens
                    errorList <- errInfo :: errorList
                    wasError <- true
                else
                    pushesBackup := !pushes
                    erTok()
                    curLvl := pushesMap.Keys |> Seq.min
                    restorePushes ()
                    makeReductions ()
                    attachEdges()
                    if not !isEOF then shift()
            else
                print @"dot\stack_%A_2"
                if !isEOF
                then
                    restorePushes ()
                    makeReductions()
                    attachEdges()
                    print @"dot\stack_%A_3"
                    isEnd := true
                else
                    print @"dot\stack_%A_4"
                    if !curLvl > 0
                    then
                        if pushesMap.ContainsKey !curLvl
                        then restorePushes ()
                        else
                            clearUsedStates ()
                    makeReductions ()
                    attachEdges()
                    let bad, good = !pushes |> Array.partition (fun x -> x.Count = 0)
                    if bad.Length > 0 || good.Length = 0
                    then
                        //()
                        pushesBackup := !pushes 
                        erTok ()
//                    if good.Length = 0
//                    then 
//                        //wasError <- true
//                        pushesBackup := !pushes
//                        erTok ()
//                        if pushesMap.Count > 0 
//                        then ()
//                          //curLvl := pushesMap.Keys |> Seq.min
//                          //restorePushes ()
//                        //else wasError <- true
                    push ()

        let isAcceptState() = 
            let flag = ref false
            for s in usedStates do
                if not !flag 
                then
                    flag := parserSource.AccStates.[s]
            !flag

        // if finish isn't accepting state then error
        if !isEnd && usedStates.Count > 0 && not <| isAcceptState() 
        then
            //recovery()
            makeReductions()
            attachEdges()

        let lastTokens count =
            [| for i = max 0 (tempTokens.Count-count) to tempTokens.Count-1 do
                yield tempTokens.[i]|]
        let debugFuns () =
            let vertices = usedStates.ToArray() |> Array.map (fun i -> stateToVertex.[i])
            {
                drawGSSDot = drawDot parserSource.TokenToNumber tempTokens parserSource.LeftSide vertices parserSource.NumToString parserSource.ErrorIndex
                lastTokens = lastTokens
            }

        if not errorList.IsEmpty 
        then
            errorList <- List.rev errorList
            let tokenToString token = token |> parserSource.TokenToNumber |> parserSource.NumToString
            for i = 0 to errorList.Length-1 do ()
            
        if wasError 
        then
            erTok() 
            Error (!curInd, errors.ToArray() , "Parse Error", debugFuns (), new Dictionary<_,_>())
        else
            let root = ref None
            let addTreeTop res =
                let children = new Family(parserSource.StartRule,  new Nodes(res, null, null))
                new AST(children, null)
            for s in usedStates do
                if parserSource.AccStates.[s] 
                then
                    root :=
                        stateToVertex.[s].OutEdges.first.Ast
                        |> addTreeTop
                        |> Some
            match !root with
            | None ->
                erTok() 
                Error (!curInd, errors.ToArray(), "Input was fully processed, but it's not complete correct string.", debugFuns (), new Dictionary<_,_>())
            | Some res -> 
            //    debugFuns().drawGSSDot "res.dot"
                Success(new Tree<_>(tempTokens.ToArray() , res, parserSource.Rules), errors.ToArray(), new Dictionary<_,_>())