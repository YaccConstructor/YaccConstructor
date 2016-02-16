module Yard.Generators.RNGLR.ReadBack.Parser  

open Yard.Generators.Common
open Yard.Generators.RNGLR.ReadBack
open Yard.Generators.RNGLR.ReadBack.Graphs
open System.Collections.Generic
open FSharpx.Collections.Experimental

type ParserDebugFuns<'TokenType> = {
    drawGSSDot : string -> unit
    /// If you need more then one last token
    lastTokens : int -> 'TokenType[]
}

type ParseReadBackResult<'TokenType> =
    | Success of array<'TokenType>
    | Error of int * array<'TokenType> * string * ParserDebugFuns<'TokenType>

let buildAstReadBack<'TokenType> (parserSource : ParserSourceReadBack<'TokenType>) (tokens : seq<'TokenType>) =
    let enum = tokens.GetEnumerator()
    // Change if it doesn't equal to zero. Now it's true according to states building algorithm
    let startState = 0
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let getEpsilon =
        let epsilons = Array.init parserSource.LeftSide.Length (fun i -> SppfLabel.EpsilonReduction(i))
        fun i -> epsilons.[i]
    // If input stream is empty or consists only of RNGLR_EOF token
    if not <| enum.MoveNext() || parserSource.EofIndex = parserSource.TokenToNumber enum.Current then
        if parserSource.AcceptEmptyInput 
        then
            Success [||]
        else
            Error (0, [||], "This grammar cannot accept empty string",
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
        let edges = Array.init statesCount (fun _ -> new ResizeArray<GssVertex * ReductionTemp>())
        let simpleEdges = Array.init statesCount (fun _ -> new ResizeArray<GssVertex * SppfLabel>())
        let currentReductions : (ReductionTemp option)[] = Array.init parserSource.RightSideNFA.Length (fun _ -> None)

        let pushes = new Stack<_> (statesCount * 2 + 10)
        /// Stores states, used on current level. Instead statesCount must be number of non-terminals, but doesn't matter
        let usedStates = new Stack<_>(statesCount)
        let stateToVertex : (GssVertex option)[] = Array.zeroCreate statesCount

        let addVertex state level (edgeOpt : option<GssVertex * SppfLabel>) =
            let dict = stateToVertex
            let v =
                match dict.[state] with
                | Some v -> v
                | None ->
                    let v = new GssVertex(state, level)
                    dict.[state] <- Some v
                    let push = parserSource.Gotos.[state].[!curNum]
                    // Push to init state is impossible
                    if push <> 0 then
                        pushes.Push (v, push)
                    let arr = parserSource.ZeroReduces.[state].[!curNum]
                    if arr <> null then
                        for prod in arr do
                            reductions.Push (v, prod, 0, None)
                    usedStates.Push state
                    v
            if edgeOpt.IsSome && not (isEpsilonReduction <| snd edgeOpt.Value) then 
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
                if pos = 0 then
                    let state = parserSource.Gotos.[vertex.State].[nonTerm]
                    let newVertex = addVertex state num None
                    let ast = getEpsilon parserSource.LeftSide.[prod]
                    if not <| containsSimpleEdge vertex ast simpleEdges.[state] then
                        addSimpleEdge vertex ast simpleEdges.[state]
                else 
                    let nfa = parserSource.RightSideNFA.[prod]
                    let reductionTemp, rightEnd =
                        match currentReductions.[prod] with
                        | Some x ->
                            match x.TryGetAlreadyVisited nfa.[pos] vertex with
                            | Some y -> x, y
                            | None -> 
                                let y = new VertexWithBackTrack<_,_>((nfa.[pos], vertex))
                                x.AddRightEnd y
                                x, y
                        | None ->         
                            let y = new VertexWithBackTrack<_,_>((nfa.[pos], vertex))                    
                            let x = new ReductionTemp(prod, nfa.Length, y)
                            currentReductions.[prod] <- x
                            x, y
                    
                    //TODO:move

                    //vertex is already in reductionTemp, put there and return it's epsilon closure
                    //NOT SAFE: does not check, if vertex has already been epsilon-closed
                    let rec epsilonClose (vertex : Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>) (closure : ResizeArray<_>) =
                        closure.Add vertex
                        let nfaVertex, gssVertex = vertex.label
                        for edge in nfaVertex.inEdges do
                            if edge.label = parserSource.EpsilonIndex then
                                let prevVertex, goForward =
                                    match reductionTemp.TryGetAlreadyVisited edge.dest gssVertex with
                                    | Some pv -> pv, false
                                    | None -> 
                                        let pv = new Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>((edge.dest, gssVertex))
                                        reductionTemp.AddVisited pv
                                        pv, true
                                prevVertex.addEdge (new Edge<_,_>(vertex, SppfLabel.Epsilon))
                                epsilonClose prevVertex closure

                    let handlePath (path : AstNode[]) (final : Vertex) =
                        if final = null
                        then recovery()//pushes.Clear()
                        else
                            let state = parserSource.Gotos.[final.State].[nonTerm]
                            let newVertex = addVertex state num None
                    
                            let family = new Family(prod, new Nodes(Array.copy path))
                            if not <| containsEdge final family edges.[state] then
                                let isCreated, edgeLabel = addEdge final family edges.[state] false
                                if pos > 0 && isCreated && not (isEpsilon edgeLabel) then
                                    let arr = parserSource.Reduces.[state].[!curNum]
                                    let edgeOpt = Some (final, edgeLabel :> AstNode)
                                    if arr <> null then
                                        for (prod, pos) in arr do
                                            reductions.Push (newVertex, prod, pos, edgeOpt)

                    let rec walk remainLength (vertex : Vertex) path =
                        if remainLength = 0 then handlePath path vertex
                        elif vertex <> null
                        then
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
                                    path.[remainLength - 1] <- a :> AstNode
                                    walk (remainLength - 1) v path
                                    while j < edges.Count && trd edges.[j] = a do
                                        j <- j + 1
                                    i <- j
                        else recovery() //pushes.Clear()

                    //TODO: написать функцию замыкания
                    //TODO: проверить, есть ли уже такая редукция
                    //TODO: добавление и проверка существания дуг - в редукции
                    let nextVertex = new VertexWithBackTrack<_,_>(
                    let firstEdge = new Edge<_,_>(fst edgeOpt.Value, snd edgeOpt.Value)
                    path.[pos - 1] <- snd edgeOpt.Value
                    walk (pos - 1) (fst edgeOpt.Value) path

        let curInd = ref 0
        let isEnd = ref false
        let attachEdges () =
            ()
            (*let inline snd3 (_,x,_) = x
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
                        vEdges.[count] <- new GssEdge(v, a)
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
                simpleEdges.[vertex].Clear()*)

        let shift num =
            let newAstNode = Terminal(tokens.Length)
            tokens.Add enum.Current
            if enum.MoveNext() then
                curToken := enum.Current
                curNum := parserSource.TokenToNumber enum.Current
            else
                curNum := parserSource.EofIndex
            for vertex in usedStates do
                stateToVertex.[vertex] <- None
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
                let newVertex = addVertex state num <| Some (vertex, newAstNode)
                addSimpleEdge vertex newAstNode simpleEdges.[state]
        
        let recovery() = ()

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
        
        let isAcceptState() =                
            usedStates.ToArray()
            |> Array.exists (fun state -> parserSource.AccStates.[state])

        // if finish isn't accepting state then error
        if !isEnd && usedStates.Count > 0 && not <| isAcceptState() 
        then
            if errorRuleExist 
            then 
                 recovery()
                 makeReductions (!curInd + 1) recovery
                 attachEdges()
            else wasError := true

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
                let tree = new Tree<_> (tokens.ToArray(), res :> AstNode, parserSource.Rules)
                //tree.AstToDot parserSource.NumToString parserSource.TokenToNumber None parserSource.LeftSide "../../../Tests/RNGLR/sppf.dot"
                Success (tree, [||], errDict)