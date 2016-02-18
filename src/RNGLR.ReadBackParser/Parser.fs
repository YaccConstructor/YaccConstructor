module Yard.Generators.RNGLR.ReadBack.Parser  

open Yard.Generators.Common
open Yard.Generators.RNGLR.ReadBack
open Yard.Generators.RNGLR.ReadBack.Graphs
open System.Collections.Generic
open Microsoft.FSharp.Collections
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
        //let edges = Array.init statesCount (fun _ -> new ResizeArray<GssVertex * ReductionTemp>())
        let notAttachedEdges = Array.init statesCount (fun _ -> new ResizeArray<GssVertex * SppfLabel>())
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
                
                let gssVertex, prod, pos, edgeOpt = reductions.Pop()
                if pos = 0 then
                    let nonTerm = parserSource.LeftSide.[prod]
                    let state = parserSource.Gotos.[gssVertex.State].[nonTerm]
                    let newVertex = addVertex state num None
                    let ast = getEpsilon parserSource.LeftSide.[prod]
                    if not <| containsSimpleEdge gssVertex ast notAttachedEdges.[state] then
                        addSimpleEdge gssVertex ast notAttachedEdges.[state]
                else 
                    let nfa = parserSource.RightSideNFA.[prod]
                    let reductionTemp =
                        match currentReductions.[prod] with
                        | Some x -> x
                        | None ->         
                            let x = new ReductionTemp(prod, nfa.Length)
                            currentReductions.[prod] <- Some x
                            x
                    
                    //vertex is already in reductionTemp, get epsilon-closure of it, put vertices on the way to reductionTemp
                    let epsilonClose (vertex : Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>) =
                        let nfaInitVertex, gssVertex = vertex.label
                        let dict = Array.init nfa.Length (fun _ -> None)
                        let count = ref 1
                        dict.[nfaInitVertex.label] <- Some vertex
                        let rec epsilonClose (vertex : Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>) =
                            let nfaVertex, _ = vertex.label
                            for edge in nfaVertex.inEdges do
                                if parserSource.indexToSymbolType edge.label = SymbolType.Epsilon && dict.[edge.dest.label].IsNone then
                                    let prevVertex =
                                        match reductionTemp.TryGetAlreadyVisited (edge.dest :?> VertexWithBackTrack<_,_>) gssVertex with
                                        | Some pv -> pv
                                        | None -> 
                                            let pv = new Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>((edge.dest :?> VertexWithBackTrack<_,_>, gssVertex))
                                            reductionTemp.AddVisited pv
                                            pv
                                    dict.[edge.dest.label] <- Some prevVertex
                                    incr count
                                    epsilonClose prevVertex
                        dict |> Array.choose (fun x -> x)

                    //vertex is already in reductionTemp, put there it's predecessors
                    //NOT SAFE: does not check, if vertex has already been processed
                    let rec reductionDfs (vertex : Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>) =
                        let nfaVertex, gssVertex = vertex.label
                        for edge in nfaVertex.inEdges do
                            if parserSource.indexToSymbolType edge.label = SymbolType.Epsilon then
                                reductionStep (edge.dest :?> VertexWithBackTrack<_,_>) gssVertex vertex SppfLabel.Epsilon
                            else
                                let f (leftGssVertex, label) = 
                                    if matchNfaAndGssEdgeLabels edge label then
                                        reductionStep (edge.dest :?> VertexWithBackTrack<_,_>) leftGssVertex vertex label
                                if gssVertex.Level <> num then
                                    let f (gssEdge : GssEdge) = f (gssEdge.Dest, gssEdge.Label)
                                    if gssVertex.firstOutEdge.IsSome then
                                        f gssVertex.firstOutEdge.Value
                                        if gssVertex.otherOutEdges != null then
                                            gssVertex.otherOutEdges |> Array.iter f
                                else
                                    notAttachedEdges.[gssVertex.State] |> ResizeArray.iter f

                    and matchNfaAndGssEdgeLabels nfaEdge sppfLabel =
                        match parserSource.indexToSymbolType nfaEdge.label, sppfLabel with
                        | SymbolType.Nonterminal, SppfLabel.Reduction (prod,_)
                        | SymbolType.Nonterminal, SppfLabel.EpsilonReduction prod ->
                            nfaEdge.label = parserSource.LeftSide.[prod]
                        | SymbolType.Terminal, SppfLabel.Terminal term ->
                            nfaEdge.label = term
                        | _ -> false

                    and reductionStep (leftNfaVertex : VertexWithBackTrack<int, int>) leftGssVertex rightVertex sppfLabel =
                        let prevVertex =
                            match reductionTemp.TryGetAlreadyVisited leftNfaVertex leftGssVertex with
                            | Some pv -> pv
                            | None -> 
                                let pv = new Vertex<VertexWithBackTrack<int, int> * GssVertex, SppfLabel>((leftNfaVertex, leftGssVertex))
                                reductionTemp.AddVisited pv                              
                        //TODO: maybe we should check and return, if we have reached left end of a handle, and add edge only in positive case
                        //this automaticaly would save us from useless hanging edges and subsequent search...
                        //But we can entrust this to GC
                                reductionDfs pv
                                pv
                        prevVertex.addEdge (new Edge<_,_>(rightVertex, sppfLabel))

                    let rightEnd =
                        match reductionTemp.TryGetAlreadyVisited nfa.[pos] gssVertex with
                        | Some x -> x
                        | None -> 
                                let x = new Vertex<_,_>((nfa.[pos], gssVertex))
                                reductionTemp.AddRightEnd x
                                x
                    let rightEndClosure = epsilonClose rightEnd
                    for vertex in rightEndClosure do
                        let nfaVertex, gssVertex = vertex.label
                        let prevGssVertex, sppfLabel = edgeOpt.Value
                        for edge in nfaVertex.inEdges do
                            if matchNfaAndGssEdgeLabels edge sppfLabel then
                                reductionStep (edge.dest :?> VertexWithBackTrack<_,_>) prevGssVertex vertex sppfLabel
                    
                    //let handle() =
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