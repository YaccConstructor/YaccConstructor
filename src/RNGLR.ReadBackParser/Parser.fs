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
    | Success of Sppf * array<'TokenType>
    | Error of int * array<'TokenType> * string * ParserDebugFuns<'TokenType>

let buildAstReadBack<'TokenType> (parserSource : ParserSourceReadBack<'TokenType>) (tokens : seq<'TokenType>) =
    let enum = tokens.GetEnumerator()
    // Change if it doesn't equal to zero. Now it's true according to states building algorithm
    let startState = 0
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let getEpsilon =
        let epsilons = Array.init parserSource.LeftSide.Length (fun i -> SppfLabel.EpsilonReduction(i))
        fun i -> epsilons.[i]

    //DEBUG
    let timeStart = System.DateTime.Now

    // If input stream is empty or consists only of RNGLR_EOF token
    if not <| enum.MoveNext() || parserSource.EofIndex = parserSource.TokenToNumber enum.Current then
        if parserSource.AcceptEmptyInput 
        then
            let emptyParse =
                let start = 
                    let gssStart = new GssVertex(0, 0)
                    let nfaStart = new VertexWithBackTrack<int, int>(0)
                    new SppfVertex (nfaStart, gssStart)
                let final =
                    let gssFinal = new GssVertex(1, 0)
                    let nfaFinal = new VertexWithBackTrack<int, int>(0)
                    new SppfVertex(nfaFinal, gssFinal)
                start.addEdge(new SppfEdge(final, SppfLabel.EpsilonReduction -1))
                start, 1, 0, Set.ofArray [|0|]
            Success (emptyParse, [||])
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
        let temporarySppfEdges = new Stack<_>()
        let currentReductions : (ReductionTemp option)[] = Array.init parserSource.RightSideNFA.Length (fun _ -> None)

        let pushes = new Stack<_> (statesCount * 2 + 10)
        /// Stores states, used on current level. Instead statesCount must be number of non-terminals, but doesn't matter
        let usedStates = new Stack<_>(statesCount)
        let stateToVertex : (GssVertex option)[] = Array.zeroCreate statesCount
        //DEBUG
        let maxReductionDepth = ref 0
        let checkAndPrintMaxReductionDepth prod startLevel finalLevel finalState =
            if startLevel - finalLevel > !maxReductionDepth then
                maxReductionDepth := startLevel - finalLevel
                let nonTerm = parserSource.NumToString parserSource.LeftSide.[prod]
                printfn "Prod: %d (%s), start level : %d, final level: %d, final state: % d" prod nonTerm startLevel finalLevel finalState


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
                    if push <> startState then
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
                let nonTermLeftSide = parserSource.LeftSide.[prod]
                if pos = 0 then
                    let state = parserSource.Gotos.[gssVertex.State].[nonTermLeftSide]
                    // goto init state is impossible
                    if state <> startState then
                        let newVertex = addVertex state num None
                        let ast = getEpsilon prod
                        if not <| containsEdge gssVertex ast notAttachedEdges.[state] then
                            addEdge gssVertex ast notAttachedEdges.[state]
                else 
                    //TODO: PERFORMANCE REVIEW
                    //all this downcasts can be no good
                    let nfa = parserSource.RightSideNFA.[prod]
                    let reductionTemp =
                        match currentReductions.[prod] with
                        | Some x -> x
                        | None ->         
                            let x = new ReductionTemp(prod, nfa.Length, num)
                            currentReductions.[prod] <- Some x
                            x
                    
                    let dfsStack = new Stack<_>(20)

                    //vertex is already in reductionTemp, get epsilon-closure of it, put vertices on the way to reductionTemp
                    let epsilonClose (vertex : SppfVertex) =
                        let nfaInitVertex, gssVertex = vertex.nfaVertex, vertex.gssVertex
                        let dict = Array.init nfa.Length (fun _ -> None)
                        let count = ref 1
                        dict.[nfaInitVertex.label] <- Some vertex
                        let searchStack = new Stack<_>(20)
                        let f (vertex : SppfVertex) alreadyVisited =
                            let nfaVertex = vertex.nfaVertex
                            for edge in nfaVertex.inEdges do
                                if parserSource.indexToSymbolType edge.label = SymbolType.Epsilon && dict.[edge.dest.label].IsNone then
                                    let prevVertex, prevAlreadyVisited =
                                        match reductionTemp.TryGetAlreadyVisited (edge.dest :?> VertexWithBackTrack<_,_>) gssVertex with
                                        | Some pv -> pv, true
                                        | None -> 
                                            let pv = new SppfVertex((edge.dest :?> VertexWithBackTrack<_,_>, gssVertex))
                                            reductionTemp.AddVisited pv
                                            pv, false
                                    if not alreadyVisited then
                                        prevVertex.addEdge(new SppfEdge(vertex, SppfLabel.Epsilon))
                                    dict.[edge.dest.label] <- Some prevVertex
                                    incr count
                                    searchStack.Push (prevVertex, prevAlreadyVisited)
                                
                        searchStack.Push (vertex, false)
                        while searchStack.Count > 0 do
                            let vertex, alreadyVisited = searchStack.Pop()
                            f vertex alreadyVisited
                        dict |> Array.choose (fun x -> x)
                        
                    let matchNfaAndGssEdgeLabels (nfaEdge : Edge<_,_>) = function
                        | SppfLabel.Reduction (prod,_)
                        | SppfLabel.EpsilonReduction prod ->
                            nfaEdge.label = parserSource.LeftSide.[prod]
                        | SppfLabel.TemporaryReduction rt ->
                            nfaEdge.label = parserSource.LeftSide.[rt.Production]
                        | SppfLabel.Terminal token ->
                            nfaEdge.label = parserSource.TokenToNumber tokens.[token]
                        | _ -> false

                    let reductionStep (leftNfaVertex : VertexWithBackTrack<int, int>) (leftGssVertex : GssVertex) rightVertex sppfLabel =
                        let prevVertex =
                            match reductionTemp.TryGetAlreadyVisited leftNfaVertex leftGssVertex with
                            | Some pv -> pv
                            | None -> 
                                let pv = new SppfVertex((leftNfaVertex, leftGssVertex))
                                reductionTemp.AddVisited pv                              
                        //TODO: maybe we should check and return, if we have reached left end of a handle, and add edge only in positive case
                        //this automaticaly would save us from useless hanging edges and subsequent search...
                        //But we can entrust this to GC
                                dfsStack.Push(pv)
                                pv
                        let edge = new SppfEdge(rightVertex, sppfLabel)
                        match sppfLabel with
                        | SppfLabel.TemporaryReduction _ -> temporarySppfEdges.Push(prevVertex, edge)
                        | _ -> ()
                        prevVertex.addEdge edge

                    //vertex is already in reductionTemp, put there it's predecessors
                    //NOT SAFE: does not check, if vertex has already been processed
                    let rec reductionDfs (vertex : SppfVertex) =
                        
                        let nfaVertex, gssVertex = vertex.nfaVertex, vertex.gssVertex
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
                                        if gssVertex.otherOutEdges <> null then
                                            gssVertex.otherOutEdges |> Array.iter f
                                else
                                    notAttachedEdges.[gssVertex.State] |> ResizeArray.iter f

                    let rightEnd =
                        match reductionTemp.TryGetAlreadyVisited nfa.[pos] gssVertex with
                        | Some x -> x
                        | None -> 
                                let x = new SppfVertex((nfa.[pos], gssVertex))
                                reductionTemp.AddRightEnd x
                                x
                    let rightEndClosure = epsilonClose rightEnd
                    for vertex in rightEndClosure do
                        let nfaVertex, gssVertex = vertex.nfaVertex, vertex.gssVertex
                        let prevGssVertex, sppfLabel = edgeOpt.Value
                        for edge in nfaVertex.inEdges do
                            if matchNfaAndGssEdgeLabels edge sppfLabel then
                                reductionStep (edge.dest :?> VertexWithBackTrack<_,_>) prevGssVertex vertex sppfLabel

                    while dfsStack.Count > 0 do
                        dfsStack.Pop() |> reductionDfs
                    
                    //handle
                    while reductionTemp.NotHandledLeftEnds.Count > 0 do
                        let leftEnd = reductionTemp.NotHandledLeftEnds.Dequeue()
                        let leftEndGss = leftEnd.gssVertex
                        
                        //it's not a zero reduction
                        if not <| vxEq gssVertex leftEndGss then
                            let state = parserSource.Gotos.[leftEndGss.State].[nonTermLeftSide]
                            // goto init state is impossible
                            if state <> startState then
                                let newVertex = addVertex state num None
                                let sppfLabel = SppfLabel.TemporaryReduction reductionTemp
                        
    //                            if containsEdge gssVertex sppfLabel notAttachedEdges.[state] then
    //                                printf "heh"

                                addEdge leftEndGss sppfLabel notAttachedEdges.[state]
                                let arr = parserSource.Reduces.[state].[!curNum]
                                let edgeOpt = Some (leftEndGss, sppfLabel)
                                if arr <> null then
                                    for (prod, pos) in arr do
                                        reductions.Push (newVertex, prod, pos, edgeOpt)
                                //DEBUG
                                //checkAndPrintMaxReductionDepth prod num gssVertex.Level gssVertex.State

        let curInd = ref 0
        let isEnd = ref false
        let attachEdges () =
            for state in usedStates do
                let edges = notAttachedEdges.[state]
                let count = edges.Count
                let toEdge (gssVertex : GssVertex, sppfLabel) =
                    let sppfLabel =
                        match sppfLabel with
                        | TemporaryReduction rt ->
                            SppfLabel.Reduction (rt.Production, (rt.getLeftEnd gssVertex.Level gssVertex.State, parserSource.RightSideNFA.[rt.Production].Length, rt.EndLevel, rt.AcceptingNfaStates))
                        | _ -> sppfLabel
                    new GssEdge(gssVertex, sppfLabel)
                let gssEdges = edges |> ResizeArray.map toEdge
                if count > 0 then
                    stateToVertex.[state].Value.firstOutEdge <- Some gssEdges.[0]
                    if count > 1 then
                        stateToVertex.[state].Value.otherOutEdges <- ResizeArray.sub gssEdges 1 (count - 1) |> ResizeArray.toArray
                edges.Clear()
            while temporarySppfEdges.Count > 0 do
                let source, edge = temporarySppfEdges.Pop()
                let sppfLabel = 
                    match edge.Label with
                    | TemporaryReduction rt ->
                        SppfLabel.Reduction (rt.Production, (source, parserSource.RightSideNFA.[rt.Production].Length, rt.EndLevel, rt.AcceptingNfaStates))
                    | _ -> edge.Label
                edge.setLabel sppfLabel
            for i = 0 to currentReductions.Length - 1 do
                currentReductions.[i] <- None

        let shift num =
            let newAstNode = Terminal(tokens.Length)
            tokens.Add enum.Current
            if enum.MoveNext() then
                curToken := enum.Current
                curNum := parserSource.TokenToNumber enum.Current
                //DEBUG
                if !curNum = parserSource.EofIndex then
                    printfn "EOF time: %A" (System.DateTime.Now - timeStart)
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
                addEdge vertex newAstNode notAttachedEdges.[state]
        
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
                drawGSSDot = fun _ -> () (*drawDot parserSource.TokenToNumber tokens parserSource.LeftSide vertices parserSource.NumToString parserSource.ErrorIndex*)
                lastTokens = lastTokens
            }
        
        if !wasError 
        then 
            Error (!curInd , [|!curToken|] , "Parse Error", debugFuns ())
        else
            let resSppf = ref None
            for vertex in usedStates do
                if parserSource.AccStates.[vertex] 
                then
                    resSppf :=
                        match stateToVertex.[vertex].Value.firstOutEdge.Value.Label with
                        | SppfLabel.Reduction (_, sppf) -> Some sppf
                        | _ -> None
            match !resSppf with
            | None -> Error (!curInd, [|!curToken|], "Input was fully processed, but it's not complete correct string.", debugFuns ())
            | Some res -> 
                //debugFuns().drawGSSDot "res.dot"
                //tree.AstToDot parserSource.NumToString parserSource.TokenToNumber None parserSource.LeftSide "../../../Tests/RNGLR/sppf.dot"
                Success (res, [||])