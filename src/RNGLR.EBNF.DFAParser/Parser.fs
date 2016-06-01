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
open Yard.Generators.RNGLR.ReadBack.Graphs

type ParserDebugFuns<'TokenType> = {
    drawGSSDot : string -> unit
    /// If you need more then one last token
    lastTokens : int -> 'TokenType[]
}

type ParseResult<'TokenType> =
    | Success of Sppf * array<'TokenType>
    | Error of int * array<'TokenType> * string * ParserDebugFuns<'TokenType>

let drawDot (tokenToNumber : _ -> int) (tokens : BlockResizeArray<_>) (leftSide : int[])
        (initNodes : seq<GssVertex>) (numToString : int -> string) (errInd: int) (path : string) =
    use out = new System.IO.StreamWriter (path)
    let was = new Dictionary<_,_>()
    let levels = new Dictionary<_,_>()
    out.WriteLine "digraph GSS {"
    let print s = out.WriteLine ("    " + s)
    let curNum = ref 0
    print "rankdir=RL"
    let getAstString (ast : SppfLabel) =
        match ast with
        | SppfLabel.Terminal t -> tokens.[t] |> tokenToNumber |> numToString    
        | SppfLabel.Reduction (prod, _) 
        | EpsilonReduction prod -> leftSide.[prod] |> numToString
        | _ -> failwith "Unexpected ast"

    let rec dfs (u : GssVertex) =
        was.Add (u, !curNum)
        if not <| levels.ContainsKey u.Level then
            levels.[u.Level] <- [!curNum]
        else
            levels.[u.Level] <- !curNum :: levels.[u.Level]
        print <| sprintf "%d [label=\"%d\"]" !curNum u.State
        incr curNum
        if u.firstOutEdge.IsSome then
            handleEdge u u.firstOutEdge.Value
            if u.otherOutEdges <> null then
                u.otherOutEdges |> Array.iter (handleEdge u)

    and handleEdge u (e : GssEdge) =
        let v = e.Dest
        if not <| was.ContainsKey v then
            dfs v
        print <| sprintf "%d -> %d [label=\"%s\"]" was.[u] was.[v] (getAstString e.Label)

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
                    let dfaStart = 0
                    new SppfVertex (dfaStart, gssStart)
                let final =
                    let gssFinal = new GssVertex(1, 0)
                    let dfaFinal = 0
                    new SppfVertex(dfaFinal, gssFinal)
                start.addEdge(new SppfEdge(final, SppfLabel.EpsilonReduction -1))
                start, 1, 0, ref (Set.ofArray [|0|])
            (*let tree = sppfToTree<_> 0 emptyParse ([||]) parserSource.LeftSide
                        parserSource.NontermToRule parserSource.RightSideNFA parserSource.EpsilonIndex parserSource.CanInferEpsilon
            Success (tree, [||])*)
            Success(emptyParse, [||])
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
        let currentReductions : (ReductionTemp option)[] = Array.init parserSource.DfaTables.Length (fun _ -> None)

        let pushes = new Stack<_> (statesCount * 2 + 10)
        /// Stores states, used on current level. Instead statesCount must be number of non-terminals, but doesn't matter
        let usedStates = new Stack<_>(statesCount)
        let stateToVertex : (GssVertex option)[] = Array.zeroCreate statesCount
        //DEBUG
        (*let maxReductionDepth = ref 0
        let checkAndPrintMaxReductionDepth prod startLevel finalLevel finalState =
            if startLevel - finalLevel > !maxReductionDepth then
                maxReductionDepth := startLevel - finalLevel
                let nonTerm = parserSource.NumToString parserSource.LeftSide.[prod]
                printfn "Prod: %d (%s), start level : %d, final level: %d, final state: % d" prod nonTerm startLevel finalLevel finalState*)


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
                            reductions.Push (v, prod, -1, None)
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
                if pos = -1 then
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
                    let dfaTable, dfaFinishState = parserSource.DfaTables.[prod], parserSource.DfaFinishStates.[prod]

                    let reductionTemp =
                        match currentReductions.[prod] with
                        | Some x -> x
                        | None ->         
                            let x = new ReductionTemp(prod, dfaTable.Length, dfaFinishState, num)
                            currentReductions.[prod] <- Some x
                            x
                    
                    let dfsStack = new Stack<_>(20)
    
                    let getDfaTransitions (dfaState : int) = function
                        | SppfLabel.Reduction (prod,_)
                        | SppfLabel.EpsilonReduction prod ->
                            dfaTable.[dfaState].[parserSource.LeftSide.[prod]]
                        | SppfLabel.Terminal token ->
                            dfaTable.[dfaState].[parserSource.TokenToNumber tokens.[token]]
                        | _ -> None

                    let reductionStep (leftDfaState : int) (leftGssVertex : GssVertex) rightVertex sppfLabel =
                        let prevVertex =
                            match reductionTemp.TryGetAlreadyVisited leftDfaState leftGssVertex with
                            | Some pv -> pv
                            | None -> 
                                let pv = new SppfVertex(leftDfaState, leftGssVertex)
                                if leftDfaState = dfaFinishState then
                                    reductionTemp.AddLeftEnd pv
                                else
                                    reductionTemp.AddVisited pv                              
                        //TODO: maybe we should check and return, if we have reached left end of a handle, and add edge only in positive case
                        //this automaticaly would save us from useless hanging edges and subsequent search...
                        //But we can entrust this to GC
                                dfsStack.Push(pv)
                                pv
                        let edge = new SppfEdge(rightVertex, sppfLabel)
                        prevVertex.addEdge edge

                    //vertex is already in reductionTemp, put there it's predecessors
                    //NOT SAFE: does not check, if vertex has already been processed
                    let rec reductionDfs (vertex : SppfVertex) =
                        
                        let dfaState, gssVertex = vertex.dfaState, vertex.gssVertex
                        
                        let f label leftGssVertex = 
                            match getDfaTransitions dfaState label with
                            | Some dests ->
                                for leftDfaState in dests do
                                    reductionStep leftDfaState leftGssVertex vertex label
                            | None -> ()
                        if gssVertex.Level <> num then
                            let f (gssEdge : GssEdge) = f gssEdge.Label gssEdge.Dest
                            if gssVertex.firstOutEdge.IsSome then
                                f gssVertex.firstOutEdge.Value
                                if gssVertex.otherOutEdges <> null then
                                    gssVertex.otherOutEdges |> Array.iter f
                        else
                            notAttachedEdges.[gssVertex.State] |> ResizeArray.iter (fun (leftGssVertex, label) -> f label leftGssVertex)

                    let rightEnd =
                        match reductionTemp.TryGetAlreadyVisited pos gssVertex with
                        | Some x -> x
                        | None -> 
                                let x = new SppfVertex(pos, gssVertex)
                                reductionTemp.AddRightEnd x
                                x
                    let prevGssVertex, sppfLabel = edgeOpt.Value
                    match getDfaTransitions pos sppfLabel with
                    | Some dfaDests ->
                        for dfaDest in dfaDests do
                            reductionStep dfaDest prevGssVertex rightEnd sppfLabel
                    | None -> ()

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
                                let sppfLabel = 
                                    SppfLabel.Reduction (
                                        prod, (
                                            leftEnd,
                                            parserSource.DfaTables.[reductionTemp.Production].Length,
                                            reductionTemp.EndLevel,
                                            reductionTemp.AcceptingDfaStates))
                                
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
                //let toEdge (gssVertex : GssVertex, sppfLabel) =
                    //new GssEdge(gssVertex, sppfLabel)
                //let gssEdges = edges |> ResizeArray.map toEdge
                let f i = let dest, label = edges.[i] in new GssEdge(dest, label)
                if edges.Count > 0 then
                    stateToVertex.[state].Value.firstOutEdge <- Some (f 0) 
                if edges.Count > 1 then
                    stateToVertex.[state].Value.otherOutEdges <- Array.init (edges.Count - 1) (fun i -> f (i+1))
                edges.Clear()
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
            let vertices = usedStates.ToArray() |> Array.map (fun i -> stateToVertex.[i]) |> Array.choose (fun x -> x)
            {
                drawGSSDot = drawDot parserSource.TokenToNumber tokens parserSource.LeftSide vertices parserSource.NumToString parserSource.ErrorIndex
                lastTokens = lastTokens
            }
        
        if !wasError 
        then 
            Error (!curInd , [|!curToken|] , "Parse Error", debugFuns ())
        else
            let res = ref None
            for vertex in usedStates do
                if parserSource.AccStates.[vertex] 
                then
                    res :=
                        match stateToVertex.[vertex].Value.firstOutEdge.Value.Label with
                        | SppfLabel.Reduction (startRule, sppf) -> Some (startRule, sppf)
                        | _ -> None
            match !res with
            | None -> Error (!curInd, [|!curToken|], "Input was fully processed, but it's not complete correct string.", debugFuns ())
            | Some (startRule, sppf) -> 
                //debugFuns().drawGSSDot @"C:\temp\help_old.dot"
                //tree.AstToDot parserSource.NumToString parserSource.TokenToNumber None parserSource.LeftSide "../../../Tests/RNGLR/sppf.dot"
                (*let ast = 
                    sppfToTree<'TokenType> startRule sppf (tokens.ToArray()) parserSource.LeftSide parserSource.NontermToRule 
                        parserSource.RightSideNFA parserSource.EpsilonIndex parserSource.CanInferEpsilon

                Success (ast, [||])*)
                Success (sppf, [||])