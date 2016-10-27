module Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput

open System 
open Microsoft.FSharp.Collections

open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns

type SysDict<'k,'v> = System.Collections.Generic.Dictionary<'k,'v>
type Queue<'t> = System.Collections.Generic.Queue<'t>
type CompressedArray<'t> = Yard.Generators.GLL.ParserCommon.CompressedArray<'t>

/// For debuging
type EdgeOfGSS = 
    {
        startVertex : GSSVertexNFA
        endVertex : GSSVertexNFA
        state : int<state>
        len : uint16
    }
/// For debuging
let printEdges fileName (edges : System.Collections.Generic.HashSet<EdgeOfGSS>) =
    let toPrint = new ResizeArray<_>(["digraph G {\nnode [shape = circle]"])
    let edgs = new ResizeArray<_>()
    let nodes = new ResizeArray<_>()
    
    let getStrFromVertex (v : GSSVertexNFA) = 
        let edgeOfInput = CommonFuns.getEdge v.PositionInInput
        let posOnEdgeOfInput = CommonFuns.getPosOnEdge v.PositionInInput
        
        sprintf "St:%i;Edg:%i;Pos:%i" v.NontermState edgeOfInput posOnEdgeOfInput

    for edge in edges do
        let endName = getStrFromVertex edge.endVertex
        let startName = getStrFromVertex edge.startVertex
        let edgeName = sprintf "ContinueSt:%i,Len:%i" edge.state edge.len

        edgeName |> edgs.Add

        if nodes.Contains endName |> not then
            endName |> nodes.Add
            let nName = sprintf "%i[label=\"%s\"]" (nodes.Count-1) endName
            nName |> toPrint.Add

        if nodes.Contains startName |> not then
            startName |> nodes.Add
            let nName = sprintf "%i[label=\"%s\"]" (nodes.Count-1) startName
            nName |> toPrint.Add

        let startId = nodes.IndexOf startName
        let endId = nodes.IndexOf endName

        let edge = sprintf "%i -> %i [label=\"%s\",color=blue]; \n" startId endId edgeName

        toPrint.Add edge

    toPrint.Add "}"

    System.IO.File.WriteAllLines(fileName, toPrint)



let buildAbstract (parser : FSAParserSourceGLL) (input : BioParserInputGraph) = 
    //let shift = input.Shift
    //let edgesOfGSS = new System.Collections.Generic.HashSet<_>()

    let anyNonterm = parser.NumOfAnyState

    if input.EdgeCount = 0 then failwith ("This grammar does not accept empty input.") else
    let result = new System.Collections.Generic.HashSet<_>()
    let dummyEdge = input.EdgeCount
    let outEdges = 
        let r = Array.init<_> input.VertexCount (fun _ -> new System.Collections.Generic.List<int>())
        for i in 0..input.EdgeCount - 1 do
             r.[input.Edges.[i].Start].Add i
        r

    /// Descriptors
    let setU = new CompressedArray<SysDict<int<state>, int64[]>>(Array.concat([input.ChainLength;[|input.EdgeCount|]]), (fun _ -> null ),0)
    /// Poped elements
    let setP = new SysDict<int64<vertexMeasure>, Yard.Generators.Common.DataStructures.ResizableUsualOne<int<positionInInput>*uint16>>(500)
    /// Edges of GSS:
    /// |vertex| --- stateToContinue, length ---> |vertex|
    let edges = Array.init parser.NonTermCount (fun _ -> new CompressedArray<SysDict<int<state>, SysDict<int<state>, (int<positionInInput> * uint16)[]>>> (input.ChainLength, (fun _ -> null),0))

    /// State of FSA.
    let currentState = ref <| parser.StartState
    /// Position in input graph (packed edge+position).
    let currentIndex = ref (-1<positionInInput>)
    let currentLength = ref 0us
    //let currentLeftPosition = ref -1<leftPosition>
    let currentGSSNode = ref <| new GSSVertexNFA(-1<positionInInput>,-1<state>)
    let currentContext = ref <| new ContextFSA(!currentIndex, !currentState, !currentGSSNode,!currentLength)

    let startContexts = 
        input.InitialPositions
        |> Array.rev
        |> Array.Parallel.map(fun e -> 
            let pos = e * 1<positionInInput>
            //let leftPos = e * 1<leftPosition>
            let vertex = new GSSVertexNFA(pos, !currentState)
            new ContextFSA(pos, !currentState, vertex, !currentLength))
    /// Stack of contexts
    let setR = new System.Collections.Generic.Stack<ContextFSA>(startContexts)  
                 
    /// Checks for existing of context in SetU. If not adds it to SetU.
    let containsContext (inputIndex: int<positionInInput>) (state : int<state>) (vertex : GSSVertexNFA) =
        let vertexKey = CommonFuns.pack vertex.PositionInInput vertex.NontermState
        if setU.[inputIndex] <> null
        then
            let cond, current = setU.[inputIndex].TryGetValue state
            if cond
            then
                if not (current |> Array.contains vertexKey)
                then
                    setU.[inputIndex].[state] <- Array.append current [|vertexKey|]
                    false
                else
                    true
            else
                setU.[inputIndex].Add(state, [|vertexKey|])
                false
        else 
            let dict1 = new SysDict<_, _>()
            setU.[inputIndex] <- dict1
            dict1.Add(state, [|vertexKey|])
            false

    /// Adds new context to stack (setR)
    let pushContext (inputVertex : int<positionInInput>) (state : int<state>) vertex len =
        setR.Push(new ContextFSA(inputVertex, state, vertex, len))

    /// Adds new context to stack (setR) if it is first occurrence of this context (if SetU doesn't contain it).
    let addContext (inputVertex : int<positionInInput>) (state : int<state>) vertex len =
        if not <| containsContext inputVertex state vertex 
        then
            pushContext inputVertex state vertex len

    /// Checks for existing of edge in edges set. If not adds it to edges set.
    let containsEdge (startVertex : GSSVertexNFA) (endVertex : GSSVertexNFA) (state : int<state>) (len : uint16) =
        let outEdges = edges.[int(startVertex.NontermState)].[startVertex.PositionInInput]
        (* debug
        edgesOfGSS.Add(
            {
                startVertex = startVertex
                endVertex = endVertex
                state = state
                len  = len
            }) |> ignore 
        *)
        let cond, dict = 
            if outEdges <> null
            then
                let cond, dictStateKey = outEdges.TryGetValue state
                if cond
                then
                    let cond, posLen = dictStateKey.TryGetValue endVertex.NontermState
                    if cond
                    then
                        if posLen |> Array.contains (endVertex.PositionInInput,len) 
                        then
                            true, None
                        else
                            let newPosLen = Array.append posLen [|endVertex.PositionInInput,len|]
                            dictStateKey.[endVertex.NontermState] <- newPosLen
                                                        
                            false, None
                    else
                        let arr = [|endVertex.PositionInInput,len|]
                        dictStateKey.Add(endVertex.NontermState, arr)
                        false, None 
                else
                    let d1 = new SysDict<int<state>, _>()
                    let arr = [|endVertex.PositionInInput,len|]
                    d1.Add(endVertex.NontermState, arr)
                    outEdges.Add(state, d1)
                    false, None
            else 
                let d1 = new SysDict<int<_>, SysDict<int<state>, _>>()
                let d2 = new SysDict<int<state>, _>()
                let arr = [|endVertex.PositionInInput,len|]
                d2.Add(endVertex.NontermState, arr)
                d1.Add(state, d2)
                false, Some d1
        if dict.IsSome then edges.[int startVertex.NontermState].[startVertex.PositionInInput] <- dict.Value
        cond
        
    ///Creates new descriptors.(Calls when found nonterninal in rule(on current input edge, or on some of next)))
    let create (stateToContinue : int<state>) (nonTermName : int<state>) =
        let index = !currentIndex
        let currentVertex = !currentGSSNode
        let len = !currentLength
        let newVertex = new GSSVertexNFA(index, nonTermName)

        if edges.[int nonTermName].[index] <> null
        then//such GSS vertex already exist
            if not <| containsEdge newVertex currentVertex stateToContinue len
            then//no such edge between vertices
                let vertexKey = packVertexFSA index nonTermName
                let cond, poped = setP.TryGetValue vertexKey
                if cond
                then// aready poped for current index and nonterm
                    // add contexts for each position in input
                    poped.DoForAll (fun (newIndex, l) ->
                        addContext newIndex stateToContinue currentVertex (len + l))
        else//need to create new edge, vertex and context
            containsEdge newVertex currentVertex stateToContinue len |> ignore
            addContext index nonTermName newVertex 0us
            
    /// 
    let pop () =
        let curVertex = !currentGSSNode
        let curIndex = !currentIndex
        let curLen = !currentLength

        let outEdges = edges.[int curVertex.NontermState].[curVertex.PositionInInput]
            
        if outEdges <> null && outEdges.Count <> 0
        then
            let vertexKey = packVertexFSA curVertex.PositionInInput curVertex.NontermState

            let cond, value = setP.TryGetValue vertexKey
            if cond
            then
                value.Add (curIndex, curLen)
            else
                let newList = new ResizableUsualOne<_>((curIndex, curLen))
                setP.Add(vertexKey, newList)

            for stateТoNonterm in outEdges do
                let state = stateТoNonterm.Key  
                for nontermPosLen in stateТoNonterm.Value do
                    let nonterm = nontermPosLen.Key
                    for position,length in nontermPosLen.Value do
                        let adjacentVertex = new GSSVertexNFA(position, nonterm)
                        addContext curIndex state adjacentVertex (curLen + length)

        if curVertex.NontermState = parser.StartState
        then
            let leftEdge, leftPos =
                getEdge curVertex.PositionInInput,
                getPosOnEdge curVertex.PositionInInput
            let rightEdge, rightPos = 
                let edge = getEdge curIndex
                let pos = getPosOnEdge curIndex
                if edge = dummyEdge
                then
                    pos, input.ChainLength.[pos]
                else
                    edge, pos
            result.Add(new ResultStruct(leftEdge,
                                        leftPos,
                                        rightEdge,
                                        rightPos - 1, // this value can be -1, it means that path ends in vertex, not on edge
                                        !currentLength))
            |> ignore

    /// Moves positions in input and grammar by 1.
    let eatTerm nextState curEdge curPos =
        let chainLen = input.ChainLength.[curEdge]
        let newLength = !currentLength + 1us
                
        if (curPos + 1) < chainLen
        then//edge isn't finished(cur token on edgee is on current edge)
            let newIndex = packEdgePos curEdge (curPos + 1)
            pushContext newIndex nextState !currentGSSNode newLength
        else//edge is finished(should take tokens from out edges)
            let oEdges = outEdges.[input.Edges.[curEdge].End]
            if oEdges.Count <> 0
            then 
                oEdges.ForEach (fun outEdge ->
                    let newIndex = packEdgePos outEdge 0
                    pushContext newIndex nextState !currentGSSNode newLength)
            else//reached end of input.
                // put dummyIndex in stack
                let dummyIndex = packEdgePos dummyEdge curEdge
                pushContext dummyIndex nextState !currentGSSNode newLength

    while setR.Count <> 0 do
        currentContext := setR.Pop()
        currentIndex   := currentContext.Value.Index
        currentGSSNode := currentContext.Value.Vertex
        currentState   := currentContext.Value.State
        currentLength  := currentContext.Value.Length
        
        let outEdges = parser.OutNonterms.[int !currentState]

        if !currentState |> parser.FinalStates.Contains
        then// Current state is final
            pop ()
            
        let curEdge = CommonFuns.getEdge !currentIndex
        let curPos = CommonFuns.getPosOnEdge !currentIndex// + shift
            
        if curEdge = dummyEdge then(*we reached end of input*) () else

        let curToken = input.Edges.[curEdge].Tokens.[curPos]
        let dictionaryKey = parser.GetTermsDictionaryKey !currentState curToken
                
        let cond, nextState = parser.StateAndTokenToNewState.TryGetValue dictionaryKey
        if cond
        then
            eatTerm nextState curEdge curPos

        for curNonterm, nextState in outEdges do
            if curNonterm = anyNonterm
            then//found "any" rule
                eatTerm nextState curEdge curPos
            else
//                let curToken = input.Edges.[curEdge].Tokens.[curPos] 
//                if parser.FirstSet.Contains <| parser.GetFirstSetItem (int curNonterm) curToken
//                then// Found nonterninal in rule, and it have derivation starting with current token.
//                    // Create new descriptors.
                create nextState curNonterm

//    printfn "Number of descriptors: %i" !numberOfDescr
//    printfn "Number of reused descriptors: %i" !numberOfReusedDescr 
//    printfn "Number of GSS nodes: %i" !numberOfGSSNodes
//    printfn "Number of GSS edges: %i" !numberOfGSSEdges
    //printEdges "GSS.dot" edgesOfGSS
          
    match result.Count with
        | 0 -> Error ("String was not parsed")
        | _ -> Success1 (Array.ofSeq result)
            