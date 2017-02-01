module Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput

open System 
open Microsoft.FSharp.Collections

open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns
open YC.GLL.GSS

type SysDict<'k,'v> = System.Collections.Generic.Dictionary<'k,'v>
type Queue<'t> = System.Collections.Generic.Queue<'t>

let buildAbstract (parser : FSAParserSourceGLL) (input : BioParserInputGraph) = 

    let anyNonterm = parser.NumOfAnyState

    if input.EdgeCount = 0 then failwith ("This grammar does not accept empty input.") else
    let result = new System.Collections.Generic.HashSet<_>()
    let dummyEdge = input.EdgeCount
    let outEdges = 
        let r = Array.init<_> input.VertexCount (fun _ -> new System.Collections.Generic.List<int>())
        for i in 0..input.EdgeCount - 1 do
             r.[input.Edges.[i].Start].Add i
        r

    let gss = new GSS()

    /// State of FSA.
    let currentState = ref <| parser.StartState
    /// Position in input graph (packed edge+position).
    let currentIndex = ref (-1<positionInInput>)
    let currentLength = ref 0us
    //let currentLeftPosition = ref -1<leftPosition>
    let currentGSSNode = ref <| new GSSVertex(-1<positionInGrammar>, -1<positionInInput>)
    let currentContext = ref <| new ContextFSA<GSSVertex>(!currentIndex, !currentState, !currentGSSNode,!currentLength)

    let startContexts = 
        input.InitialPositions
        |> Array.rev
        |> Array.map(fun e -> 
            let pos = e * 1<positionInInput>
            let vertex = new GSSVertex(!currentState, pos)
            gss.AddVertex vertex |> ignore
            new ContextFSA<_>(pos, !currentState, vertex, !currentLength))

    /// Stack of contexts
    let setR = new System.Collections.Generic.Stack<ContextFSA<_>>(startContexts)  
                 
    /// Adds new context to stack (setR)
    let pushContext posInInput posInGrammar gssVertex len =
        setR.Push(new ContextFSA<_>(posInInput, posInGrammar, gssVertex, len))

    /// Adds new context to stack (setR) if it is first occurrence of this context (if SetU doesn't contain it).
    let addContext posInInput posInGrammar (gssVertex:GSSVertex) len =
        if not <| gssVertex.ContainsContext posInInput posInGrammar
        then pushContext posInInput posInGrammar gssVertex len
    
    ///Creates new descriptors.(Calls when found nonterninal in rule(on current input edge, or on some of next)))
    let create stateToContinue posInGrammar =
        let index = !currentIndex
        let currentVertex = !currentGSSNode
        let len = !currentLength
        let newVertex = new GSSVertex(posInGrammar, index)
        let exists, startV = gss.ContainsEdge(newVertex, currentVertex, stateToContinue, len)        

        if startV.P.Count > 0
        then startV.P |> ResizeArray.iter(fun (newIndex, l) -> addContext newIndex stateToContinue currentVertex (len + l))        
        else addContext index posInGrammar startV 0us
            
    /// 
    let pop () =
        let curVertex = !currentGSSNode
        let curIndex = !currentIndex
        let curLen = !currentLength

        let outEdges = gss.OutEdges curVertex |> Array.ofSeq
            
        if outEdges <> null && outEdges.Length <> 0
        then
            let vertexKey = packVertexFSA curVertex.PositionInInput curVertex.PositionInGrammar
            let value = curVertex.P.Add (curIndex, curLen)
            for e in outEdges do
                addContext curIndex e.Tag.StateToContinue e.Target (curLen + e.Tag.LengthOfProcessedString)

        if curVertex.PositionInGrammar = parser.StartState
        then
            let leftEdge, leftPos =
                getEdge curVertex.PositionInInput,
                getPosOnEdge curVertex.PositionInInput
            let rightEdge, rightPos = 
                let edge = getEdge curIndex
                let pos = getPosOnEdge curIndex
                if edge = dummyEdge
                then pos, input.ChainLength.[pos]
                else edge, pos
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
                //put dummyIndex in stack
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
        then eatTerm nextState curEdge curPos

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
            