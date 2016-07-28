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
     
[<Struct>]
type ResultStruct =
    val le : int
    val lpos : int
    val re : int
    val rpos : int
    val length : uint16
    new (l, l1, r, r1, len) = {le = l; lpos = l1; re = r; rpos = r1; length = len}
    override this.ToString () = "Start:edge:" + (this.le.ToString()) + ";pos:" + (this.lpos.ToString()) + "--" +
                                "Final:edge:" + (this.re.ToString()) + ";pos:" + (this.rpos.ToString())

let buildAbstract (parser : FSAParserSourceGLL) (input : BioParserInputGraph) = 
    let shift = input.Shift
    if input.EdgeCount = 0 then Error ("This grammar does not accept empty input.") else
    let parser = parser
    let input = input
    let result = new System.Collections.Generic.HashSet<_>()
    let dummyGSSNode = new GSSVertexNFA(-1<positionInInput>,-1<state>)
    let outEdges = 
        let r = Array.init<List<int>> input.VertexCount (fun _ -> List<int>.Empty)
        for i in 0..input.EdgeCount - 1 do
            r.[input.Edges.[i].Start] <- i :: r.[input.Edges.[i].Start]
        r

    /// Descriptors
    let setU = new CompressedArray<SysDict<int<state>, SysDict<int64,int<leftPosition> []>>>(input.ChainLength, (fun _ -> null ),shift)
    /// Poped elements
    let setP = new SysDict<int64<vertexMeasure>, Yard.Generators.Common.DataStructures.ResizableUsualOne<int<positionInInput>*uint16>>(500)
    /// Edges of GSS
    let edges = Array.init parser.NonTermCount (fun _ -> new CompressedArray<SysDict<int<state>, SysDict<int<leftPosition>, SysDict<int<state>, (int<positionInInput> * uint16)[]>>>> (input.ChainLength, (fun _ -> null),shift))

    /// State of FSA.
    let currentState = ref <| parser.StartState
    /// Position in input graph (packed edge+position).
    let currentIndex = ref (-1<positionInInput>)
    let currentLength = ref 0us
    let currentLeftPosition = ref -1<leftPosition>
    let currentGSSNode = ref dummyGSSNode
    let currentContext = ref <| new ContextFSA(!currentIndex, !currentState, !currentGSSNode, !currentLeftPosition, !currentLength)

    let startContexts = 
        input.InitialVertices
        |> Array.rev
        |> Array.Parallel.map(fun e -> 
            let pos = e * 1<positionInInput>
            let leftPos = e * 1<leftPosition>
            new ContextFSA(pos, !currentState, !currentGSSNode, leftPos, !currentLength))
    /// Stack of contexts
    let setR = new System.Collections.Generic.Stack<ContextFSA>(startContexts)  
                 
    /// Checks for existing of context in SetU. If not adds it to SetU.
    let containsContext (inputIndex: int<positionInInput>) (state : int<state>) (vertex : GSSVertexNFA) (leftPos : int<leftPosition>) =
        let vertexKey = CommonFuns.pack vertex.PositionInInput vertex.NontermState
        if setU.[inputIndex] <> null
        then
            let cond, current = setU.[inputIndex].TryGetValue state
            if  cond
            then
                if current.ContainsKey vertexKey
                then
                    let poss = current.[vertexKey]
                    if not (poss |> Array.contains leftPos)
                    then
                        current.[vertexKey] <- Array.append poss [|leftPos|]
                        false
                    else
                        true
                else
                    current.Add(vertexKey, [|leftPos|])                    
                    false
            else 
                let dict = new SysDict<_,_>()
                setU.[inputIndex].Add(state, dict)
                dict.Add(vertexKey, [|leftPos|]) 
                false
        else 
            let dict1 = new SysDict<_, _>()
            setU.[inputIndex] <- dict1
            let dict2 = new SysDict<_, _>()
            dict1.Add(state, dict2)
            dict2.Add(vertexKey, [|leftPos|])
            false

    let pushContext (inputVertex : int<positionInInput>) (state : int<state>) vertex extension len =
        setR.Push(new ContextFSA(inputVertex, state, vertex, extension, len))

    /// Adds new context to stack (setR) if it is first occurrence of this context (if SetU doesn't contain it).
    let addContext (inputVertex : int<positionInInput>) (state : int<state>) vertex (leftPosition : int<leftPosition>) len =
        if not <| containsContext inputVertex state vertex leftPosition 
        then
            setR.Push(new ContextFSA(inputVertex, state, vertex, leftPosition, len))

    /// Checks for existing of edge in edges set. If not adds it to edges set.
    let containsEdge (startVertex : GSSVertexNFA) (endVertex : GSSVertexNFA) (state : int<state>) (leftPosition:int<leftPosition>) (len : uint16) =
        let outEdges = edges.[int(startVertex.NontermState)].[startVertex.PositionInInput]

        let cond, dict = 
            if outEdges <> null
            then
                let cond, dictExtensionKey = outEdges.TryGetValue state
                if cond
                then
                    let cond, dictNontermKey = dictExtensionKey.TryGetValue leftPosition
                    if cond
                    then
                        let cond, posLen = dictNontermKey.TryGetValue endVertex.NontermState
                        if cond
                        then
                            if Array.contains (endVertex.PositionInInput,len) posLen
                            then
                                true, None
                            else
                                let newPosLen = Array.append posLen [|endVertex.PositionInInput,len|]
                                dictNontermKey.[endVertex.NontermState] <- newPosLen
                                false, None
                        else
                            let arr = [|endVertex.PositionInInput,len|]
                            dictNontermKey.Add(endVertex.NontermState, arr)
                            false, None
                    else
                        let arr = [|endVertex.PositionInInput,len|]
                        let d = new SysDict<int<state>, _>()
                        d.Add(endVertex.NontermState, arr)
                        dictExtensionKey.Add(leftPosition, d)
                        false, None    
                else
                    let d1 = new SysDict<int<_>, SysDict<int<state>, _>>()
                    let d2 = new SysDict<int<state>, _>()
                    let arr = [|endVertex.PositionInInput,len|]
                    d2.Add(endVertex.NontermState, arr)
                    d1.Add(leftPosition, d2)
                    outEdges.Add(state, d1)
                    false, None
            else 
                let d0 = new SysDict<int<state>, SysDict<int<_>, SysDict<int<state>, _>>>()
                let d1 = new SysDict<int<_>, SysDict<int<state>, _>>()
                let d2 = new SysDict<int<state>, _>()
                let arr = [|endVertex.PositionInInput,len|]
                d2.Add(endVertex.NontermState, arr)
                d1.Add(leftPosition, d2)
                d0.Add(state, d1)
                false, Some d0
        if dict.IsSome then edges.[int startVertex.NontermState].[startVertex.PositionInInput] <- dict.Value
        cond
        
    ///Creates new descriptors.(Calls when found nonterninal in rule(on current input edge, or on some of next)))
    let create (cE : int) (cP : int) (stateToPop : int<state>) (vertexToPop : GSSVertexNFA) (nonTermName : int<state>) (len : uint16) =
        let index = packEdgePos cE cP
        let leftPosition = !currentLeftPosition
        let newVertex = new GSSVertexNFA(index, nonTermName)

        if edges.[int nonTermName].[index] <> null
        then//such vertex already exist
            if not <| containsEdge newVertex vertexToPop stateToPop leftPosition len
            then//no such edge between vertices
                let vertexKey = packVertexFSA index nonTermName
                if setP.ContainsKey(vertexKey)
                then// aready poped for current index and nonterm
                    // add contexts for each position in input
                    let arr = setP.[vertexKey]
                          
                    arr.DoForAll (fun (newIndex, l) ->
                        addContext newIndex stateToPop vertexToPop leftPosition (len + l))
        else//need to create new edge, vertex and context
            containsEdge newVertex vertexToPop stateToPop leftPosition len |> ignore
            addContext index nonTermName newVertex leftPosition 0us
            
    /// 
    let pop () =
        let curVertex = !currentGSSNode
        let curIndex = !currentIndex
        let curLeftPos = !currentLeftPosition
        let curLen = !currentLength

        if not ((curVertex.NontermState = -1<state>) && (curVertex.PositionInInput = -1<positionInInput>))
        then
            let vertexKey = packVertexFSA curVertex.PositionInInput curVertex.NontermState

            if setP.ContainsKey vertexKey
            then
                setP.[vertexKey].Add (curIndex, curLen)
            else
                let newList = new ResizableUsualOne<_>((curIndex, curLen))
                setP.Add(vertexKey, newList)

            let outEdges = edges.[int curVertex.NontermState].[curVertex.PositionInInput]
                
            for stateExt in outEdges do
                let state = stateExt.Key  
                for leftPosNonterm in stateExt.Value do
                    let leftPos = leftPosNonterm.Key
                    for nontermPosLen in leftPosNonterm.Value do
                        let nonterm = nontermPosLen.Key
                        for position,length in nontermPosLen.Value do
                            let adjacentVertex = new GSSVertexNFA(position, nonterm)
                            addContext curIndex state adjacentVertex leftPos (curLen + length)

    let condition = ref true 
    let stop = ref false

    let dispatcher () =
        let get() = setR.Pop()

        if setR.Count <> 0
        then// Queue of descriptors isn't empty.
            currentContext := get()
            currentIndex := currentContext.Value.Index
            currentGSSNode := currentContext.Value.Vertex
            currentState := currentContext.Value.State
            currentLeftPosition := currentContext.Value.LeftPos
            currentLength := currentContext.Value.Length
            condition := false
        else 
            stop := true  
                  
    let processing () =  
        condition := true
        let state = !currentState
        let index = !currentIndex

        /// Moves positions in input and grammar by 1.
        let eatTerm nextState curEdge curPos =
            let chainLen = input.ChainLength.[curEdge]
            let newLength = !currentLength + 1us
                
            if (curPos + 1) < chainLen
            then//edge isn't finished(cur token on edgee is on current edge)
                let newIndex = packEdgePos curEdge (curPos + 1)
                pushContext newIndex nextState !currentGSSNode !currentLeftPosition newLength
            else//edge is finished(cur token on edgee is not on current edge)
                let oEdges = outEdges.[input.Edges.[curEdge].End]
                oEdges
                |> List.iter (fun outEdge ->
                    let newIndex = packEdgePos outEdge 0
                    pushContext newIndex nextState !currentGSSNode !currentLeftPosition newLength)

        if Array.isEmpty parser.States.[int state]
        then// Rule finished.
            if state = parser.FinalState
            then
                //let leftPos = (getLeftExtension  !currentExtension) * 1<positionInInput>
                //let rightPos = (getRightExtension !currentExtension) * 1<positionInInput>
                let leftPos = (int !currentLeftPosition) * 1<positionInInput>
                let rightPos = !currentIndex
                result.Add(new ResultStruct(getEdge leftPos,
                                            getPosOnEdge leftPos,
                                            getEdge rightPos,
                                            getPosOnEdge rightPos - 1, // this value can be -1, it means that path ends in vertex, not on current edge
                                            !currentLength))
                |> ignore
                
            pop ()
        else// Rule isn't finished.
            // Process each next edge and state.
            // Although process each edge that comes from the state in wich we can come by epsilon edge.
            for curSymbol, nextState in parser.States.[int state] do
                let curEdge = CommonFuns.getEdge !currentIndex
                let curPos = CommonFuns.getPosOnEdge !currentIndex// + shift

                if parser.NumIsTerminal curSymbol
                then//current grammar symbol is terminal
                    let curToken = input.Edges.[curEdge].Tokens.[curPos] 
                    if curToken = curSymbol
                    then eatTerm nextState curEdge curPos
                elif parser.NumIsEpsilon curSymbol
                then//found epsilon edge
                    pushContext index nextState !currentGSSNode !currentLeftPosition !currentLength
                else//current grammar symbol is nonterminal
                    let curNonterm = curSymbol*1<state>
                    let curToken = input.Edges.[curEdge].Tokens.[curPos] 
                    if parser.FirstSet.Contains <| parser.GetFirstSetItem (int curNonterm) curToken
                    then// Found nonterninal in rule, and it have derivation starting with current token.
                        // Create new descriptors.
                        create curEdge curPos nextState !currentGSSNode curNonterm !currentLength    

    let control () =
            while not !stop do
            if !condition then dispatcher() else processing()
    control()
                 
    match result.Count with
        | 0 -> Error ("String was not parsed")
        | _ -> Success1 (Array.ofSeq result)
            