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
    //let shift = input.Shift
    let maxlen = 370us
    if input.EdgeCount = 0 then Error ("This grammar does not accept empty input.") else
    let result = new System.Collections.Generic.HashSet<_>()
    let dummyEdge = input.EdgeCount
    let outEdges = 
        let r = Array.init<_> input.VertexCount (fun _ -> new ResizeArray<int>())
        for i in 0..input.EdgeCount - 1 do
             r.[input.Edges.[i].Start].Add i
        r

    /// Descriptors
    //let setU = new CompressedArray<SysDict<int<state>, int64<vertexMeasure>[]>>(Array.concat([input.ChainLength;[|input.EdgeCount|]]), (fun _ -> null ),0)
    /// Poped elements
    //let setP = new SysDict<int64<vertexMeasure>, Yard.Generators.Common.DataStructures.ResizableUsualOne<int<positionInInput>*uint16>>(500)
    /// Edges of GSS:
    /// |vertex| --- stateToContinue, length ---> |vertex|
    //let edges = Array.init parser.NonTermCount (fun _ -> new CompressedArray<SysDict<int<state>, SysDict<int<state>, (int<positionInInput> * uint16)[]>>> (input.ChainLength, (fun _ -> null),0))

    let SetU (_,_,u) = u
    /// Poped elements
    let SetP (_,p,_) = p
    /// Edges of GSS:
    /// --- stateToContinue, length ---> |vertex|
    let Edges (e,_,_) = e


    let GSS = Array.init parser.NonTermCount (fun _ ->
            new CompressedArray<(SysDict<int<state>, // vertex.Nonterm
                                         SysDict<int<positionInInput>, //vertex.posInInput
                                                 (int<state> * uint16)[]>>) ref //edges
                                *
                                (ResizableUsualOne<int<positionInInput>*uint16>) option ref //SetP
                                *
                                (SysDict<int<state>, int<positionInInput>[]>) ref //SetU
                                > (input.ChainLength, (fun _ -> ref null,ref None,ref null),0))

    /// State of FSA.
    let currentState = ref <| parser.StartState
    /// Position in input graph (packed edge+position).
    let currentIndex = ref (-1<positionInInput>)
    let currentLength = ref 0us
    //let currentLeftPosition = ref -1<leftPosition>
    let currentGSSNode = ref <| new GSSVertexNFA(-1<positionInInput>,-1<state>)
    let currentContext = ref <| new ContextFSA(!currentIndex, !currentState, !currentGSSNode,!currentLength)

    let startContexts = 
        input.InitialVertices
        |> Array.rev
        |> Array.Parallel.map(fun e -> 
            let pos = e * 1<positionInInput>
            //let leftPos = e * 1<leftPosition>
            let vertex = new GSSVertexNFA(pos, !currentState)
            new ContextFSA(pos, !currentState, vertex, !currentLength))
    /// Stack of contexts
    let setR = new System.Collections.Generic.Stack<ContextFSA>(startContexts)  
                 
    /// Checks for existing of context in SetU. If not adds it to SetU.
    let containsContext (inputIndex: int<positionInInput>) (state : int<state>) (vertexNontermState : int<state>) (vertexPositionInInput : int<positionInInput>) =
        let setU = SetU GSS.[int vertexNontermState].[vertexPositionInInput]
        if !setU <> null
        then
            let cond, current = (!setU).TryGetValue state
            if  cond
            then
                if current |> Array.contains inputIndex
                then
                    true
                else
                    (!setU).[state] <- Array.append current [|inputIndex|]
                    false
            else
                (!setU).Add(state, [|inputIndex|])
                false
        else
            let dict1 = new SysDict<_, _>()
            dict1.Add (state,[|inputIndex|])
            setU := dict1
            false
    /// Adds new context to stack (setR)
    let pushContext (inputVertex : int<positionInInput>) (state : int<state>) vertex len =
        
        setR.Push(new ContextFSA(inputVertex, state, vertex, len))

    /// Adds new context to stack (setR) if it is first occurrence of this context (if SetU doesn't contain it).
    let addContext (inputVertex : int<positionInInput>) (state : int<state>) (vertexPositionInInput : int<positionInInput>) (vertexNontermState : int<state>) len =
        if not <| containsContext inputVertex state vertexNontermState vertexPositionInInput
        then
            pushContext inputVertex state (new GSSVertexNFA(vertexPositionInInput, vertexNontermState)) len
    
    /// Checks for existing of edge in edges set. If not adds it to edges set.
    let containsEdge (startVertexPositionInInput : int<positionInInput>) (startVertexState : int<state>) (endVertex : GSSVertexNFA) (state : int<state>) (len : uint16) =
        let outEdges = Edges GSS.[int startVertexState].[startVertexPositionInInput]

        let cond, dict = 
            if !outEdges <> null
            then
                let cond, posStatelen = (!outEdges).TryGetValue endVertex.NontermState
                if cond
                then
                    let cond, statelen = posStatelen.TryGetValue endVertex.PositionInInput
                    if cond
                    then
                        if statelen |> Array.contains (state,len) 
                        then
                            true, None
                        else
                            let newStatelen = Array.append statelen [|state,len|]
                            posStatelen.[endVertex.PositionInInput] <- newStatelen
                            false, None
                    else
                        let arr = [|state,len|]
                        posStatelen.Add(endVertex.PositionInInput, arr)
                        false, None 
                else
                    let arr = [|state,len|]
                    let d1 = new SysDict<_, _>()
                    d1.Add(endVertex.PositionInInput, arr)
                    (!outEdges).Add(endVertex.NontermState, d1)
                    false, None
            else 
                let arr = [|state,len|]
                let d1 = new SysDict<_, _>()
                d1.Add(endVertex.PositionInInput, arr)
                let d2 = new SysDict<_, SysDict<_, _>>()
                d2.Add(endVertex.NontermState, d1)
                false, Some d2
        if dict.IsSome then outEdges := dict.Value
        cond
        
    ///Creates new descriptors.(Calls when found nonterninal in rule(on current input edge, or on some of next)))
    let create (stateToContinue : int<state>) (nonTermName : int<state>) =
        let index = !currentIndex
        let currentVertex = !currentGSSNode
        let len = !currentLength
        //let newVertex = new GSSVertexNFA(index, nonTermName)
        let GSSNode = GSS.[int nonTermName].[index]

        if !(Edges GSSNode) <> null
        then//such vertex already exist
            if not <| containsEdge index nonTermName currentVertex stateToContinue len
            then//no such edge between vertices
                if (!(SetP GSSNode)).IsSome
                    then// aready poped for current index and nonterm
                        // add contexts for each position in input
                        (!(SetP GSSNode)).Value.DoForAll (fun (newIndex, l) ->
                            addContext newIndex stateToContinue currentVertex.PositionInInput currentVertex.NontermState (len + l))
        else//need to create new edge, vertex and context
            containsEdge index nonTermName currentVertex stateToContinue len |> ignore
            addContext index nonTermName index nonTermName 0us
            
    /// 
    let pop () =
        let curVertex = !currentGSSNode
        let curIndex = !currentIndex
        let curLen = !currentLength
        let setP = SetP GSS.[int curVertex.NontermState].[curVertex.PositionInInput]
        if curVertex.NontermState <> parser.StartState
        then
            if (!(setP)).IsSome
            then
                (!setP).Value.Add (curIndex, curLen)
            else
                setP := new ResizableUsualOne<_>((curIndex, curLen)) |> Some

            let outEdges = Edges GSS.[int curVertex.NontermState].[curVertex.PositionInInput]
                
            for keyValue1 in !outEdges do
                let nonterm = keyValue1.Key 
                for keyValue2 in keyValue1.Value do
                    let position = keyValue2.Key
                    for state,length in keyValue2.Value do
//                        if curLen + length > maxlen then
//                            ()
//                        else
                        addContext curIndex state position nonterm (curLen + length)
        else//Nowhere to pop. Reached end of start state.
            //  Need to add derivation to results.
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

        //if newLength > maxlen then () else
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

    let condition = ref true 
    let stop = ref false

    let dispatch () =
        let get() = setR.Pop()

        if setR.Count <> 0
        then// Queue of descriptors isn't empty.
            currentContext := get()
            currentIndex := currentContext.Value.Index
            currentGSSNode := currentContext.Value.Vertex
            currentState := currentContext.Value.State
            currentLength := currentContext.Value.Length
            condition := false
        else 
            stop := true  
                  
    let handle () =  
        condition := true
        let outEdges = parser.States.[int !currentState]

        if Array.isEmpty outEdges
        then// Rule finished.
            pop ()
        else// Rule isn't finished.
            // Process each next edge and state.
            // Although process each edge that comes from the state in wich we can come by epsilon edge.
            let curEdge = CommonFuns.getEdge !currentIndex
            let curPos = CommonFuns.getPosOnEdge !currentIndex// + shift

            if curEdge = dummyEdge then ()(*we reached end of input, but current state is not final*) else

            for curSymbol, nextState in outEdges do
                if parser.NumIsTerminal curSymbol
                then//current grammar symbol is terminal
                    let curToken = input.Edges.[curEdge].Tokens.[curPos] 
                    if curToken = curSymbol
                    then eatTerm nextState curEdge curPos
                elif parser.NumIsEpsilon curSymbol
                then//found epsilon edge
                    pushContext !currentIndex nextState !currentGSSNode !currentLength
                elif (parser.NumToString curSymbol).Equals("any")
                then//found any rule
                    eatTerm nextState curEdge curPos
                else//current grammar symbol is nonterminal
                    let curNonterm = curSymbol*1<state>
                    (*let curToken = input.Edges.[curEdge].Tokens.[curPos] 
                    if parser.FirstSet.Contains <| parser.GetFirstSetItem (int curNonterm) curToken
                    then// Found nonterninal in rule, and it have derivation starting with current token.
                        // Create new descriptors.
                        create nextState curNonterm  *) //current 16s grammar first set contains all states and tokens
                    create nextState curNonterm

    let control () =
            while not !stop do
            if !condition then dispatch() else handle()
    control()
    (*
    let lens = Array.init (10000) (fun _ -> 0)
    
    GSS
    |> Array.iter (fun x ->
        input.ChainLength
        |> Array.iteri (fun i l ->
            for j in 0..l-1 do
                let s = CommonFuns.packEdgePos i j
                let len = 
                    if (!(SetP (x.[s])) ).IsSome
                    then 
                        (!(SetP (x.[s]))).Value.Length()
                    else
                        0
                if len >= 10000 then printfn "%i" len
                else lens.[len] <- lens.[len] + 1)
                )

    lens
    |> Array.mapi (fun i x -> i.ToString() + " : " + x.ToString())
    |> (fun x -> System.IO.File.AppendAllLines("C:\\YCInfernal\\res", x))
    *)
                
    match result.Count with
        | 0 -> Error ("String was not parsed")
        | _ -> Success1 (Array.ofSeq result)
            