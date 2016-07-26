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
    if input.EdgeCount = 0 then
      Error ("This grammar does not accept empty input.")     
    else
        let parser = parser
        let input = input
        let reused = ref 0
        let descriptorNumber = ref 0
        let dummyGSSNode = new GSSVertexNFA(-1<positionInInput>,-1<state>)
        let outEdges = 
            let r = Array.init<List<int>> input.VertexCount (fun _ -> List<int>.Empty)
            for i in 0..input.EdgeCount - 1 do
                r.[input.Edges.[i].Start] <- i :: r.[input.Edges.[i].Start]
            r

        /// Descriptors
        let setU = new CompressedArray<SysDict<int<state>, SysDict<int64,int64<extension> [] ref>>>(input.ChainLength, (fun _ -> null ),shift)
        /// Poped elements
        let setP = new SysDict<int64<vertexMeasure>, Yard.Generators.Common.DataStructures.ResizableUsualOne<int64<extension>*uint16>>(500)
        
        /// State of FSA.
        let currentState = ref <| parser.StartState
        /// Position in input graph (packed edge+position).
        let currentIndex = ref (-1<positionInInput>)
        let currentLength = ref 0us
        let currentExtension = ref 0L<extension>
        let currentGSSNode = ref dummyGSSNode
        let currentContext = ref <| new ContextFSA(!currentIndex, !currentState, !currentGSSNode, !currentExtension, !currentLength)

        /// Edges of GSS
        let edges = Array.init parser.NonTermCount (fun _ -> new CompressedArray<SysDict<int<state>, SysDict<int64<extension>, SysDict<int<state>, (int<positionInInput> * uint16)[]>>>> (input.ChainLength, (fun _ -> null),shift))
        
        let result = new System.Collections.Generic.HashSet<_>()
        
        let startContexts = 
            input.InitialVertices
            |> Array.rev
            |> Array.Parallel.map(fun e -> 
                let pos: int<positionInInput> = LanguagePrimitives.Int32WithMeasure e
                let t = getEdge pos
                let tt = getPosOnEdge pos + shift
                let ttt = packEdgePos t tt
                let ext = packExtension ttt ttt
                new ContextFSA(pos, !currentState, !currentGSSNode, ext, !currentLength))
        /// Stack of contexts
        let setR = new System.Collections.Generic.Stack<ContextFSA>(startContexts)  
        
        /// Returns key in table by nonterm and token.
        let getKey (state: int<state>) token= 
            int( (int state <<< 16) ||| (token - parser.NonTermCount) )
         
        /// Checks for existing of context in SetU. If not adds it to SetU.
        let containsContext (inputIndex: int<positionInInput>) (state : int<state>) (vertex : GSSVertexNFA) extension =
            let vertexKey = CommonFuns.pack vertex.PositionInInput vertex.NontermState
            if setU.[inputIndex] <> null
            then
                let cond, current = setU.[inputIndex].TryGetValue state
                if  cond
                then
                    if current.ContainsKey vertexKey
                    then
                        let trees = current.[vertexKey]
                        if not <| Array.contains extension !trees
                        then 
                            Array.Resize(trees, (!trees).Length + 1)
                            (!trees).[(!trees).Length - 1] <- extension
                            //trees := Array.reduce` extension
                            false
                        else
                            true
                    else 
//                        let arr = new ResizeArray<int64<extension>>()
//                        arr.Add extension
                        current.Add(vertexKey, ref [|extension|])                    
                        false
                else 
                    let dict = new SysDict<_,_>()
                    setU.[inputIndex].Add(state, dict)
                    //let arr = new ResizeArray<_>()
                    //arr.Add extension
                    dict.Add(vertexKey, ref [|extension|]) 
                    false
            else 
                let dict1 = new SysDict<_, _>()
                setU.[inputIndex] <- dict1
                let dict2 = new SysDict<_, _>()
                dict1.Add(state, dict2)
//                let arr = new ResizeArray<_>()
//                arr.Add extension
                dict2.Add(vertexKey, ref [|extension|])
                false
        

        let pushContext (inputVertex : int<positionInInput>) (state : int<state>) vertex extension len =
            setR.Push(new ContextFSA(inputVertex, state, vertex, extension, len))

        /// Adds new context to stack (setR) if it is first occurrence of this context (if SetU doesn't contain it).
        let addContext (inputVertex : int<positionInInput>) (state : int<state>) vertex extension len =
            if not <| containsContext inputVertex state vertex extension 
            then
                setR.Push(new ContextFSA(inputVertex, state, vertex, extension, len))
                incr descriptorNumber
            else
                incr reused

        /// Checks for existing of edge in edges set. If not adds it to edges set.
        let containsEdge (newVertex : GSSVertexNFA) (v : GSSVertexNFA) (state : int<state>) (extension:int64<extension>) (len : uint16) =
            let outEdges = edges.[int(newVertex.NontermState)].[newVertex.PositionInInput]

            let cond, dict = 
                if outEdges <> null
                then
                    let cond, dictExtensionKey = outEdges.TryGetValue state
                    if cond
                    then
                        let cond, dictNontermKey = dictExtensionKey.TryGetValue extension
                        if cond
                        then
                            let cond, posLen = dictNontermKey.TryGetValue v.NontermState
                            if cond
                            then
                                if Array.contains (v.PositionInInput,len) posLen
                                then
                                    true, None
                                else
                                    let newSlotlevel = Array.append posLen [|v.PositionInInput,len|]
                                    dictNontermKey.[v.NontermState] <- newSlotlevel
                                    false, None
                            else
                                let arr = [|v.PositionInInput,len|]
                                dictNontermKey.Add(v.NontermState, arr)
                                false, None
                        else
                            let arr = [|v.PositionInInput,len|]
                            let d = new SysDict<int<state>, _>()
                            d.Add(v.NontermState, arr)
                            dictExtensionKey.Add(extension, d)
                            false, None    
                    else
                        let d1 = new SysDict<int64<extension>, SysDict<int<state>, _>>()
                        let d2 = new SysDict<int<state>, _>()
                        let arr = [|v.PositionInInput,len|]
                        d2.Add(v.NontermState, arr)
                        d1.Add(extension, d2)
                        outEdges.Add(state, d1)
                        false, None
                else 
                    let d0 = new SysDict<int<state>, SysDict<int64<extension>, SysDict<int<state>, _>>>()
                    let d1 = new SysDict<int64<extension>, SysDict<int<state>, _>>()
                    let d2 = new SysDict<int<state>, _>()
                    let arr = [|v.PositionInInput,len|]
                    d2.Add(v.NontermState, arr)
                    d1.Add(extension, d2)
                    d0.Add(state, d1)
                    false, Some d0
            if dict.IsSome then edges.[int newVertex.NontermState].[newVertex.PositionInInput] <- dict.Value
            cond
        
        ///Creates new descriptors.(Calls when found nonterninal in rule(on current input edge, or on some of next)))
        let create (cE : int) (cP : int) (state : int<state>) (vertex : GSSVertexNFA) (nonTermName : int<state>) (len : uint16) =
            let edgePos = packEdgePos cE cP
            let extension = packExtension (getLeftExtension !currentExtension) edgePos 
            let newVertex = new GSSVertexNFA(edgePos, nonTermName)
            let isContainsEdge = containsEdge newVertex vertex state extension len

            if edges.[int nonTermName].[edgePos] <> null
            then
                if not <| isContainsEdge
                then
                    let vertexKey = packVertexFSA edgePos nonTermName
                    if setP.ContainsKey(vertexKey)
                    then
                        let arr = setP.[vertexKey]
                          
                        arr.DoForAll (fun (ex, l) ->
                            let e = packExtension (getLeftExtension extension) (getRightExtension ex)
                            let right : int<positionInInput> = LanguagePrimitives.Int32WithMeasure <| getRightExtension ex
                            let iiii = packEdgePos (getEdge right) (getPosOnEdge right - shift)
                            addContext iiii state vertex e (len + l))
            else
                let curToken = input.Edges.[cE].Tokens.[cP]
                let key = getKey nonTermName curToken
                let newState = parser.Table.[key]
                addContext !currentIndex newState newVertex (packExtension edgePos edgePos) 0us
            
        /// Adds vertex to setP. For each slot level adds new context.
        let pop () =
            let u = !currentGSSNode
            let i = !currentIndex
            let extension = !currentExtension
            let len = !currentLength

            if u <> dummyGSSNode
            then
                let vertexKey = packVertexFSA u.PositionInInput u.NontermState

                if setP.ContainsKey vertexKey
                then
                    setP.[vertexKey].Add (extension, len)
                else
                    let newList = new ResizableUsualOne<_>((extension, len))
                    setP.Add(vertexKey, newList)

                let outEdges = edges.[int u.NontermState].[u.PositionInInput]
                //foreach label, extension, slot, level
                for stateExt in outEdges do
                    let state = stateExt.Key  
                    for extNonterm in stateExt.Value do
                        let ext = extNonterm.Key
                        for nontermPosLen in extNonterm.Value do
                            let nonterm = nontermPosLen.Key
                            for position,length in nontermPosLen.Value do
                                let l = (getLeftExtension ext)
                                //let tt = getPosOnEdge l
                                let r = (getRightExtension extension)
                                //let ttt = getPosOnEdge r
                                let ext = packExtension l r
                                let newVertex = new GSSVertexNFA(position, nonterm)
                                addContext i state newVertex ext (len + length)

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
                currentExtension := currentContext.Value.Extension
                currentLength := currentContext.Value.Length
                condition := false
            else 
                stop := true  
                  
        let processing () =  
            condition := true
            let state = !currentState
            let index = !currentIndex

            /// Moves positions in input and grammar by 1.
            let eatTerm nextState =
                let pos = (1 + getPosOnEdge !currentIndex)
                let newIndex = packEdgePos (getEdge !currentIndex) pos
                let newLength = !currentLength + 1us
                let le = (getLeftExtension !currentExtension)
                let re = packEdgePos (getEdge newIndex) ((getPosOnEdge newIndex) + shift)
                let newExtension = packExtension le re
                pushContext index nextState !currentGSSNode newExtension newLength

            if Array.isEmpty parser.States.[int state]
            then// Rule finished.
                if state = parser.LastState
                then
                    let leftPos : int<positionInInput> = LanguagePrimitives.Int32WithMeasure <| getLeftExtension  !currentExtension
                    let rightPos  : int<positionInInput> = LanguagePrimitives.Int32WithMeasure <| getRightExtension !currentExtension
                    result.Add(new ResultStruct(getEdge leftPos,
                                                getPosOnEdge leftPos,
                                                getEdge rightPos,
                                                getPosOnEdge rightPos,
                                                !currentLength))
                    |> ignore
                
                pop ()
            else// Rule isn't finished.
                // Process each next edge and state.
                // Although process each edge that comes from the state in wich we can come by epsilon edge.

                //let edgesToProcess = new Queue<int * int<state>>()
                //parser.States.[int state]
                //|> Array.iter (fun x -> edgesToProcess.Enqueue x)

                for curSymbol, nextState in parser.States.[int state] do
                    ///current edge in input
                    //let curSymbol, nextState = edgesToProcess.Dequeue()
                    let cE = CommonFuns.getEdge !currentIndex
                    ///current position on edge in input
                    let cP = CommonFuns.getPosOnEdge !currentIndex + shift
                    let chainLen = input.ChainLength.[cE]

                    if parser.NumIsTerminal curSymbol || parser.NumIsLiteral curSymbol
                    then//current grammar symbol is terminal
                        if cP < chainLen
                        then//edge isn't finished(cur token on edgee is on current edge)
                            let curToken = input.Edges.[cE].Tokens.[cP] 
                            if curToken = curSymbol
                            then eatTerm nextState
                        else//edge is finished(cur token on edgee is not on current edge)
                            let curEdge = 
                                let oEdges = outEdges.[input.Edges.[cE].End]
                                //result will be only one, bacause input doesn't contain vetices with out edges with equal first tokens
                                oEdges
                                |> List.tryFind (fun outEdge -> curSymbol = input.Edges.[outEdge].Tokens.[shift])
                                
                            if curEdge.IsNone then () else
                            // One of out edges starts with token equal with current terminal.
                            // Move positions.
                            eatTerm nextState
                    elif parser.NumIsEpsilon curSymbol
                    then//found epsilon edge
                        pushContext index nextState !currentGSSNode !currentExtension !currentLength
                    else//current grammar symbol is nonterminal
                        let curNonterm = curSymbol*1<state>
                        if cP < chainLen
                        then//edge isn't finished(cur token on edgee is on current edge)
                            // Found nonterninal in rule, and it have derivation starting with current token.
                            // Create new descriptors.
                            create cE cP state !currentGSSNode curNonterm !currentLength
                        else//edge is finished(cur token on edgee is not on current edge)
                            let oEdges = outEdges.[input.Edges.[cE].End]
                            for oe in oEdges do
                                // Found nonterninal in rule, and it have derivation
                                //     starting with first token of one of the out edges.
                                // Create new descriptors for this token.
                                create oe shift state !currentGSSNode curNonterm !currentLength      
                    
        let control () =
             while not !stop do
                if !condition then dispatcher() else processing()
        control()
                 
        match result.Count with
            | 0 -> 
                Error ("String was not parsed")
            | _ -> 
                //printfn "Reused descriptors %d" !reused
                //printfn "All descriptors %d" !descriptorNumber
                Success1 (Array.ofSeq result) 
                                  