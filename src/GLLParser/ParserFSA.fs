module Yard.Generators.GLL.ParserFSA

open System 
open Microsoft.FSharp.Collections

open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns
open Yard.Generators.GLL.MeasureTypes
open System.Collections.Generic
open FSharpx.Collections.Experimental

type CompressedArray<'t> = Yard.Generators.GLL.ParserCommon.CompressedArray<'t>

/// For debuging
type EdgeOfGSS = 
    {
        startVertex : GSSVertexFSA
        endVertex : GSSVertexFSA
        state : int<state>
        len : uint16
    }
/// For debuging
let printEdges fileName (edges : System.Collections.Generic.HashSet<EdgeOfGSS>) =
    let toPrint = new ResizeArray<_>(["digraph G {\nnode [shape = circle]"])
    let edgs = new ResizeArray<_>()
    let nodes = new ResizeArray<_>()
    
    let getStrFromVertex (v : GSSVertexFSA) = 
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
    
let buildAST (parser : FSAParserSourceGLL) (input : seq<int>) = 
    //let shift = input.Shift
    //let edgesOfGSS = new System.Collections.Generic.HashSet<_>()
    //let anyNonterm = parser.NumOfAnyState
    let input = input |> Array.ofSeq

    if input.Length = 0 then failwith ("Input sequence is empty.") else
    let epsilon = -1
    let result = ref None
    let endOfInput = input.Length * 1<positionInInput>
    

    /// Descriptors
    let setU = Array.zeroCreate<Dictionary<int<state>, Dictionary<int64, int<node>[]>>> (input.Length + 1)
        //new CompressedArray<Dictionary<int<state>, int64[]>>(Array.concat([input.ChainLength;[|input.EdgeCount|]]), (fun _ -> null ),0)
    /// Poped elements
    let setP = new Dictionary<int64<vertexMeasure>, Yard.Generators.Common.DataStructures.ResizableUsualOne<int<node>>>(500)
    /// Edges of GSS:
    /// |vertex| --- stateToContinue, node ---> |vertex|
    let edges = Array.init parser.NonTermCount (fun _ -> Array.zeroCreate<Dictionary<int<state>, Dictionary<int<state>, (int<positionInInput> * int<node>)[]>>> input.Length)

    /// State of FSA.
    let currentState = ref <| parser.StartState
    /// Position in input.
    let currentIndex = ref (-1<positionInInput>)
    //let currentLength = ref 0us
    let currentGSSNode = ref <| new GSSVertexFSA(-1<positionInInput>,-1<state>)
    
    let dummyNode = -1<node>
    let dummyAST = new TerminalNode(-1, packExtension -1 -1)
    let currentN = ref dummyNode
    let currentR = ref dummyNode

    let packedNodes = new Dictionary<int, int<node>>()
    //Array.zeroCreate<IntDictionary<IntDictionary<ResizableUsualOne<LblNodePair>>>> (inputLength + 1)
    let inline fIntermed x y z = x * (input.Length + 1) * (input.Length + 1) + y * (input.Length + 1) + z
    let inline fNonterm x y z = (x * (input.Length + 1) * (input.Length + 1) + y * (input.Length + 1) + z) * -1
    let inline getKeyForPackedNode x y z w =
        x
        * parser.StateAndTokenToNewState.Count
        * (input.Length + 1)
        * (input.Length + 1)
        + y
        * (input.Length + 1)
        * (input.Length + 1)
        + z * (input.Length + 1)
        + w
    let nonTerminalNodes = new Dictionary<int, int<node>>()
    //let nonTerminalNodes = Array3D.zeroCreate<int<node>> parser.NonTermCount (inputLength + 1) (inputLength + 1)

    //we can use dictionary <extension, dict>
    let intermidiateNodes = new Dictionary<int, int<node>>()
    let terminalNodes = new BlockResizeArray<int<node>>()
    let epsilonNodes = new BlockResizeArray<int<node>>()
    let sppfNodes = new BlockResizeArray<INode>()

    let currentContext = ref <| new ContextFSA(!currentIndex, !currentState, !currentGSSNode, !currentN)

    let startContext = 
        let pos = 0<positionInInput>
        let vertex = new GSSVertexFSA(pos, !currentState)
        new ContextFSA(pos, !currentState, vertex, !currentN)
    
    (*
    let startContexts = 
        input.InitialPositions
        |> Array.rev
        |> Array.Parallel.map(fun e -> 
            let pos = e * 1<positionInInput>
            //let leftPos = e * 1<leftPosition>
            let vertex = new GSSVertexFSA(pos, !currentState)
            new ContextFSA(pos, !currentState, vertex, !currentLength))
            *)
    /// Stack of contexts
    let setR = new System.Collections.Generic.Stack<ContextFSA>([startContext])
    
    let findSppfNode (t : TypeOfNode) lExt rExt : int<node> =
        match t with 
        | Nonterm state ->
            let key = fNonterm lExt rExt (int state)
            let contains, n = nonTerminalNodes.TryGetValue key
            if not contains
            then
                let newNode = new NonTerminalNode(state, (packExtension lExt rExt))
                sppfNodes.Add(newNode)
                let num = (sppfNodes.Length - 1)*1<node>
                nonTerminalNodes.Add(key, num)
                num
            else n
        | Intermed state -> 
            let key = fIntermed lExt rExt (int state)
            let contains, n = intermidiateNodes.TryGetValue key
            if not contains
            then
                let newNode = new IntermidiateNode(state, (packExtension lExt rExt))
                sppfNodes.Add(newNode)
                let num = (sppfNodes.Length - 1)*1<node>
                intermidiateNodes.Add(key, num)
                num  
            else n

    let findSppfPackedNode parent (state : int<state>) leftExtension rightExtension (left : INode) (right : INode)  = 
        //let rule = getRule label
        let createNode () =
            let newNode = new PackedNode(state, left, right)
            sppfNodes.Add(newNode)
            let num = (sppfNodes.Length - 1 )*1<node>
            ///
            if parent = dummyNode then failwith "try to get dummyNode from sppfNodes"
            ///
            match (sppfNodes.Item (int parent)) with
            | :? NonTerminalNode as n ->
                n.AddChild newNode
            | :? IntermidiateNode as i ->
                i.AddChild newNode
            | _ -> ()
            num
        printf "hehe"
        let i = getLeftExtension leftExtension
        let j = getRightExtension leftExtension
        let k = getRightExtension rightExtension
        let key = getKeyForPackedNode i j k (int state)
        //Array.zeroCreate<IntDictionary<IntDictionary<int>>> (inputLength + 1)            
        let contains, d1 = packedNodes.TryGetValue key
        if contains then d1
        else
            let newNode = createNode()
            packedNodes.Add(key, newNode)
            newNode 
    
             
    let getNodeT symbol (pos : int<positionInInput>) =
        let index = int pos
        if symbol = epsilon
        then
            if epsilonNodes.Item index <> Unchecked.defaultof<int<node>>
            then
                epsilonNodes.Item index
            else
                let t = new EpsilonNode(packExtension index index)
                sppfNodes.Add t
                let res = sppfNodes.Length - 1
                epsilonNodes.[index] <- ((sppfNodes.Length - 1)*1<node>)
                res * 1<node>   
        else
            if terminalNodes.Item index <> Unchecked.defaultof<int<node>>
            then
                terminalNodes.Item index
            else
                let t = new TerminalNode(symbol, packExtension index (index + 1))
                sppfNodes.Add t
                let res = sppfNodes.Length - 1
                terminalNodes.[index] <- ((sppfNodes.Length - 1)*1<node>)
                res * 1<node>
    
    let getNodeP (state : int<state>) (t : TypeOfNode) currentN currentR = 
        let currR = sppfNodes.Item (int currentR)
        let extR = currR.getExtension ()
        let lExtR, rExtR = getLeftExtension extR, getRightExtension extR
         
        if currentN <> dummyNode
        then
            let currL = sppfNodes.Item (int currentN)
            let extL = currL.getExtension ()
            let lExtL, _ = getLeftExtension extL, getRightExtension extL
            let y = findSppfNode t lExtL rExtR
            let extra = findSppfPackedNode y state extL extR currL currR
            y
        else
            //let extL = currL.getExtension ()
            //let lExtL, rExtL = getLeftExtension extL, getRightExtension extL
            let y = findSppfNode t lExtR rExtR
            let extra = findSppfPackedNode y state extR extR dummyAST currR
            y

    let getNodes state nontermState (currentN : int<node>) (currentR : int<node>) = 
        ///
        if currentR = dummyNode then failwith "Given dummyNode as currentR in getNodes function."
        ///
        let x = 
            if state |> parser.FinalStates.Contains
            then
                getNodeP state (Nonterm nontermState) currentN currentR
            else
                dummyNode

        let y =
            let isCurrentRNontermAndItsExtentsEqual = 
                match sppfNodes.Item (int currentR) with
                | :? NonTerminalNode as n ->
                    getRightExtension n.Extension = getLeftExtension n.Extension
                | _ -> false

            if (currentN = dummyNode)&&(not isCurrentRNontermAndItsExtentsEqual)
            then
                currentR
            else
                getNodeP state (Intermed state) currentN currentR
        y, x
             
    /// Checks for existing of context in SetU. If not adds it to SetU.
    let containsContext (inputIndex: int<positionInInput>) (state : int<state>) (vertex : GSSVertexFSA) (node : int<node>)=
        let vertexKey = CommonFuns.pack vertex.PositionInInput vertex.NontermState
        if setU.[int inputIndex] <> null
        then
            let cond1, vertexToNodes = setU.[int inputIndex].TryGetValue state
            if cond1
            then
                let cond2, current = vertexToNodes.TryGetValue vertexKey
                if cond2
                then    
                    if not (current |> Array.contains node)
                    then
                        setU.[int inputIndex].[state].[vertexKey] <- Array.append current [|node|]
                        false
                    else
                        true
                else
                    setU.[int inputIndex].[state].Add(vertexKey, [|node|])
                    false
            else
                let dict1 = new Dictionary<_, _>()
                dict1.Add(vertexKey, [|node|])
                setU.[int inputIndex].Add(state, dict1)
                false
        else
            let dict2 = new Dictionary<_, _>()
            dict2.Add(vertexKey, [|node|])
            let dict1 = new Dictionary<_, _>()
            dict1.Add(state, dict2)
            setU.[int inputIndex] <- dict1
            false

    /// Adds new context to stack (setR)
    let pushContext (inputIndex : int<positionInInput>) (state : int<state>) vertex node =
        setR.Push(new ContextFSA(inputIndex, state, vertex, node))

    /// Adds new context to stack (setR) if it is first occurrence of this context (if SetU doesn't contain it).
    let addContext (inputVertex : int<positionInInput>) (state : int<state>) vertex node =
        if not <| containsContext inputVertex state vertex node
        then
            pushContext inputVertex state vertex node

    /// Checks for existing of edge in edges set. If not adds it to edges set.
    let containsEdge (startVertex : GSSVertexFSA) (endVertex : GSSVertexFSA) (state : int<state>) (node : int<node>) =
        let outEdges = edges.[int startVertex.NontermState].[int startVertex.PositionInInput]
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
                        if posLen |> Array.contains (endVertex.PositionInInput,node) 
                        then
                            true, None
                        else
                            let newPosLen = Array.append posLen [|endVertex.PositionInInput,node|]
                            dictStateKey.[endVertex.NontermState] <- newPosLen
                                                        
                            false, None
                    else
                        let arr = [|endVertex.PositionInInput,node|]
                        dictStateKey.Add(endVertex.NontermState, arr)
                        false, None 
                else
                    let d1 = new Dictionary<int<state>, _>()
                    let arr = [|endVertex.PositionInInput,node|]
                    d1.Add(endVertex.NontermState, arr)
                    outEdges.Add(state, d1)
                    false, None
            else 
                let d1 = new Dictionary<int<_>, Dictionary<int<state>, _>>()
                let d2 = new Dictionary<int<state>, _>()
                let arr = [|endVertex.PositionInInput,node|]
                d2.Add(endVertex.NontermState, arr)
                d1.Add(state, d2)
                false, Some d1
        if dict.IsSome then edges.[int startVertex.NontermState].[int startVertex.PositionInInput] <- dict.Value
        cond

    let rec pop (curVertex : GSSVertexFSA) curIndex newNode =
        let outEdges = edges.[int curVertex.NontermState].[int curVertex.PositionInInput]
            
        if outEdges <> null && outEdges.Count <> 0
        then
            let vertexKey = packVertexFSA curVertex.PositionInInput curVertex.NontermState

            let cond, value = setP.TryGetValue vertexKey
            if cond
            then
                value.Add newNode
            else
                let newList = new ResizableUsualOne<_>(newNode)
                setP.Add(vertexKey, newList)

            for stateТoNonterm in outEdges do
                let state = stateТoNonterm.Key  
                for nontermToNode in stateТoNonterm.Value do
                    let nonterm = nontermToNode.Key
                    for position, node in nontermToNode.Value do
                        let adjacentVertex = new GSSVertexFSA(position, nonterm)
                        let y, n = getNodes state nonterm node newNode
                        if y <> dummyNode
                        then
                            addContext curIndex state adjacentVertex y
                        if n <> dummyNode
                        then
                            pop adjacentVertex curIndex n
        let node = sppfNodes.Item (int newNode)
        let nodeExt = node.getExtension()
        let lExt, rExt = getLeftExtension nodeExt, getRightExtension nodeExt
        if (lExt = 0)&&(rExt = input.Length)&&(curVertex.NontermState = parser.StartState)
        then//startNonterminal
            if !result = None
            then
                result := node |> Some
            
(*
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
            |> ignore *)

    ///Creates new descriptors.(Called when found nonterninal in rule(on current input edge, or on some of next)))
    let create (stateToContinue : int<state>) (nonTermState : int<state>) =
        let index = !currentIndex
        let currentVertex = !currentGSSNode
        let newVertex = new GSSVertexFSA(index, nonTermState)
        let currentNode = !currentN

        if edges.[int nonTermState].[int index] <> null
        then//such GSS vertex already exist
            if not <| containsEdge newVertex currentVertex stateToContinue currentNode
            then//no such edge between vertices
                let vertexKey = packVertexFSA index nonTermState
                let cond, poped = setP.TryGetValue vertexKey
                if cond
                then// aready poped for current index and nonterm
                    // add contexts for each position in input
                    poped.DoForAll (fun node ->
                        let y,nonterm = getNodes stateToContinue nonTermState currentNode node
                        if nonterm <> dummyNode
                        then
                            let x = (sppfNodes.Item (int nonterm))
                            let newIndex = (1<positionInInput>) * getRightExtension (x.getExtension())
                            pop currentVertex newIndex nonterm
                        let x = (sppfNodes.Item (int y))
                        let newIndex = (1<positionInInput>) * getRightExtension (x.getExtension())
                        addContext newIndex stateToContinue currentVertex y)
        else//need to create new edge, vertex and context
            containsEdge newVertex currentVertex stateToContinue currentNode |> ignore
            addContext index nonTermState newVertex dummyNode

    /// Moves positions in input and grammar by 1.
    let eatTerm term nextState =
        let newR = getNodeT term !currentIndex
        let y, nontermNode = getNodes nextState (!currentGSSNode).NontermState !currentN newR
        //currentN := y
        
        if nontermNode <> dummyNode
        then
            pop !currentGSSNode (!currentIndex + 1<positionInInput>) nontermNode 
        
        pushContext (!currentIndex + 1<positionInInput>) nextState !currentGSSNode y
    
    let getNewDescriptor() =
        currentContext := setR.Pop()
        currentIndex   := currentContext.Value.Index
        currentGSSNode := currentContext.Value.Vertex
        currentState   := currentContext.Value.State
        currentN       := currentContext.Value.CurrentN
        currentR       := dummyNode

    while setR.Count <> 0 do
        getNewDescriptor()

        if (!currentN = dummyNode)&&(!currentState |> parser.FinalStates.Contains)
        then 
            let eps = getNodeT epsilon !currentIndex
            let _, nontermNode = getNodes !currentState (!currentGSSNode).NontermState dummyNode eps
            pop !currentGSSNode !currentIndex nontermNode

        let outEdges = parser.OutNonterms.[int !currentState]
    
        if !currentIndex <> endOfInput then
            let curToken = input.[int !currentIndex]
            let dictionaryKey = parser.GetTermsDictionaryKey !currentState curToken
                
            let cond, nextState = parser.StateAndTokenToNewState.TryGetValue dictionaryKey
            if cond
            then
                eatTerm curToken nextState

            for curNonterm, nextState in outEdges do
                (*if curNonterm = anyNonterm
                then//found "any" rule
                    eatTerm nextState curEdge curPos
                else
    //                let curToken = input.Edges.[curEdge].Tokens.[curPos] 
    //                if parser.FirstSet.Contains <| parser.GetFirstSetItem (int curNonterm) curToken
    //                then// Found nonterninal in rule, and it have derivation starting with current token.
    //                    // Create new descriptors.
                *)
                create nextState curNonterm
        else(*we reached end of input*)
            ()


//    printfn "Number of descriptors: %i" !numberOfDescr
//    printfn "Number of reused descriptors: %i" !numberOfReusedDescr 
//    printfn "Number of GSS nodes: %i" !numberOfGSSNodes
//    printfn "Number of GSS edges: %i" !numberOfGSSEdges
    //printEdges "GSS.dot" edgesOfGSS
          
    match !result with
        | None -> Error ("String was not parsed")
        | Some node -> 
            let r1 = new Tree<_> (node)
            r1.AstToDot parser.StateToNontermName "ASTforAutomaton.dot"
            Success (r1)
            