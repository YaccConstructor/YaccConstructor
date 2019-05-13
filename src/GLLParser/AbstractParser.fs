module YC.Parsing.GLL.AbstractParser

open Microsoft.FSharp.Collections
open YC.Parsing.Common.GraphInput
open YC.Parsing.Common.GLL
open YC.Parsing.Common.ASTGLLFSA
open YC.Parsing.GLL.GSS
open YC.Parsing.GLL.SPPF

let summLengths (len1 : ParseData) (len2 : ParseData) = 
    match len1, len2 with 
    | Length len1, Length len2  -> Length(len1 + len2)
    | _ -> failwith "Wrong type"

let unpackNode = function
    | TreeNode x -> x
    | _ -> failwith "Wrong type"

let parse (parser : ParserSourceGLL) (input : IParserInput<_>) (buildTree : bool) = 
    let dummy = 
        if buildTree
        then TreeNode(-1<nodeMeasure>)
        else Length(0us)
    let epsilon = -1<token>

    let gss = new GSS()
    let gssVertexInstanceHolder = new GSSVertexInstanceHolder()
    let sppf = new SPPF(parser.StartState, parser.FinalStates)
    
    let startContexts = 
        input.InitialPositions
        //|> Array.rev
        |> Array.map(fun pos -> 
            let vertex = gssVertexInstanceHolder.Get(parser.StartState, pos)
            gss.AddVertex vertex |> ignore
            new ContextFSA<_,_>(pos, parser.StartState, vertex, dummy, 0))

    /// Stack of contexts
    let setR = new C5.IntervalHeap<_>() :> C5.IPriorityQueue<_>
    setR.AddAll(startContexts)
    
/// Adds new context to stack (setR)
    let pushContext posInInput posInGrammar gssVertex data priority =
        setR.Add(new ContextFSA<_,_>(posInInput, posInGrammar, gssVertex, data, priority)) |> ignore

    /// Adds new context to stack (setR) if it is first occurrence of this context (if SetU doesn't contain it).
    let addContext posInInput posInGrammar (gssVertex:GSSVertex) data priority =
        if (not <| gssVertex.ContainsContext posInInput posInGrammar data)
        //&& (posInInput > 0<positionInInput> && not <| gssVertex.ContainsContext (posInInput + 1<positionInInput>) posInGrammar data)
        then pushContext posInInput posInGrammar gssVertex data priority
    
    /// 
    let rec pop (posInInput:int<positionInInput>) (gssVertex : GSSVertex) (newData : ParseData) newPriority =
        let outEdges = gss.OutEdges gssVertex |> Array.ofSeq
        
        if new PoppedData(posInInput, newData) |> gssVertex.P.Add |> not then () else
        if outEdges <> null && outEdges.Length <> 0
        then
            for e in outEdges do
                if buildTree
                then
                    let y, n = sppf.GetNodes e.Tag.StateToContinue e.Target.Nonterm e.Tag.Data newData
                    if y <> dummy
                    then
                        addContext posInInput e.Tag.StateToContinue e.Target y newPriority
                    if n <> dummy
                    then
                        pop posInInput e.Target n newPriority
                else
                    if e.Tag.StateToContinue |> parser.FinalStates.Contains
                    then
                        pop posInInput e.Target (summLengths newData e.Tag.Data) newPriority
                    addContext posInInput e.Tag.StateToContinue e.Target (summLengths newData e.Tag.Data) newPriority

    ///Creates new descriptors.(Calls when found nonterninal in rule(on current input edge, or on some of next)))
    let create (curContext:ContextFSA<_,_>) stateToContinue nonterm newPriority =        
        let startV = gssVertexInstanceHolder.Get(nonterm, curContext.PosInInput)
        let vertexExists, edgeExists = gss.ContainsVertexAndEdge(startV, curContext.GssVertex, stateToContinue, curContext.Data)        

        if vertexExists
        then
            if not edgeExists
            then
//                if startV.P.Count > 0
//                then 
                startV.P.SetP
                |> ResizeArray.iter(fun p -> 
                    if buildTree
                    then 
                        let y,nontermNode = sppf.GetNodes stateToContinue curContext.GssVertex.Nonterm curContext.Data p.data
                        if nontermNode <> dummy
                        then
                            let x = (sppf.Nodes.Item (int <| unpackNode nontermNode))
                            let newIndex = getRightExtension (x.getExtension())
                            pop newIndex curContext.GssVertex nontermNode newPriority
                        let x = (sppf.Nodes.Item (int <| unpackNode y))
                        let newIndex = getRightExtension (x.getExtension())
                        addContext newIndex stateToContinue curContext.GssVertex y newPriority
                    else
                        if stateToContinue |> parser.FinalStates.Contains
                        then
                            pop p.posInInput curContext.GssVertex (summLengths curContext.Data p.data) newPriority
                        addContext p.posInInput stateToContinue curContext.GssVertex (summLengths curContext.Data p.data) newPriority)        
        else addContext curContext.PosInInput nonterm startV dummy newPriority

    let eatTerm (currentContext : ContextFSA<GSSVertex,_>) nextToken nextPosInInput nextPosInGrammar newPriority =
        if buildTree
        then
            let y, nontermNode =
                match nextToken with 
                | Some (nextToken,weight) ->
                    let newR = sppf.GetNodeT nextToken currentContext.PosInInput nextPosInInput weight
                    sppf.GetNodes nextPosInGrammar currentContext.GssVertex.Nonterm currentContext.Data newR               
                | None ->
                    //printfn "%A --- %A" currentContext.PosInInput nextPosInInput
                    let newR = sppf.GetNodeT (-10<token>) currentContext.PosInInput nextPosInInput 1<weight>
                    sppf.GetNodes nextPosInGrammar currentContext.GssVertex.Nonterm currentContext.Data newR
                    //currentContext.Data, dummy

             

            if nontermNode <> dummy
            then
                pop nextPosInInput currentContext.GssVertex nontermNode newPriority
        
            if parser.MultipleInEdges.[int nextPosInGrammar]
            then 
                addContext nextPosInInput nextPosInGrammar currentContext.GssVertex y newPriority
            else
                pushContext nextPosInInput nextPosInGrammar currentContext.GssVertex y newPriority
        else
            if nextPosInGrammar |> parser.FinalStates.Contains
            then
                pop nextPosInInput currentContext.GssVertex (summLengths currentContext.Data (Length(1us))) newPriority
            
            if parser.MultipleInEdges.[int nextPosInGrammar]
            then 
                addContext nextPosInInput nextPosInGrammar currentContext.GssVertex (summLengths currentContext.Data (Length(1us))) newPriority
            else
                pushContext nextPosInInput nextPosInGrammar currentContext.GssVertex (summLengths currentContext.Data (Length(1us))) newPriority
    
    
    let processed = ref 0
    let mlnCount = ref 0
    let startTime = ref System.DateTime.Now
    let inErrorRecoveryMode = ref false

    let inline isParsed () = 
        match input with
        | :? LinearIputWithErrors as input ->
            gss.Vertices
            |> Seq.filter (fun v -> v.Nonterm = parser.StartState)
            |> Seq.exists (fun v -> v.P.SetP |> ResizeArray.exists (fun p -> int p.posInInput = input.Input.Length))
        | _ -> false

    while not (setR.Count = 0  || ((*!inErrorRecoveryMode &&*) isParsed ())) do
        let currentContext = setR.DeleteMin()
        inErrorRecoveryMode := currentContext.Priority <> 0

        incr processed
        if !processed = 10000000
        then
            incr mlnCount            
            printfn "%A mlns of D procesed. %A D/sec" (!mlnCount * 10) (!processed / int (System.DateTime.Now - !startTime).TotalMilliseconds * 1000)
            processed := 0
            startTime :=  System.DateTime.Now

        let possibleNontermMovesInGrammar = parser.OutNonterms.[int currentContext.PosInGrammar]

        /// Current state is final
        if (currentContext.Data = dummy)&&(currentContext.PosInGrammar |> parser.FinalStates.Contains)
        then 
            if buildTree
            then
                let eps = sppf.GetNodeT epsilon currentContext.PosInInput currentContext.PosInInput 0<weight>
                let _, nontermNode = sppf.GetNodes currentContext.PosInGrammar currentContext.GssVertex.Nonterm dummy eps
                pop currentContext.PosInInput currentContext.GssVertex nontermNode currentContext.Priority
            else
                pop currentContext.PosInInput currentContext.GssVertex dummy currentContext.Priority
        
        /// Nonterminal transitions. Move pointer in grammar. Position in input is not changed.
        for curNonterm, nextState in possibleNontermMovesInGrammar do            
            create currentContext nextState curNonterm currentContext.Priority

        /// Terminal transitions.
        input.ForAllOutgoingEdges
            currentContext.PosInInput
            currentContext.Priority
            (fun nextToken nextPosInInput newPriority weight -> 
                if nextToken <> parser.EpsilonInputTag
                then
                    let isTransitionPossible, nextPosInGrammar = parser.StateAndTokenToNewState.TryGetValue (parser.GetTermsDictionaryKey currentContext.PosInGrammar (int nextToken))
                    if isTransitionPossible
                    then eatTerm currentContext (Some (nextToken, weight)) nextPosInInput nextPosInGrammar newPriority
                else eatTerm currentContext None nextPosInInput currentContext.PosInGrammar newPriority
            )

    gss, sppf,
        if buildTree
        then
            let roots = sppf.GetRoots gss
            sppf.SetWeights roots
            sppf.ChooseMinimalForest roots
            Some <| new Tree<_>(roots, input.PositionToString, parser.IntToString)
        else
            None
       
let findVertices (gss:GSS) state : seq<GSSVertex> =    
    gss.Vertices
    |> Seq.filter (fun v -> v.Nonterm = state)

let buildAst (parser : ParserSourceGLL) (input : IParserInput<_>) = 
    let _, _, tree = parse parser input true
    let tree = if tree.IsNone
                then failwith "NotParsed"
                else tree.Value
    tree
        
let getAllSPPFRoots (parser : ParserSourceGLL) (input : IParserInput<_>) = 
    let gss, sppf, _ = parse parser input true
    sppf.GetRootsForStartAndFinal gss input.InitialPositions input.FinalPositions

let isParsed (parser : ParserSourceGLL) (input : LinearInput) = 
    let gss, _, _ = parse parser input false
    findVertices gss parser.StartState
    |> Seq.exists (fun v -> v.P.SetP |> ResizeArray.exists (fun p -> int p.posInInput = input.Input.Length))

let getAllRangesForState gss state =
    findVertices gss state
    |> Seq.collect (fun v -> v.P.SetP |> Seq.map (fun poped -> v.PositionInInput, poped.posInInput))    

let getAllRangesForStartState (parser : ParserSourceGLL) (input : IParserInput<_>) = 
    let gss, _, _ = parse parser input false
    getAllRangesForState gss parser.StartState

let getAllRangesForStateWithLength gss state =
    findVertices gss state
    |> Seq.collect (fun v -> v.P.SetP |> Seq.map (fun poped -> v.PositionInInput, poped.posInInput, match poped.data with Length x -> x | TreeNode _ -> failwith "Impossible!"))

let getGSS (parser : ParserSourceGLL) (input : IParserInput<_>) = 
    let gss, _, _ = parse parser input false
    gss

let getAllRangesForStartStateWithLength (parser : ParserSourceGLL) (input : IParserInput<_>) = 
    let gss, _, _ = parse parser input false
    getAllRangesForStateWithLength gss parser.StartState