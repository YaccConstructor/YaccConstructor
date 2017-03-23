module Yard.Generators.GLL.AbstractParserWithoutTree

open System 
open Microsoft.FSharp.Collections

open Yard.Generators.GLL
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns
open YC.GLL.GSS

let parse (parser : ParserSourceGLL) (input : IParserInput) = 
    let gss = new GSS()

    let startContexts = 
        input.InitialPositions
        |> Array.rev
        |> Array.map(fun pos -> 
            let vertex = new GSSVertex(parser.StartState, pos)
            gss.AddVertex vertex |> ignore
            new ContextFSA<_>(pos, parser.StartState, vertex, 0us))

    /// Stack of contexts
    let setR = new System.Collections.Generic.Stack<ContextFSA<_>>(startContexts)
                 
    /// Adds new context to stack (setR)
    let pushContext posInInput posInGrammar gssVertex len =
        setR.Push(new ContextFSA<_>(posInInput, posInGrammar, gssVertex, len))

    /// Adds new context to stack (setR) if it is first occurrence of this context (if SetU doesn't contain it).
    let addContext posInInput posInGrammar (gssVertex:GSSVertex) len =
        if not <| gssVertex.ContainsContext posInInput posInGrammar len
        then pushContext posInInput posInGrammar gssVertex len
    
    ///Creates new descriptors.(Calls when found nonterninal in rule(on current input edge, or on some of next)))
    let create (curContext:ContextFSA<_>) stateToContinue nonterm =        
        let newVertex = new GSSVertex(nonterm, curContext.PosInInput)
        let vertexExists, edgeExists, startV = gss.ContainsVertAndEdge(newVertex, curContext.GssVertex, stateToContinue, curContext.Length)        

        if vertexExists
        then
            if startV.P.Count > 0
            then startV.P |> ResizeArray.iter(fun p -> addContext p.posInInput stateToContinue curContext.GssVertex (curContext.Length + p.data))        
        else
            addContext curContext.PosInInput nonterm startV 0us
            
    /// 
    let pop (curContext:ContextFSA<_>) =
        let curGssVertex = curContext.GssVertex
        let outEdges = gss.OutEdges curGssVertex |> Array.ofSeq
        
        curGssVertex.AddP (new PoppedData(curContext.PosInInput, curContext.Length))
        if outEdges <> null && outEdges.Length <> 0
        then
            for e in outEdges do
                addContext curContext.PosInInput e.Tag.StateToContinue e.Target (curContext.Length + e.Tag.Data)

    let processed = ref 0
    let mlnCount = ref 0
    let startTime = ref System.DateTime.Now

    while setR.Count <> 0 do
        let currentContext = setR.Pop()

        incr processed
        if !processed = 10000000
        then
            incr mlnCount            
            printfn "%A mlns of D procesed. %A D/sec" (!mlnCount * 10) (!processed / int (System.DateTime.Now - !startTime).TotalMilliseconds * 1000)
            processed := 0
            startTime :=  System.DateTime.Now

        let possibleNontermMovesInGrammar = parser.OutNonterms.[int currentContext.PosInGrammar]

        /// Current state is final
        if currentContext.PosInGrammar |> parser.FinalStates.Contains
        then pop currentContext
        
        /// Nonterminal transitions. Move pointer in grammar. Position in input is not changed.
        for curNonterm, nextState in possibleNontermMovesInGrammar do            
            create currentContext nextState curNonterm

        /// Terminal transitions.
        input.ForAllOutgoingEdges
            currentContext.PosInInput
            (fun nextToken nextPosInInput -> 
                let isTransitionPossible, nextPosInGrammar = parser.StateAndTokenToNewState.TryGetValue (parser.GetTermsDictionaryKey currentContext.PosInGrammar (int nextToken))
                if isTransitionPossible
                then pushContext nextPosInInput nextPosInGrammar currentContext.GssVertex (currentContext.Length + 1us)
            )

    gss
       
let findVertices (gss:GSS) state =    
    gss.Vertices
    |> Seq.filter (fun v -> v.Nonterm = state)
             
let isParsed (parser : ParserSourceGLL) (input : LinearInput) = 
    let gss = parse parser input
    findVertices gss parser.StartState
    |> Seq.exists (fun v -> v.P |> ResizeArray.exists (fun p -> int p.posInInput = input.Input.Length))

let getAllRangesForState gss state =
    findVertices gss state
    |> Seq.collect (fun v -> v.U |> Seq.collect (fun kvp -> kvp.Value |> Seq.map (fun x -> v.PositionInInput, v.GetUncompressetPositions kvp.Key |> fst)))    

let getAllRangesForStartState (parser : ParserSourceGLL) (input : IParserInput) = 
    let gss = parse parser input
    getAllRangesForState gss parser.StartState

let getAllRangesForStateWithLength gss state =
    findVertices gss state
    |> Seq.collect (fun v -> v.U |> Seq.collect (fun kvp -> kvp.Value |> Seq.map (fun x -> v.PositionInInput, v.GetUncompressetPositions kvp.Key |> fst, x)))

let getAllRangesForStartStateWithLength (parser : ParserSourceGLL) (input : IParserInput) = 
    let gss = parse parser input
    getAllRangesForStateWithLength gss parser.StartState