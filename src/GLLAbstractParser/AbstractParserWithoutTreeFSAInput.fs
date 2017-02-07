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

let findVertices (gss:GSS) state =    
    gss.Vertices
    |> Seq.filter (fun v -> v.PositionInGrammar = state)

let parse (parser : FSAParserSourceGLL) (input : IParserInput) = 

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
        if not <| gssVertex.ContainsContext posInInput posInGrammar
        then pushContext posInInput posInGrammar gssVertex len
    
    ///Creates new descriptors.(Calls when found nonterninal in rule(on current input edge, or on some of next)))
    let create (curContext:ContextFSA<_>) stateToContinue posInGrammar =        
        let newVertex = new GSSVertex(curContext.PosInGrammar, curContext.PosInInput)
        let exists, startV = gss.ContainsEdge(newVertex, curContext.GssVertex, stateToContinue, curContext.Length)        

        if startV.P.Count > 0
        then startV.P |> ResizeArray.iter(fun (newIndex, l) -> addContext newIndex stateToContinue curContext.GssVertex (curContext.Length + l))        
        else addContext curContext.PosInInput posInGrammar startV 0us
            
    /// 
    let pop (curContext:ContextFSA<_>) =
        let curGssVertex = curContext.GssVertex
        let outEdges = gss.OutEdges curGssVertex |> Array.ofSeq
            
        if outEdges <> null && outEdges.Length <> 0
        then
            let vertexKey = packVertexFSA curGssVertex.PositionInInput curGssVertex.PositionInGrammar
            let value = curGssVertex.P.Add (curContext.PosInInput, curContext.Length)
            for e in outEdges do
                addContext curContext.PosInInput e.Tag.StateToContinue e.Target (curContext.Length + e.Tag.LengthOfProcessedString)

    while setR.Count <> 0 do
        let currentContext = setR.Pop()
        let possibleNontermMoviesInGrammar = parser.OutNonterms.[int currentContext.PosInGrammar]

        /// Current state is final
        if currentContext.PosInGrammar |> parser.FinalStates.Contains
        then pop currentContext
        
        /// Nonterminal transitions. Move pointer in grammar. Position in input is not changed.
        for curNonterm, nextState in possibleNontermMoviesInGrammar do            
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
         
let buildAbstract (parser : FSAParserSourceGLL) (input : IParserInput) = 
    let gss = parse parser input
    findVertices gss parser.StartState