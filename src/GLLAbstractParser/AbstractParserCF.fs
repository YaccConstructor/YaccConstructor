module Yard.Generators.GLL.AbstractParserCF

open System 
open Microsoft.FSharp.Collections
open System.Collections.Generic
open FSharpx.Collections.Experimental

open Yard.Generators.GLL
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns

open YC.GLL.GSS

let inline toInputPos x = int x |> LanguagePrimitives.Int32WithMeasure<positionInInput>
let inline toGrammarPos x = int x |> LanguagePrimitives.Int32WithMeasure<positionInGrammar> 

let summLengths (len1 : ParseData) (len2 : ParseData) = 
    match len1, len2 with 
    | Length len1, Length len2  -> Length(len1 + len2)
    | _ -> failwith "Wrong type"

// now we have two CF grammars and two GSSs
let parse (leftGrammar : ParserSourceGLL) (rightGrammar : ParserSourceGLL) (parseSubstrings: bool) = 
    
    let gssVertexInsHolderLeft, gssVertexInsHolderRight = new GSSVertexCFInstanceHolder(), new GSSVertexCFInstanceHolder()
    let gssLeft, gssRight = new GSS(), new GSS()

    // choose the grammar constructions we want to work with
    let inline getGrammarInfo selector = 
        match selector with 
        | Left -> (leftGrammar, gssLeft, gssVertexInsHolderLeft) 
        | Right -> (rightGrammar, gssRight, gssVertexInsHolderRight)
    
    let createStartVertices leftPos rightPos =
//        let left = gssVertexInsHolderLeft.Get(leftPos, (toInputPos rightPos))
//        let right = gssVertexInsHolderRight.Get(rightPos, (toInputPos leftPos))
        let left = gssVertexInsHolderLeft.Get(leftPos, (toInputPos rightPos), null)
        let right = gssVertexInsHolderRight.Get(rightPos, (toInputPos leftPos), null)
//        let left = new GSSVertexCF(leftPos, (toInputPos rightPos), null)
//        let right = new GSSVertexCF(rightPos, (toInputPos leftPos), null)
        gssLeft.AddVertex left |> ignore
        gssRight.AddVertex right |> ignore
        left, right


    let (startContexts: ContextCF<GSSVertex> []) =
        if parseSubstrings
        then 
            [|for i in rightGrammar.OutTerms.Length - 1 .. -1 .. 0 -> i * 1<positionInGrammar>|]
            //|> Array.rev
            |> Array.map(fun pos -> 
                             let leftVertex, rightVertex = createStartVertices leftGrammar.StartState pos
                             new ContextCF<_>(leftGrammar.StartState, pos, leftVertex, rightVertex))
        else
            let s1, s2 = leftGrammar.StartState, rightGrammar.StartState
            let leftVertex, rightVertex = createStartVertices s1 s2
            [| new ContextCF<_>(s1, s2, leftVertex, rightVertex) |]

    let setR = Stack<_>(startContexts)
    
    // with Empty data
    let pushContext posInGrammar1 posInGrammar2 gssVertex1 gssVertex2 =
        setR.Push(new ContextCF<_>(posInGrammar1, posInGrammar2, gssVertex1, gssVertex2))
   
    /// Adds descriptor of the form (p1, p2, v1, v2) to the stack (setR)
    /// if it's first occurrence of this descriptor (both U-sets doesn't contain corresponding values)
    let rec addContext masterGrammar posInGrammar1 posInGrammar2 (gssVertex1: GSSVertex) (gssVertex2: GSSVertex) =
        match masterGrammar with 
        | Left ->
            let contains1 = gssVertex1.ContainsContext (toInputPos posInGrammar2) posInGrammar1 Empty
            let contains2 = gssVertex2.ContainsContext (toInputPos posInGrammar1) posInGrammar2 Empty
            if not (contains1 && contains2)
            then pushContext posInGrammar1 posInGrammar2 gssVertex1 gssVertex2
        | Right -> addContext Left posInGrammar2 posInGrammar1 gssVertex2 gssVertex1
            
    
    /// Makes pop-action with specified grammar and gss (position in the second grammar doesn't change) 
    let rec pop masterGrammar (curContext: ContextCF<_>) gssVertexMaster posInSlaveGrammar =
        let grammar, gss, vertexHolder = getGrammarInfo masterGrammar              
        let _, gssVertexSlave = masterGrammar.neg |> curContext.GetInfo
        let outEdges = gss.OutEdges gssVertexMaster |> Array.ofSeq 
        
        if new PoppedData(toInputPos posInSlaveGrammar, Empty) |> gssVertexMaster.P.Add |> not then () else       
//        gssVertexMaster.P.SetP.Add (new PoppedData(toInputPos posInSlaveGrammar, Empty))
            if outEdges <> null && outEdges.Length <> 0
            then  
                for e in outEdges do
                    if e.Tag.StateToContinue |> grammar.FinalStates.Contains
                    then
                        pop masterGrammar curContext e.Target posInSlaveGrammar 
                    addContext masterGrammar e.Tag.StateToContinue posInSlaveGrammar e.Target gssVertexSlave
    
    let create masterGrammar (curContext: ContextCF<_>) stateToContinue nonterm =  
        let grammar, gss, vertexHolder = getGrammarInfo masterGrammar
        let _, gssVertexMaster = masterGrammar |> curContext.GetInfo
        let posInSlaveGrammar, gssVertexSlave = masterGrammar.neg |> curContext.GetInfo      
        
//        let newVertex = new GSSVertexCF(nonterm, toInputPos posInSlaveGrammar, gssVertexSlave)

//        let startV = vertexHolder.Get(nonterm, toInputPos posInSlaveGrammar)
        let startV = vertexHolder.Get(nonterm, toInputPos posInSlaveGrammar, gssVertexSlave)
        let vertexExists, edgeExists = gss.ContainsVertexAndEdge(startV, gssVertexMaster, stateToContinue, curContext.Data)     

        if vertexExists
        then
            if not edgeExists
            then
                startV.P.SetP
                |> ResizeArray.iter(fun p -> 
                    if stateToContinue |> grammar.FinalStates.Contains
                    then
                        pop masterGrammar curContext gssVertexMaster (toGrammarPos p.posInInput)
                    addContext masterGrammar stateToContinue (toGrammarPos p.posInInput) gssVertexMaster gssVertexSlave)      
        else addContext masterGrammar nonterm posInSlaveGrammar startV gssVertexSlave

    let handleFinalStates masterGrammar (currentContext: ContextCF<_>) =
        let grammar, _, _ = getGrammarInfo masterGrammar
        let posInMasterGrammar, gssVertexMaster = masterGrammar |> currentContext.GetInfo
        let posInSlaveGrammar, _ = masterGrammar.neg |> currentContext.GetInfo
        
        if posInMasterGrammar |> grammar.FinalStates.Contains
        then pop masterGrammar currentContext gssVertexMaster posInSlaveGrammar

    let makeNontermTransitions masterGrammar (currentContext: ContextCF<_>) =
        let grammar, _, _ = getGrammarInfo masterGrammar
        let posInMasterGrammar, gssVertexmaster = masterGrammar |> currentContext.GetInfo
        let possibleNontermMovesInGrammar = grammar.OutNonterms.[int posInMasterGrammar]

        for curNonterm, nextState in possibleNontermMovesInGrammar do            
            create masterGrammar currentContext nextState curNonterm

    let makeTermTransitions (currentContext: ContextCF<_>) =
       let outTermsLeft, outTermsRight = 
           leftGrammar.OutTerms.[int currentContext.PosInGrammar1], rightGrammar.OutTerms.[int currentContext.PosInGrammar2]
       outTermsLeft |> Array.iter (fun (t1, s1) -> 
                                       outTermsRight 
                                       |> Array.filter (fun (t2, s2) -> t2 = t1)
                                       |> Array.iter (fun (t2, s2) -> 
                                                          addContext Left s1 s2 currentContext.GssVertex1 currentContext.GssVertex2))
    
    let count = ref 0

    while setR.Count <> 0 do
        incr count
        let currentContext = setR.Pop()
        handleFinalStates Left currentContext
        handleFinalStates Right currentContext
        makeNontermTransitions Left currentContext
        makeNontermTransitions Right currentContext
        makeTermTransitions currentContext

        if !count = 3000000
        then setR.Clear()
    
    gssLeft, gssRight, !count

let findVertices (gss: GSS) state : seq<GSSVertex> =    
    gss.Vertices
    |> Seq.filter (fun v -> v.Nonterm = state)

let getAllCompleteRangesForState gss state finalState =
    findVertices gss state  
    |> Seq.collect (fun v -> v.U |> Seq.choose (fun kvp -> let (pi, pg) = v.GetUncompressetPositions kvp.Key 
                                                           in if pg = finalState then Some (v.PositionInInput, pi) else None))