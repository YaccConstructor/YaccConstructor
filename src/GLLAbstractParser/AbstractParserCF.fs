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

// now we have two CF grammars and two GSSs
let parse (leftGrammar : ParserSourceGLL) (rightGrammar : ParserSourceGLL) = 
    
    let gssLeft, gssRight = new GSS(), new GSS()

    // choose the grammar constructions we want to work with
    let inline getGrammarInfo selector = 
        match selector with | Left -> (leftGrammar, gssLeft) | Right -> (rightGrammar, gssRight)

    let startContext =
        let s1, s2 = leftGrammar.StartState, rightGrammar.StartState
        let vertexParser, vertexInput = new GSSVertex(s1, toInputPos s2), new GSSVertex(s2, toInputPos s1)
        gssLeft.AddVertex vertexParser |> ignore
        gssRight.AddVertex vertexInput |> ignore
        new ContextCF<_>(s1, s2, vertexParser, vertexInput)

    let setR = Stack<_>([startContext])
    
    // with Empty data
    let pushContext posInGrammar1 posInGrammar2 gssVertex1 gssVertex2 =
        setR.Push(new ContextCF<_>(posInGrammar1, posInGrammar2, gssVertex1, gssVertex2))
   
    /// Adds descriptor of the form (p1, p2, v1, v2) to the stack (setR)
    /// if it's first occurrence of this descriptor (both U-sets doesn't contain corresponding values)
    let addContext posInGrammar1 posInGrammar2 (gssVertex1: GSSVertex) (gssVertex2: GSSVertex) =
        if not <| 
            gssVertex1.ContainsContext (toInputPos posInGrammar2) posInGrammar1 Empty
            && gssVertex2.ContainsContext (toInputPos posInGrammar1) posInGrammar2 Empty
        then pushContext posInGrammar1 posInGrammar2 gssVertex1 gssVertex2
    
    /// Makes pop-action with specified grammar and gss (position in the second grammar doesn't change) 
    let rec pop masterGrammar (curContext: ContextCF<_>) gssVertexMaster posInSlaveGrammar =
        let grammar, gss = getGrammarInfo masterGrammar              
        let _, gssVertexSlave = masterGrammar.neg |> curContext.GetInfo
        
        let outEdges = gss.OutEdges gssVertexMaster |> Array.ofSeq        
        gssVertexMaster.P.Add (new PoppedData(toInputPos posInSlaveGrammar, Empty))
        if outEdges <> null && outEdges.Length <> 0
        then  
            for e in outEdges do
                if e.Tag.StateToContinue |> grammar.FinalStates.Contains
                then
                    pop masterGrammar curContext e.Target posInSlaveGrammar 
                addContext e.Tag.StateToContinue posInSlaveGrammar e.Target gssVertexSlave
    
    let create masterGrammar (curContext: ContextCF<_>) stateToContinue nonterm =  
        let grammar, gss = getGrammarInfo masterGrammar
        let _, gssVertexMaster = masterGrammar |> curContext.GetInfo
        let posInSlaveGrammar, gssVertexSlave = masterGrammar.neg |> curContext.GetInfo      
        
        let newVertex = new GSSVertex(nonterm, toInputPos posInSlaveGrammar)
        let vertexExists, edgeExists, startV = gss.ContainsVertAndEdge(newVertex, gssVertexMaster, stateToContinue, Empty)        

        if vertexExists
        then
            if not edgeExists
            then
                startV.P
                |> ResizeArray.iter(fun p -> 
                    if stateToContinue |> grammar.FinalStates.Contains
                    then
                        pop masterGrammar curContext gssVertexMaster (toGrammarPos p.posInInput)
                    addContext stateToContinue (toGrammarPos p.posInInput) gssVertexMaster gssVertexSlave)      
        else addContext nonterm posInSlaveGrammar startV gssVertexSlave

    let handleFinalStates masterGrammar (currentContext: ContextCF<_>) =
        let grammar, _ = getGrammarInfo masterGrammar
        let posInMasterGrammar, gssVertexMaster = masterGrammar |> currentContext.GetInfo
        let posInSlaveGrammar, _ = masterGrammar.neg |> currentContext.GetInfo
        
        if posInMasterGrammar |> grammar.FinalStates.Contains
        then pop masterGrammar currentContext gssVertexMaster posInSlaveGrammar

    let handleNontermTransitions masterGrammar (currentContext: ContextCF<_>) =
        let grammar, _ = getGrammarInfo masterGrammar
        let posInMasterGrammar, gssVertexmaster = masterGrammar |> currentContext.GetInfo
        let possibleNontermMovesInGrammar = grammar.OutNonterms.[int posInMasterGrammar]

        for curNonterm, nextState in possibleNontermMovesInGrammar do            
            create masterGrammar currentContext nextState curNonterm

    let handleTerminalTransitions (currentContext: ContextCF<_>) =
        // it is impossible to get state's out terms from ParserSourceGLL-info
        // need to add "outTerms" to ParserSource or use FSA directly 
        ()

    while setR.Count <> 0 do
        let currentContext = setR.Pop()
        ()


    (gssLeft, gssRight)