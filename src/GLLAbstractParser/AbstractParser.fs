module Yard.Generators.GLL.AbstractParser
open System 
open Microsoft.FSharp.Collections
open System.Collections.Generic
open FSharpx.Collections.Experimental

open Yard.Generators.GLL
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns
open Yard.Generators.Common.ASTGLLFSA
open YC.GLL.GSS
open YC.GLL.SPPF

let summLengths (len1 : ParseData) (len2 : ParseData) = 
    match len1, len2 with 
    | Length len1, Length len2  -> Length(len1 + len2)
    | _ -> failwith "Wrong type"

let unpackNode = function
    | TreeNode x -> x
    | _ -> failwith "Wrong type"

let parse (parser : ParserSourceGLL) (input : IParserInput) (buildTree : bool) = 
    let dummy = 
        if buildTree
        then TreeNode(-1<nodeMeasure>)
        else Length(0us)
    let epsilon = -1<token>

    let gss = new GSS()
    let sppf = new SPPF(parser.StartState, parser.FinalStates)
    
    let startContexts = 
        input.InitialPositions
        //|> Array.rev
        |> Array.map(fun pos -> 
            let _,vertex = GSSVertex.Get(parser.StartState, pos)
            gss.AddVertex vertex |> ignore
            new ContextFSA<_>(pos, parser.StartState, vertex, dummy))

    /// Stack of contexts
    let setR = new System.Collections.Generic.Stack<ContextFSA<_>>(startContexts)
    
    /// Adds new context to stack (setR)
    let pushContext posInInput posInGrammar gssVertex data =
        setR.Push(new ContextFSA<_>(posInInput, posInGrammar, gssVertex, data))

    /// Adds new context to stack (setR) if it is first occurrence of this context (if SetU doesn't contain it).
    let addContext posInInput posInGrammar (gssVertex:GSSVertex) data =
        if not <| gssVertex.ContainsContext posInInput posInGrammar data
        then pushContext posInInput posInGrammar gssVertex data
    
    /// 
    let rec pop (posInInput:int<positionInInput>) (gssVertex : GSSVertex) (newData : ParseData)=
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
                        addContext posInInput e.Tag.StateToContinue e.Target y
                    if n <> dummy
                    then
                        pop posInInput e.Target n
                else
                    if e.Tag.StateToContinue |> parser.FinalStates.Contains
                    then
                        pop posInInput e.Target (summLengths newData e.Tag.Data)
                    addContext posInInput e.Tag.StateToContinue e.Target (summLengths newData e.Tag.Data)

    ///Creates new descriptors.(Calls when found nonterninal in rule(on current input edge, or on some of next)))
    let create (curContext:ContextFSA<_>) stateToContinue nonterm =        
        let vertexExists, startV = GSSVertex.Get(nonterm, curContext.PosInInput)
        let edgeExists = gss.ContainsEdge(vertexExists, startV, curContext.GssVertex, stateToContinue, curContext.Data)        

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
                            let newIndex = (1<positionInInput>) * getRightExtension (x.getExtension())
                            pop newIndex curContext.GssVertex nontermNode
                        let x = (sppf.Nodes.Item (int <| unpackNode y))
                        let newIndex = (1<positionInInput>) * getRightExtension (x.getExtension())
                        addContext newIndex stateToContinue curContext.GssVertex y
                    else
                        if stateToContinue |> parser.FinalStates.Contains
                        then
                            pop p.posInInput curContext.GssVertex (summLengths curContext.Data p.data)
                        addContext p.posInInput stateToContinue curContext.GssVertex (summLengths curContext.Data p.data))        
        else addContext curContext.PosInInput nonterm startV dummy

    let eatTerm (currentContext : ContextFSA<GSSVertex>) nextToken nextPosInInput nextPosInGrammar =
        if buildTree
        then
            let newR = sppf.GetNodeT nextToken currentContext.PosInInput nextPosInInput
            let y, nontermNode = sppf.GetNodes nextPosInGrammar currentContext.GssVertex.Nonterm currentContext.Data newR

            if nontermNode <> dummy
            then
                pop nextPosInInput currentContext.GssVertex nontermNode 
        
            if parser.MultipleInEdges.[int nextPosInGrammar]
            then 
                addContext nextPosInInput nextPosInGrammar currentContext.GssVertex y
            else
                pushContext nextPosInInput nextPosInGrammar currentContext.GssVertex y
        else
            if nextPosInGrammar |> parser.FinalStates.Contains
            then
                pop nextPosInInput currentContext.GssVertex (summLengths currentContext.Data (Length(1us)))
            
            if parser.MultipleInEdges.[int nextPosInGrammar]
            then 
                addContext nextPosInInput nextPosInGrammar currentContext.GssVertex (summLengths currentContext.Data (Length(1us)))
            else
                pushContext nextPosInInput nextPosInGrammar currentContext.GssVertex (summLengths currentContext.Data (Length(1us)))
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
        if (currentContext.Data = dummy)&&(currentContext.PosInGrammar |> parser.FinalStates.Contains)
        then 
            if buildTree
            then
                let eps = sppf.GetNodeT epsilon currentContext.PosInInput currentContext.PosInInput
                let _, nontermNode = sppf.GetNodes currentContext.PosInGrammar currentContext.GssVertex.Nonterm dummy eps
                pop currentContext.PosInInput currentContext.GssVertex nontermNode
            else
                pop currentContext.PosInInput currentContext.GssVertex dummy
        
        /// Nonterminal transitions. Move pointer in grammar. Position in input is not changed.
        for curNonterm, nextState in possibleNontermMovesInGrammar do            
            create currentContext nextState curNonterm

        /// Terminal transitions.
        input.ForAllOutgoingEdges
            currentContext.PosInInput
            (fun nextToken nextPosInInput -> 
                let isTransitionPossible, nextPosInGrammar = parser.StateAndTokenToNewState.TryGetValue (parser.GetTermsDictionaryKey currentContext.PosInGrammar (int nextToken))
                if isTransitionPossible
                then eatTerm currentContext nextToken nextPosInInput nextPosInGrammar
                   //pushContext nextPosInInput nextPosInGrammar currentContext.GssVertex (currentContext.Length + 1us)
            )

    gss, sppf,
        if buildTree
        then 
            Some <| new Tree<_>(sppf.GetRoots gss input.InitialPositions.[0], input.PositionToString)
        else
            None
       
let findVertices (gss:GSS) state : seq<GSSVertex> =    
    gss.Vertices
    |> Seq.filter (fun v -> v.Nonterm = state)

let buildAst (parser : ParserSourceGLL) (input : IParserInput) = 
    let _, _, tree = parse parser input true
    let tree = if tree.IsNone
                then failwith "NotParsed"
                else tree.Value
    tree
        
let isParsed (parser : ParserSourceGLL) (input : LinearInput) = 
    let gss, _, _ = parse parser input false
    findVertices gss parser.StartState
    |> Seq.exists (fun v -> v.P.SetP |> ResizeArray.exists (fun p -> int p.posInInput = input.Input.Length))

let getAllRangesForState gss state =
    findVertices gss state
    |> Seq.collect (fun v -> v.P.SetP |> Seq.map (fun poped -> v.PositionInInput, poped.posInInput))    

let getAllRangesForStartState (parser : ParserSourceGLL) (input : IParserInput) = 
    let gss, _, _ = parse parser input false
    getAllRangesForState gss parser.StartState

let getAllRangesForStateWithLength gss state =
    findVertices gss state
    |> Seq.collect (fun v -> v.P.SetP |> Seq.map (fun poped -> v.PositionInInput, poped.posInInput, match poped.data with Length x -> x | TreeNode _ -> failwith "Impossible!"))

let getAllRangesForStartStateWithLength (parser : ParserSourceGLL) (input : IParserInput) = 
    let gss, _, _ = parse parser input false
    getAllRangesForStateWithLength gss parser.StartState