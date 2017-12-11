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

let epsilon = -1<token>

type Counter() = 
    let processed = ref 0
    let mlnCount = ref 0
    let startTime = ref System.DateTime.UtcNow
    
    member this.Lap() = 
        incr processed
        if !processed = 10000000
        then
            incr mlnCount            
            printfn "%A mlns of D procesed. %A D/sec" (!mlnCount * 10) (!processed / int (System.DateTime.UtcNow - !startTime).TotalMilliseconds * 1000)
            processed := 0
            startTime :=  System.DateTime.UtcNow

type Parser(parser : ParserSourceGLL) = 
    let gss = new GSS()
    let gssVertexInstanceHolder = new GSSVertexInstanceHolder()
    let sppf = new SPPF(parser.StartState, parser.FinalStates)
    let tree = ref None
    let input = ref None
    let buildTree = ref false
    let dummy = ref <| Length(0us)
    let descriptorsStack = new System.Collections.Generic.Stack<ContextFSA<_>>()

    member this.SPPF = sppf

    member this.Parse (inpt : IParserInput) (needBuildTree : bool) =
        buildTree := needBuildTree
        input := Some inpt
        dummy := 
            if !buildTree
            then TreeNode(-1<nodeMeasure>)
            else Length(0us)

        let startContexts = 
            input.Value.Value.InitialPositions
            //|> Array.rev
            |> Array.map(fun pos -> 
                let vertex = gssVertexInstanceHolder.Get(parser.StartState, pos)
                gss.AddVertex vertex |> ignore
                new ContextFSA<_>(pos, parser.StartState, vertex, !dummy))

        for cont in startContexts do
            descriptorsStack.Push cont

        this.HandleQ()

    member this.InputUpdated() = 
        for descr in gss.GetAllDescriptors() do
            descriptorsStack.Push descr
        this.HandleQ()

    member this.HandleQ() = 
        /// Adds new descriptor to stack
        let pushContext posInInput posInGrammar gssVertex data =
            descriptorsStack.Push(new ContextFSA<_>(posInInput, posInGrammar, gssVertex, data))

        /// Adds new descriptor to stack if it is first occurrence of this descriptor (if set of all created descriptors doesn't contain it).
        let addContext posInInput posInGrammar (gssVertex:GSSVertex) data =
            if not <| gssVertex.ContainsContext posInInput posInGrammar data
            then pushContext posInInput posInGrammar gssVertex data
    
        let rec pop (posInInput:int<positionInInput>) (gssVertex : GSSVertex) (newData : ParseData)=
            let outEdges = gss.OutEdges gssVertex |> Array.ofSeq
        
            if new PoppedData(posInInput, newData) |> gssVertex.P.Add |> not then () else
            if outEdges <> null && outEdges.Length <> 0
            then
                for e in outEdges do
                    if !buildTree
                    then
                        let y, n = sppf.GetNodes e.Tag.StateToContinue e.Target.Nonterm e.Tag.Data newData
                        if y <> !dummy
                        then
                            addContext posInInput e.Tag.StateToContinue e.Target y
                        if n <> !dummy
                        then
                            pop posInInput e.Target n
                    else
                        if e.Tag.StateToContinue |> parser.FinalStates.Contains
                        then
                            pop posInInput e.Target (summLengths newData e.Tag.Data)
                        addContext posInInput e.Tag.StateToContinue e.Target (summLengths newData e.Tag.Data)

        ///Creates new descriptors.(Calls when found nonterninal in rule(on current input edge, or on some of next)))
        let create (curContext:ContextFSA<_>) stateToContinue nonterm =        
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
                        if !buildTree
                        then 
                            let y,nontermNode = sppf.GetNodes stateToContinue curContext.GssVertex.Nonterm curContext.Data p.data
                            if nontermNode <> !dummy
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
            else addContext curContext.PosInInput nonterm startV !dummy

        let eatTerm (currentContext : ContextFSA<GSSVertex>) nextToken nextPosInInput nextPosInGrammar =
            if !buildTree
            then
                let newR = sppf.GetNodeT nextToken currentContext.PosInInput nextPosInInput
                let y, nontermNode = sppf.GetNodes nextPosInGrammar currentContext.GssVertex.Nonterm currentContext.Data newR

                if nontermNode <> !dummy
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
        
        let counter = new Counter()

        while descriptorsStack.Count <> 0 do
            let currentDescr = descriptorsStack.Pop()

            counter.Lap()

            let possibleNontermMovesInGrammar = parser.OutNonterms.[int currentDescr.PosInGrammar]

            /// Current state is final
            if (currentDescr.Data = !dummy)&&(currentDescr.PosInGrammar |> parser.FinalStates.Contains)
            then 
                if !buildTree
                then
                    let eps = sppf.GetNodeT epsilon currentDescr.PosInInput currentDescr.PosInInput
                    let _, nontermNode = sppf.GetNodes currentDescr.PosInGrammar currentDescr.GssVertex.Nonterm !dummy eps
                    pop currentDescr.PosInInput currentDescr.GssVertex nontermNode
                else
                    pop currentDescr.PosInInput currentDescr.GssVertex !dummy
        
            /// Nonterminal transitions. Move pointer in grammar. Position in input is not changed.
            for curNonterm, nextState in possibleNontermMovesInGrammar do            
                create currentDescr nextState curNonterm

            /// Terminal transitions.
            input.Value.Value.ForAllOutgoingEdges
                currentDescr.PosInInput
                (fun nextToken nextPosInInput -> 
                    let isTransitionPossible, nextPosInGrammar = parser.StateAndTokenToNewState.TryGetValue (parser.GetTermsDictionaryKey currentDescr.PosInGrammar (int nextToken))
                    if isTransitionPossible
                    then eatTerm currentDescr nextToken nextPosInInput nextPosInGrammar
                       //pushContext nextPosInInput nextPosInGrammar currentContext.GssVertex (currentContext.Length + 1us)
                )

        if !buildTree
        then 
            tree := Some <| new Tree<_>(sppf.GetRoots gss input.Value.Value.InitialPositions.[0], input.Value.Value.PositionToString, parser.IntToString)
       
    member this.FindGSSVerticesForStateOfFSA (gss:GSS) state : seq<GSSVertex> =    
        gss.Vertices
        |> Seq.filter (fun v -> v.Nonterm = state)

    member this.BuildAst (input : IParserInput) = 
        this.Parse input true
        let tree = if tree.Value.IsNone
                    then failwith "NotParsed"
                    else tree.Value.Value
        tree
        
    member this.GetAllSPPFRoots (input : IParserInput) = 
        this.Parse input true
        let forest = 
            input.InitialPositions 
            |> Array.choose (fun pos ->
                let roots = sppf.GetRoots gss pos
                if roots.Length <> 0 
                then Some(new Tree<_>(roots, input.PositionToString, parser.IntToString))
                else None) 
        forest

    member this.IsParsed (input : LinearInput) = 
        this.Parse input false
        this.FindGSSVerticesForStateOfFSA gss parser.StartState
        |> Seq.exists (fun v -> v.P.SetP
                                |> ResizeArray.exists (fun p -> int p.posInInput = input.Input.Length))

    member this.GetAllRangesForState gss state =
        this.FindGSSVerticesForStateOfFSA gss state
        |> Seq.collect (fun v -> v.P.SetP
                                 |> Seq.map (fun poped -> v.PositionInInput, poped.posInInput))    

    member this.GetAllRangesForStartState (input : IParserInput) = 
        this.Parse input false
        this.GetAllRangesForState gss parser.StartState

    member this.GetAllRangesForStateWithLength gss state =
        this.FindGSSVerticesForStateOfFSA gss state
        |> Seq.collect (fun v -> v.P.SetP
                                 |> Seq.map (fun poped ->
                                                v.PositionInInput,
                                                poped.posInInput,
                                                match poped.data with
                                                    | Length x -> x
                                                    | TreeNode _ -> failwith "Impossible!"))

    member this.GetGSS(input : IParserInput) = 
        this.Parse input false
        gss

    member this.GetAllRangesForStartStateWithLength (input : IParserInput) = 
        this.Parse input false
        this.GetAllRangesForStateWithLength gss parser.StartState

    member this.GetPrefixTrees(input : IParserInput) = 
        let roots = 
            this.Parse input true
            input.InitialPositions 
                |> Array.choose (fun pos ->
                    let roots = sppf.GetRoots gss pos
                    if roots.Length <> 0 
                    then Some(roots)
                    else None)
        roots
        |> Array.collect id
        |> Array.map(fun x -> GetPrefixTree(x))

