namespace ControlFlowGraph

open System.IO
open System.Collections.Generic

open ControlFlowGraph.Common
open ControlFlowGraph.CfgBuilder
open ControlFlowGraph.CfgElements
open ControlFlowGraph.InputStructures

open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.FSA.FsaApproximation

open Yard.Generators.Common.AST

open Printers

type DefinedIds<'BackReference when 'BackReference : equality> = 
    //this variables is defined on all possible paths
    //under any configuration
    val Always : FSA<char * Position<'BackReference>>
    //this variables is defined on some paths or/and 
    //under some configuration.
    val Somewhere : FSA<char * Position<'BackReference>>

    new () = {Always = new FSA<_>(); Somewhere = new FSA<_>()}
    new (always, somewhere) = {Always = always; Somewhere = somewhere}

    ///complement (always union somewhere)
    member this.GetFullComplementation (fsaParams : FsaParams<_, _>) = 
        let union = FSA<_>.Union(this.Always, this.Somewhere)
        union.Complementation(fsaParams.Alphabet, fsaParams.NewSymbol, fsaParams.GetChar)

    static member Intersect (fsaInfo : FsaParams<_, _>) (one : DefinedIds<_>) (two : DefinedIds<_>) = 
        
        let union one two = FSA.Union(one, two)
        let intersect one two = FSA.Intersection(one, two, fsaInfo.SymbolsAreEqual)

        let subtractFsa one (two : FSA<_>) = 
            let twoComplement = two.Complementation (fsaInfo.Alphabet, fsaInfo.NewSymbol, fsaInfo.GetChar)
            intersect one twoComplement

        let resultAlways = intersect one.Always two.Always
        let resultMaybe = intersect one.Somewhere two.Somewhere

        new DefinedIds<_>(resultAlways, resultMaybe)

    static member AreEqual (fsaInfo : FsaParams<_, _>) (one : DefinedIds<_>) (two : DefinedIds<_>) = 
        
        let isSubFsa one two = 
            FSA<_>.IsSubFsa(one, two, fsaInfo)

        let areEqual one two = 
            isSubFsa one two && isSubFsa two one 

        areEqual one.Always two.Always && areEqual one.Somewhere two.Somewhere
        

type ErrorType<'TokenType, 'BackReference when 'BackReference : equality> =
| NoError 
| MaybeError of 'TokenType * FSA<char * Position<'BackReference>>
| AlwaysError of 'TokenType * FSA<char * Position<'BackReference>>

    static member IsError errorType = 
        match errorType with
        | NoError -> false
        | _ -> true

    static member AreEqual compare (one : ErrorType<_, _>) (two : ErrorType<_, _>) = 
        match one, two with
        | NoError, NoError -> true
        | MaybeError(token1, fsa1), MaybeError(token2, fsa2) 
        | AlwaysError(token1, fsa1), AlwaysError(token2, fsa2) -> 
            compare token1 token2
        | _ -> false

type ControlFlow<'TokenType, 'BackReference when 'BackReference : equality> 
                            (tree : Tree<'TokenType>
                            , generatedStuff : GeneratedStuffSource<'TokenType, 'BackReference>
                            , langSource : LanguageSource
                            , tokToSourceString : _ -> string) = 
    
    let intToToken i = tree.Tokens.[i]

    let isNotAssign token = 
        let assignNumber = langSource.KeywordToInt.[Keyword.ASSIGN]
        generatedStuff.TokenToNumber token <> assignNumber

    let isVariable = 
        generatedStuff.TokenToNumber >> langSource.IsVariable

    let entry, exit = buildCfg tree generatedStuff langSource tokToSourceString

    let rec func' getNext acc (queue : _ list) = 
        
        match queue with
        | [] -> acc
        | head :: tail -> 
            if acc |> List.exists ((=) head)
            then 
                func' getNext acc tail
            else
                let children = getNext head
                func' getNext (head :: acc) <| List.append tail children

    let blocks = 
        
        let getNext (block : Block<_>)= 
            block.Children
            |> List.map (fun node -> node.Children)
            |> List.concat

        func' getNext [] entry.Children
        |> List.rev
        
    let nodes = 
        
        let getNext (node : InterNode<_>) = 
            node.Children
            |> List.map (fun block -> block.Children)
            |> List.concat

        func' getNext [] [entry]
        |> List.rev

    let getFsaInfo() = 
        match generatedStuff.FsaInfo with
        | Some info -> info
        | None -> raise <| invalidOp "Missing FsaInfo"

    let intersectFsa one two = 
        let fsaInfo = getFsaInfo()
        FSA.Intersection(one, two, fsaInfo.SymbolsAreEqual)

    let isSubFsa one two = 
        let fsaInfo = getFsaInfo()
        FSA.IsSubFsa(one, two, fsaInfo)

    let unionFsa one two = 
        FSA.Union(one, two)

    // one \ two
    let subtractFSA (one : FSA<_>) (two : FSA<_>) =
        let fsaInfo = getFsaInfo()
        let twoComplementation = two.Complementation(fsaInfo.Alphabet, fsaInfo.NewSymbol, fsaInfo.GetChar)

        intersectFsa one twoComplementation

    let tokenToFsa token = 
        (generatedStuff.TokenToData token) :?> FSA<char * Position<_>>

    let processNewVariable (previous : DefinedIds<_>) (variable : FSA<_>)= 
        if variable.ContainsOneWordOnly()
        then
            let prevAlways = previous.Always
            let newAlways = unionFsa prevAlways variable
            let newMaybe = subtractFSA previous.Somewhere variable
            new DefinedIds<_>(newAlways, newMaybe)
        else
            let newMaybe =
                subtractFSA variable previous.Always
                |> unionFsa previous.Somewhere

            new DefinedIds<_>(previous.Always, newMaybe)

    let checkExpression (definedVariables : DefinedIds<_>) (expressionBlock : ExpressionBlock<_>) = 
        
        let printToken token = 
            
            printfn "%s " <| tokToSourceString token
            token
            
        let printVariable token = 
            let fsa = tokenToFsa token
            
            let tokenString = 
                fsa.Edges
                |> Seq.map (fun edge -> getFsaInfo().GetChar edge.Tag)
                
            printfn "variable %A" tokenString
            token

        let analyseVariable token = 
            let tokenFsa = tokenToFsa token
            
            let fsaInfo = getFsaInfo()
            let full = definedVariables.GetFullComplementation(fsaInfo)
                
            if isSubFsa tokenFsa full
            then 
                AlwaysError(token, tokenFsa)
            else
                let firstPart = intersectFsa tokenFsa definedVariables.Somewhere
                let secondPart = intersectFsa tokenFsa full

                let common = unionFsa firstPart secondPart
                    
                if common.IsEmpty
                then NoError
                else MaybeError(token, common)

        expressionBlock.TokensGraph.GetAvailableTokens()
        //|> Seq.map printToken
        |> Seq.filter isVariable
        //|> Seq.map printVariable
        |> Seq.map analyseVariable
        |> Seq.filter ErrorType<_, _>.IsError
        |> List.ofSeq

    let rec DoMarkup (blockToVars : Dictionary<_, _>) externVariables start =
        
        let rec processBlock (block : Block<_>) = 
            let defined = blockToVars.[block]

            let after =
                match block.BlockType with 
                | Assignment -> 
                    let assignBlock = block :?> AssignmentBlock<_>
                    let leftPart = 
                        assignBlock.Id.GetAvailableTokens()
                        |> List.ofSeq

                    DoMarkup blockToVars defined <| fst assignBlock.RightPart

                    let newIds = 
                        leftPart
                        |> List.tryFind isVariable
                        |> Option.map 
                                (
                                    fun token -> 
                                        let fsa = tokenToFsa token
                                        processNewVariable defined fsa
                                )
                    match newIds with
                    | Some id -> id
                    | None -> defined
                
                | _ -> defined

            block.Children 
            |> List.iter (processInterNode after)

        and processInterNode defined node = 
            let processChild child = 
                match blockToVars.TryGetValue child with
                | true, vars -> 
                    
                    let fsaParams = getFsaInfo()
                    let commonVars = DefinedIds<_>.Intersect fsaParams vars defined

                    //Does something change?
                    if not <| DefinedIds<_>.AreEqual fsaParams vars commonVars
                    then
                        blockToVars.[child] <- commonVars
                        processBlock child
                | false, _ ->
                    blockToVars.[child] <- defined

                    processBlock child
            
            node.Children 
            |> List.iter processChild

        processInterNode externVariables start 

    let rec findErrors (markups : Dictionary<_, _>) start = 
        
        let errors = ref []
        let processed = new HashSet<_>()

        let rec processBlock (block : Block<_>)= 
            let definedVariables = markups.[block]
            let newErrors = 
                match block.BlockType with
                | Expression -> 
                    let expression = block :?> ExpressionBlock<_>
                    checkExpression definedVariables expression 
                | Assignment -> 
                    let assignment = block :?> AssignmentBlock<_>
                    
                    findErrors markups <| fst assignment.RightPart
                | x -> invalidOp <| sprintf "This construction isn't supported now: %A" x

            errors := newErrors @ !errors
            processed.Add block |> ignore

            block.Children
            |> List.iter processInterNode

        and processInterNode node = 
            node.Children
            |> List.filter (fun block -> not <| processed.Contains block)
            |> List.iter processBlock 
        
        start.Children
        |> List.iter processBlock
        
        !errors

    member this.Entry = entry
    member this.Exit = exit

    member this.Blocks = blocks
    member this.Nodes = nodes

    member this.FindUndefinedVariables() = 
        let empty = new DefinedIds<_>()
        let markups = new Dictionary<_, _>()
        DoMarkup markups empty entry
        let errors = findErrors markups entry 

        let guaranteedErrors, potentialErrors = 
            errors
            |> List.fold 
                (
                    fun acc error -> 
                        let guaranteed, maybe = acc
                        match error with
                        | AlwaysError (token, fsa) -> token :: guaranteed, maybe
                        | MaybeError (token, fsa) -> guaranteed, token :: maybe
                        | NoError -> failwith "Invalid state"
                ) ([], [])

        guaranteedErrors, potentialErrors

    member this.PrintToDot (name : string) = 
        let prefix = "_"
        let count = ref -1
        let clustersCount = ref 0

        let innerVertices = ref 0
        let shift num = num + !innerVertices
        
        let blockToNumber = new Dictionary<_, _>()
        let interNodeToNumber = new Dictionary<_, _>()
        
        use out = new StreamWriter(name)
        out.WriteLine("digraph AST {")

        let rec printBlock parentNumber block = 
            let getBlockNumber (block : Block<'TokenType>) = 
                
                match blockToNumber.TryGetValue block with
                | true, num -> num, false
                | false, _ -> 
                    incr count
                    
                    let blockString = block.BlockToString generatedStuff.TokenToString
                    out.WriteLine (sprintf "%d [label=\"%s\",shape=box]" !count blockString)
                    
                    blockToNumber.[block] <- !count
                    !count, true

            let nodeNumber, isNew = getBlockNumber block
            
            //sometimes this code is needed for debug purposes
            (*let needCluster = false
            if needCluster 
            then
                let dotString, dotIn, dotOut = block.GetDotCluster tokToSourceString shift prefix
            
                let clusterString = getClusterDotString <| sprintf "cluster_%d" nodeNumber <| dotString
                out.WriteLine clusterString
                out.WriteLine (sprintf ("%d -> %s") nodeNumber dotIn)
                out.WriteLine (sprintf ("%s -> %d") dotOut nodeNumber)
                *)
            out.WriteLine (sprintf ("%d -> %d") parentNumber nodeNumber)

            if isNew 
            then
                let printNodeWithParentNumber = printInterNode nodeNumber 
                block.Children
                |> List.iter printNodeWithParentNumber

        and printInterNode parentNumber interNode =
            
            let getNodeNumber (node : InterNode<_>) = 
                
                match interNodeToNumber.TryGetValue node with
                | true, num -> num, false
                | false, _ -> 
                    incr count
                    let label = string node
                    let color =
                        if node.Children.Length <= 1 
                        then ""
                        else ",style=\"filled\",fillcolor=red"
                    out.WriteLine (sprintf "%d [label=\"%s\"%s]" !count label color)
                    interNodeToNumber.[node] <- !count
                    !count, true
            
            let nodeNumber, isNew = getNodeNumber interNode

            if parentNumber <> -1 
            then out.WriteLine (sprintf ("%d -> %d") parentNumber nodeNumber)

            if isNew 
            then
                let printBlockWithNumber = printBlock nodeNumber
                interNode.Children
                |> List.iter printBlockWithNumber
                    
        printInterNode -1 this.Entry 
        
        out.WriteLine("}")
        out.Close()