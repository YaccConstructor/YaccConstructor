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

    let intersectFsa one two = 
        match generatedStuff.FsaInfo with
        | Some info -> FSA.Intersection(one, two, info.SymbolsAreEqual)
        | None -> raise <| invalidOp "Missing FsaInfo"

    let isSubFsa one two = 
        
        match generatedStuff.FsaInfo with
        | Some info -> not <| FSA.IsSubFsa(one, two, info)
        | None -> raise <| invalidOp "Missing FsaInfo"

    let checkExpression definedVariables (expressionBlock : ExpressionBlock<_>)= 
        
        (*let printToken token = 
            printfn "%s " <| tokToSourceString token
            token
            *)
        (*let printVariable token = 
            printfn "variable %s" <| tokToSourceString token
            token*)

        let isUndefinedVariable token = 
            let tokenFsa = 
                (generatedStuff.TokenToData token) :?> FSA<char * Position<_>>
            
            match definedVariables with
            | [] -> true
            | _ ->
                let definedFsa = 
                    definedVariables
                    |> List.reduce (fun fsa1 fsa2 -> FSA.Union(fsa1, fsa2))
            
                isSubFsa tokenFsa definedFsa
        
        expressionBlock.TokensGraph.GetAvailableTokens()
        |> Seq.filter isVariable
        |> Seq.filter isUndefinedVariable
        |> List.ofSeq

    let rec findUndefinedVariables externVariables start =
        let blockToVars = new Dictionary<_, _>()
        let errorList = ref []
        
        let rec processBlock (block : Block<_>) = 
            let prevVars = blockToVars.[block]
            let defVars = ref prevVars

            let newVariable, errors = 
                match block.BlockType with 
                | Expression -> 
                    let expressionBlock = block :?> ExpressionBlock<_>
                    None, checkExpression !defVars expressionBlock
                | Assignment -> 
                    let assignBlock = block :?> AssignmentBlock<_>
                    let leftPart = 
                        assignBlock.Id.GetAvailableTokens()
                        |> List.ofSeq

                    let newVar = 
                        leftPart
                        |> List.tryFind isVariable
                        |> Option.map
                                (
                                    fun token -> 
                                        let fsa = generatedStuff.TokenToData token
                                        fsa :?> FSA<char * Position<'BackReference>>
                                )

                    let newErrors = findUndefinedVariables !defVars <| fst assignBlock.RightPart
                    newVar, newErrors
                    
                | x -> invalidOp <| sprintf "%A block isn't supported now" x

            let isNewError token = 
                let tokData = generatedStuff.TokenToData token
                !errorList
                |> List.forall (fun t -> generatedStuff.TokenToData t <> tokData) 

            errors
            |> List.filter isNewError
            |> List.iter (fun token -> errorList := token :: !errorList)

            newVariable
            |> Option.iter (fun varName -> defVars := varName :: !defVars)
                
            block.Children 
            |> List.iter (processInterNode !defVars)

        and processInterNode defVars node = 
            let processChild child = 
                match blockToVars.TryGetValue child with
                | true, vars -> 
                    let intersectFsaLists one two = 
                        match one, two with
                        | [], _ 
                        | _, [] -> []
                        | _ -> 
                            let oneCommon = one |> List.reduce (fun one two -> FSA.Union (one, two))
                            let twoCommon = two |> List.reduce (fun one two -> FSA.Union (one, two))
                            let res = intersectFsa oneCommon twoCommon
                            [res]
                    
                    // checks if {was} is subset {now}
                    let isFsaChanged was now = 
                        match was, now with
                        | [], [] -> false
                        | _ , [] -> true
                        | [], _ -> invalidOp "Unexpected state in undefined variables finding"
                        | _ -> 
                            let newVars = List.head now
                            let oldVars = was |> List.reduce (fun one two -> FSA.Union (one, two))

                            isSubFsa oldVars newVars

                    let commonVars = intersectFsaLists vars defVars

                    //checks if variables set are changed
                    if isFsaChanged vars commonVars
                    then
                        blockToVars.[child] <- commonVars
                        processBlock child
                | false, _ ->
                    blockToVars.[child] <- defVars
                    processBlock child
            
            node.Children 
            |> List.iter processChild

        processInterNode externVariables start 
        !errorList

    member this.Entry = entry
    member this.Exit = exit

    member this.Blocks = blocks
    member this.Nodes = nodes

    member this.FindUndefinedVariables() = 
        findUndefinedVariables [] entry

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