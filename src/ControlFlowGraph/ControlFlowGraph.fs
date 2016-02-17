namespace ControlFlowGraph

open System.IO
open System.Collections.Generic

open ControlFlowGraph.Common
open ControlFlowGraph.CfgBuilder
open ControlFlowGraph.CfgElements
open ControlFlowGraph.InputStructures

open SeqExtension

open Yard.Generators.Common.AST

open Printers

type ControlFlow<'TokenType> (tree : Tree<'TokenType>
                            , parserSource : CfgParserSource<'TokenType>
                            , langSource : LanguageSource
                            , tokToSourceString : _ -> string) = 
    
    let intToToken i = tree.Tokens.[i]

    let isNotAssign token = 
        let assignNumber = langSource.KeywordToInt.[Keyword.ASSIGN]
        parserSource.TokenToNumber token <> assignNumber

    let isVariable = 
        parserSource.TokenToNumber >> langSource.IsVariable

    let entry, exit = buildCfg tree parserSource langSource tokToSourceString

    let blocks = 
        
        let rec func acc (queue : Block<_> list) = 
            match queue with
            | [] -> acc
            | head :: tail -> 
                if acc |> List.exists ((=) head)
                then 
                    func acc tail
                else
                    let children = 
                        head.Children
                        |> List.map(fun node -> node.Children)
                        |> List.concat
                    func (head :: acc) <| List.append tail children

        func [] entry.Children
        |> List.rev
        
    let nodes = 
        let rec func acc queue = 
            match queue with
            | [] -> acc
            | (head : InterNode<_>) :: tail -> 
                if acc |> List.exists ((=) head)
                then 
                    func acc tail
                else 
                    let children = 
                        head.Children
                        |> List.map (fun block -> block.Children)
                        |> List.concat
                    func (head :: acc) <| List.append tail children

        func [] [entry]
        |> List.rev

    let findUndefVariable() = 
        let blockToVars = new Dictionary<_, _>()
        let errorList = ref []
        
        let rec processBlock (block : Block<_>) = 
            let prevVars = blockToVars.[block]
            let defVars = ref prevVars
            let mutable newVar = None

            let tokens = 
                match block.BlockType with 
                | Assignment -> 
                    let leftPart = 
                        block.TokensGraph.GetAvailableTokens()
                        |> Seq.takeWhile isNotAssign
                        |> List.ofSeq

                    if leftPart.Length = 1 
                    then
                        let varName = leftPart.Head |> tokToSourceString
                        newVar <- Some varName
                    
                    block.TokensGraph.GetAvailableTokens()
                    |> Seq.skipWhile isNotAssign
                    |> List.ofSeq
                    |> List.tail
                | _ -> block.TokensGraph.GetAvailableTokens() |> List.ofSeq

            let isUndefinedVariable token = 
                let varName = token |> tokToSourceString
                !defVars |> List.forall ((<>) varName)
            
            let isNewError token = 
                let tokData = parserSource.TokenToData token
                !errorList
                |> List.forall (fun t -> parserSource.TokenToData t <> tokData) 

            tokens
            |> List.filter isVariable
            |> List.filter isUndefinedVariable
            |> List.filter isNewError
            |> List.iter (fun token -> errorList := token :: !errorList)

            newVar
            |> Option.iter (fun varName -> defVars := varName :: !defVars)
                
            block.Children 
            |> List.iter (processInterNode !defVars)

        and processInterNode defVars node = 
            let processChild child = 
                match blockToVars.TryGetValue child with
                | true, vars -> 
                    let commonVars = List.intersect vars defVars

                    //Does list change?
                    if vars.Length <> commonVars.Length
                    then
                        blockToVars.[child] <- commonVars
                        processBlock child
                | false, _ ->
                    blockToVars.[child] <- defVars
                    processBlock child
            
            node.Children 
            |> List.iter processChild

        processInterNode [] entry 
        !errorList

    member this.Entry = entry
    member this.Exit = exit

    member this.Blocks = blocks
    member this.Nodes = nodes

    member this.FindUndefVariable() = findUndefVariable()

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
                    
                    let blockString = block.BlockToString parserSource.TokenToString
                    out.WriteLine (sprintf "%d [label=\"%s\",shape=box]" !count blockString)
                    
                    blockToNumber.[block] <- !count
                    !count, true

            let nodeNumber, isNew = getBlockNumber block
            
            let needCluster = false
            if needCluster 
            then
                let dotString, dotIn, dotOut = block.GetDotCluster tokToSourceString shift prefix
            
                let clusterString = getClusterDotString <| sprintf "cluster_%d" nodeNumber <| dotString
                out.WriteLine clusterString
                out.WriteLine (sprintf ("%d -> %s") nodeNumber dotIn)
                out.WriteLine (sprintf ("%s -> %d") dotOut nodeNumber)

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
                    let label = node.ToString()
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