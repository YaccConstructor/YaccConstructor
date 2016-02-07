namespace ControlFlowGraph

open System.IO
open System.Collections.Generic

open ControlFlowGraph.Common
open ControlFlowGraph.IfHelper
open ControlFlowGraph.AssignmentHelper
open ControlFlowGraph.CfgElements
open ControlFlowGraph.InnerGraph
open ControlFlowGraph.InputStructures
open ControlFlowGraph.GraphInterpreter
open ControlFlowGraph.TokensExtractor

open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode
open Printers

type ControlFlow<'TokenType> (tree : Tree<'TokenType>
                            , parserSource : CfgParserSource<'TokenType>
                            , langSource : LanguageSource
                            , tokToSourceString : _ -> string) = 
    
    let intToToken = fun i -> tree.Tokens.[i]

    let isNotAssign token = 
        let assignNumber = langSource.KeywordToInt.[Keyword.ASSIGN]
        parserSource.TokenToNumber token <> assignNumber

    let isVariable = 
        parserSource.TokenToNumber >> langSource.IsVariable

    let processIf' = processIf intToToken parserSource.TokenToNumber tokToSourceString <| langSource.GetTempIfDict()
    let processAssignment' = processAssignment intToToken tokToSourceString 

    let entry, exit = 
        let familyToVertices = new Dictionary<_, ASTProcessingState>()

        let rec handleNode (node : obj) (graph : GraphConstructor<_>) = 
            let handleFamily (family : Family) startVertex = 
                graph.CurrentVertex <- startVertex
                if familyToVertices.ContainsKey family 
                then 
                    match familyToVertices.[family] with
                    | Processed (source, target) -> 
                        if graph.CurrentVertex <> source 
                        then 
                            graph.AddEdgeFromTo EmptyEdge graph.CurrentVertex source
                        
                        graph.CurrentVertex <- target
                        graph.LastVertex <- target
                        Some target
                    | InProgress start -> 
                        graph.AddEdgeFromTo EmptyEdge startVertex start
                        match graph.TryFindLastVertex start with
                        | Some vertex ->  
                            familyToVertices.[family] <- Processed(start, vertex)
                            graph.CurrentVertex <- vertex
                            graph.LastVertex <- vertex
                            Some vertex
                        | None -> None
                else
                    familyToVertices.[family] <- InProgress(graph.CurrentVertex)
                    let familyName = parserSource.LeftSides.[family.prod] |> parserSource.NumToString

                    if langSource.NodeToType.ContainsKey familyName 
                    then 
                        let edge = 
                            match langSource.NodeToType.[familyName] with
                            | IfStatement -> processIf' family handleNode
                            | Assignment -> processAssignment' family
                            | x -> failwithf "This construction isn't supported now: %A" x
                    
                        graph.AddEdge edge
                        graph.UpdateVertex()
                        familyToVertices.[family] <- Processed(startVertex, graph.CurrentVertex)
                        Some graph.CurrentVertex
                    else 
                        family.nodes.doForAll (fun node -> handleNode node graph)
                        familyToVertices.[family] <- Processed(startVertex, graph.CurrentVertex)
                        Some graph.CurrentVertex

            match node with 
            | :? Epsilon 
            | :? Terminal -> ()
            | :? AST as ast ->
                let commonStart = graph.CurrentVertex
                let endNumbers = 
                    ast.map (fun family -> handleFamily family commonStart)
                    |> Seq.distinct
                    |> Seq.fold 
                        (
                            fun acc numOpt -> 
                                match numOpt with 
                                | Some num -> num :: acc
                                | None -> acc
                        ) []
                    |> Array.ofSeq
                
                if endNumbers.Length = 1
                then 
                    graph.UpdateVertex()
                elif endNumbers.Length > 1
                then
                    let commonEndVertex = graph.CreateNewVertex()

                    endNumbers
                    |> Array.iter(fun num -> graph.AddEdgeFromTo EmptyEdge num commonEndVertex)
                    graph.UpdateVertex()
                    
            | x -> failwithf "Unexpected node type: %A" x

        let graphInfo = new GraphConstructor<_>()
        handleNode tree.Root graphInfo

        match graphInfo.TryFindLastVertex graphInfo.CurrentVertex with
        | Some _ -> ()
        | None -> graphInfo.AddEdge EmptyEdge

        graphToCfg graphInfo.Graph <| Some parserSource.TokenToString

    let blocks = 
        let markedBlocks = new ResizeArray<_>()
        let queue = new Queue<_>()

        let processNode (node : InterNode<_>) = 
            let processBlock block = 
                if not <| markedBlocks.Contains block
                then
                    markedBlocks.Add block
                    queue.Enqueue block
        
            node.Children 
            |> List.iter processBlock
    
        processNode entry
        while queue.Count > 0 do
            let block = queue.Dequeue()
            block.Children
            |> List.iter processNode

        markedBlocks.ToArray()

    let nodes = 
        let markedNodes = new ResizeArray<_>()
        let queue = new Queue<_>()
        
        let processNode node = 
            if not <| markedNodes.Contains node
            then
                markedNodes.Add node
                queue.Enqueue node

        processNode entry
        while queue.Count > 0 do
            let node = queue.Dequeue()
            node.Children
            |> List.iter(fun block -> block.Children |> List.iter processNode)

        markedNodes.ToArray()

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
//                | _ -> block.Tokens |> List.ofArray
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
            |> List.iter (fun child -> processInterNode child !defVars)

        and processInterNode node defVars = 
            let processChild child = 
                if blockToVars.ContainsKey child 
                then
                    let intersect one two = 
                        one |> List.filter (fun elem1 -> List.exists ((=) elem1) two)
                    let oldVars = blockToVars.[child]
                    let commonVars = intersect oldVars defVars

                    //Does list change?
                    if oldVars.Length <> commonVars.Length
                    then
                        blockToVars.[child] <- commonVars
                        processBlock child
                else
                    blockToVars.[child] <- defVars
                    processBlock child
            node.Children 
            |> List.iter processChild

        processInterNode entry []
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
                if blockToNumber.ContainsKey block 
                then 
                    blockToNumber.[block], false
                else
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
            //out.WriteLine (sprintf ("%d -> %d") parentNumber nodeNumber)

            if isNew 
            then
                let printNodeWithParentNumber = printInterNode nodeNumber 
                block.Children
                |> List.iter printNodeWithParentNumber

        and printInterNode parentNumber interNode =
            
            let getNodeNumber (node : InterNode<_>) = 
                if interNodeToNumber.ContainsKey node 
                then
                    interNodeToNumber.[node], false
                else
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