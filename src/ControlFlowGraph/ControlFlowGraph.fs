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

open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode

type private StartEndVertices = 
    val mutable Start : int
    val mutable Finish : int option

    new(start) = {Start = start; Finish = None}

type ControlFlow<'TokenType> (tree : Tree<'TokenType>
                            , parserSource : CfgParserSource<'TokenType>
                            , langSource : LanguageSource
                            , tokToSourceString : _ -> string) = 
    
    let intToToken = fun i -> tree.Tokens.[i]

    let isNotEq token = 
        let eqNumber = langSource.KeywordToInt.[Keyword.EQ]
        parserSource.TokenToNumber token <> eqNumber

    let isVariable = 
        parserSource.TokenToNumber >> langSource.IsVariable

    let processIf' = processIf intToToken parserSource.TokenToNumber <| langSource.GetTempIfDict()

    let entry, exit = 
        let familyToVertices = new Dictionary<_, StartEndVertices>()

        let getEndVertex family = familyToVertices.[family].Finish

        let rec handleNode (node : obj) (currentGraph : GraphConstructor<_>) = 
            let handleFamily (family : Family) = 
                if familyToVertices.ContainsKey family 
                then 
                    let target = familyToVertices.[family].Start
                    currentGraph.AddEdgeFromTo EmptyEdge currentGraph.CurrentVertex target
                    
                    match getEndVertex family with
                    | Some n -> currentGraph.CurrentVertex <- n
                    | None -> 
                        let newEndVertex = currentGraph.FindLastVertex target
                        newEndVertex
                        |> Option.iter(fun n -> currentGraph.CurrentVertex <- n)
                else
                    familyToVertices.[family] <- new StartEndVertices(currentGraph.CurrentVertex)
                    let familyName = parserSource.LeftSides.[family.prod] |> parserSource.NumToString

                    if langSource.NodeToType.ContainsKey familyName 
                    then 
                        let edge = 
                            match langSource.NodeToType.[familyName] with
                            | IfStatement -> processIf' family handleNode
                            | Assignment -> processAssignment intToToken family
                            | x -> failwithf "This construction isn't supported now: %A" x
                    
                        currentGraph.AddEdge edge
                        currentGraph.UpdateVertex()
                    else 
                        family.nodes.doForAll (fun node -> handleNode node currentGraph)
                    familyToVertices.[family].Finish <- Some currentGraph.CurrentVertex
                getEndVertex family

            match node with 
            | :? Epsilon 
            | :? Terminal -> ()
            | :? AST as ast ->
                let commonStart = currentGraph.CurrentVertex
                let endNumbersRef = ref []
                
                let setStartAndHandle family = 
                    currentGraph.CurrentVertex <- commonStart
                    let endNum = handleFamily family
                    endNumbersRef := endNum :: !endNumbersRef
                
                ast.doForAllFamilies setStartAndHandle
                let endNumbers = !endNumbersRef |> List.choose id
                
                if endNumbers.Length = 1
                then 
                    currentGraph.CurrentVertex <- endNumbers.Head
                elif endNumbers.Length >= 2 
                then 
                    let commonFinish = currentGraph.CreateNewVertex()
                    endNumbers
                    |> Seq.iter (fun num -> currentGraph.AddEdgeFromTo EmptyEdge num commonFinish)
                
                    currentGraph.UpdateVertex()
                    
            | x -> failwithf "Unexpected node type: %s" <| x.GetType().ToString()

        let graphInfo = new GraphConstructor<_>()
        handleNode tree.Root graphInfo
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
                         block.Tokens 
                         |> Seq.takeWhile isNotEq
                         |> List.ofSeq

                    if leftPart.Length = 1 
                    then
                        let varName = leftPart.Head |> tokToSourceString
                        newVar <- Some varName
                    
                    block.Tokens
                    |> Seq.skipWhile isNotEq
                    |> List.ofSeq
                    |> List.tail
                | _ -> block.Tokens |> List.ofArray

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

    member this.PrintToDot name = 
        let count = ref -1
        
        let blockToNumber = new Dictionary<_, _>()
        let interNodeToNumber = new Dictionary<_, _>()
        
        use out = new StreamWriter (name : string)
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
            
            out.WriteLine (sprintf ("%d -> %d") parentNumber nodeNumber)

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