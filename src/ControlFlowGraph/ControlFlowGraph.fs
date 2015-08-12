namespace ControlFlowGraph

open System.IO
open System.Collections.Generic

open ControlFlowGraph.Common
open ControlFlowGraph.Helper
open ControlFlowGraph.CfgElements
open ControlFlowGraph.InnerGraph
open ControlFlowGraph.InputStructures
open ControlFlowGraph.GraphInterpreter
open ControlFlowGraph.TokensExtractor

open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode

type ControlFlow<'TokenType> (tree : Tree<'TokenType>
                            , parserSource : CfgParserSource<'TokenType>
                            , langSource : LanguageSource
                            , tokToSourceString : _ -> string) = 
    
    let intToToken = fun i -> tree.Tokens.[i]

    let entry, exit = 
        let treeRoot = 
            match tree.Root with 
            | :? AST as ast -> ast
            | _ -> null
        
        let familyToStartVertex = new Dictionary<_, _>()
        let familyToEndVertex = new Dictionary<_, _>()

        let rec handleAst (ast : AST) (parentGraphInfo : GraphConstructor<_>) = 
        
            let handleFamily (family : Family) = 
                let familyName = parserSource.LeftSides.[family.prod] |> parserSource.NumToString

                let handle (node : obj) (parentGraphInfo : GraphConstructor<_>) = 
                    match node with 
                    | :? Epsilon 
                    | :? Terminal -> ()
                    | :? AST as ast -> handleAst ast parentGraphInfo
                    | x -> failwithf "Unexpected AST node type in Control Flow graph construction: %s" <| x.GetType().ToString()


                if langSource.NodeToType.ContainsKey familyName 
                then 
                    match langSource.NodeToType.[familyName] with
                    | IfStatement -> 

                        let tempDict = langSource.GetTempIfDict()
                        
                        let ifGraph = processIf family handle intToToken tempDict parserSource.TokenToNumber
                        let ifTag = Complicated (IfStatement, ifGraph)
                        parentGraphInfo.AddEdge ifTag
                        parentGraphInfo.UpdateVertices()

                    | Assignment -> 
                        //extracts tokens from node
                        let toksGraph = extractNodesFromFamily family
                        
                        let assignmentGraph = new GraphConstructor<_>()

                        toksGraph.CollectAllTokens()
                        |> List.map(fun tokensSet -> tokensSet |> List.map intToToken)
                        |> List.iter(fun tokensSet -> assignmentGraph.AddEdge <| Simple tokensSet)

                        let assignmentTag = Complicated (Assignment, assignmentGraph.Graph)
                        parentGraphInfo.AddEdge assignmentTag
                        parentGraphInfo.UpdateVertices()
                    | _ -> ()

                else 
                    family.nodes.doForAll (fun node -> handle node parentGraphInfo)

            let family = ast.first

            let commonStart = parentGraphInfo.StartVertex
            
            if familyToStartVertex.ContainsKey family 
            then 
                let target = familyToStartVertex.[family]
                
                parentGraphInfo.AddEdgeFromTo EmptyEdge commonStart target
                printfn "familyToEndVertex.Contains family %b" <| familyToEndVertex.ContainsKey family
                //what about familyToEndVertex?
            else
                familyToStartVertex.[family] <- parentGraphInfo.StartVertex
                handleFamily family
                familyToEndVertex.[family] <- parentGraphInfo.StartVertex

            let finishNumber = familyToEndVertex.[family]
            if ast.other <> null 
            then
                let newEndNumbers = 
                    ast.other
                    |> Array.map
                        (
                            fun family -> 
                                parentGraphInfo.StartVertex <- commonStart
                                
                                if familyToStartVertex.ContainsKey family 
                                then 
                                    let target = familyToStartVertex.[family]
                
                                    parentGraphInfo.AddEdgeFromTo EmptyEdge commonStart target
                                    printfn "familyToEndVertex.Contains family %b" <| familyToEndVertex.ContainsKey family
                                    //what about familyToEndVertex?
                                else
                                    familyToStartVertex.[family] <- parentGraphInfo.StartVertex
                                    handleFamily family
                                    familyToEndVertex.[family] <- parentGraphInfo.StartVertex
                                
                                if familyToEndVertex.ContainsKey family
                                then Some familyToEndVertex.[family]
                                else None
                        )
                    |> Array.append [|Some finishNumber|]

                let commonFinish = parentGraphInfo.EndVertex
                newEndNumbers
                |> Array.iter 
                    (
                        Option.iter (fun num -> parentGraphInfo.AddEdgeFromTo EmptyEdge num commonFinish)
                    )
                parentGraphInfo.UpdateVertices()

        let graphInfo = new GraphConstructor<_>()
        handleAst treeRoot graphInfo
        
        graphToCfg graphInfo.Graph (Some parserSource.TokenToString)

    let findUndefVariable() = 
        
        let blockToVars = new Dictionary<_, _>()
        let errorList = ref []
        
        let rec processBlock (block : Block<_>) = 
            
            let prevVars = blockToVars.[block]
            let defVars = ref prevVars
            let mutable newVar = None

            let eqNumber = langSource.KeywordToInt.[EQ]

            let tokens = 
                match block.BlockType with 
                | Assignment -> 
                    let leftPart = 
                         block.Values 
                         |> Seq.takeWhile (fun tok -> parserSource.TokenToNumber tok <> eqNumber)
                         |> List.ofSeq

                    if leftPart.Length = 1 
                    then
                        let varName = leftPart.Head |> tokToSourceString
                        newVar <- Some <| varName
                    
                    block.Values 
                    |> Seq.skipWhile (fun tok -> parserSource.TokenToNumber tok <> eqNumber)
                    |> List.ofSeq
                    |> List.tail

                | _ -> block.Values |> List.ofArray

            for tok in tokens do 
                let tokNumber = parserSource.TokenToNumber tok

                if langSource.IsVariable tokNumber
                then
                    let varName = tok |> tokToSourceString

                    //Is it error?
                    if List.forall ((<>) varName) !defVars
                    then
                        let tokData = parserSource.TokenToData tok
                        //Is it new error?
                        if List.forall (fun t -> parserSource.TokenToData t <> tokData) !errorList     
                        then errorList := tok :: !errorList 

            if newVar.IsSome 
            then defVars := newVar.Value :: !defVars
                
            block.Children 
            |> List.iter (fun child -> processInterNode child !defVars)

        and processInterNode node defVars = 
            
            let intersect one two = 
                one 
                |> List.filter (fun elem1 -> List.exists ((=) elem1) two)

            node.Children 
            |> List.iter 
                (
                    fun child -> 
                        if blockToVars.ContainsKey child 
                        then
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
                )

        processInterNode entry []

        !errorList

    let calculateNodesCount() = 
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

        markedNodes.Count

    let calculateBlocksCount() = 
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

        markedBlocks.Count

    member this.Entry = entry
    member this.Exit = exit

    member this.CalculateBlocksCount() = calculateBlocksCount()
    member this.CalculateNodesCount() = calculateNodesCount()

    member this.FindUndefVariable() = findUndefVariable()

    member this.PrintToDot name = 
        let count = ref -1
        let blockToNumber = new Dictionary<_, _>()
        let interNodeToNumber = new Dictionary<_, _>()
        
        use out = new StreamWriter (name : string)
        out.WriteLine("digraph AST {")

        let rec printBlock block parentNumber = 
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
                block.Children
                |> List.iter (fun child -> printInterNode child nodeNumber)

        and printInterNode interNode parentNumber =
            
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
                interNode.Children
                |> List.iter (fun block -> printBlock block nodeNumber)
                    
        printInterNode this.Entry -1

        out.WriteLine("}")
        out.Close()