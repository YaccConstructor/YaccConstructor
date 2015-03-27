module ControlFlowGraph

open Yard.Generators.Common.AST
open System.Collections.Generic
open QuickGraph

type BlockType = 
    | Assignment 
    | Declaration 
    | Definition
    | IfStatement 
    | ThenStatement 
    | ElseStatement 
    | ForStatement 
    | WhileStatement
    | Start
    | NoneBlock 

type ParserSource<'TokenType> = 
    val tokenToNumber : 'TokenType -> int
    val leftSides : array<int>
    val tokenData : 'TokenType -> obj
    val numToString : int -> string

    new (tokenToNumber, numToString, leftSides, tokenData) =
        {
            tokenToNumber = tokenToNumber;
            numToString = numToString;
            leftSides = leftSides;
            tokenData = tokenData;
        }

type Edge(source, target, tag) = 
    inherit TaggedEdge<int, int option>(source, target, tag)

type CfgInnerGraph() =
    inherit AdjacencyGraph<int, Edge>()
    static member StartVertex = 0

    member this.AddEdgeForced (e : Edge) =
        this.AddVertex e.Source |> ignore
        this.AddVertex e.Target |> ignore
        this.AddEdge e |> ignore

    member this.CollectAllTokens() = 
        
        let vertexToTokens = Dictionary<int, int list list>()
        let expected = Queue<int>()
        expected.Enqueue CfgInnerGraph.StartVertex

        let endVertex = this.VertexCount - 1

        let handleEdge (edge : Edge) data = 
            let newData = 
                data
                |> List.map 
                        (
                            fun tokList -> 
                                if edge.Tag.IsSome
                                then tokList |> List.append [edge.Tag.Value]
                                else tokList
                        )
            
            let newVertex = edge.Target
            
            if vertexToTokens.ContainsKey(newVertex)
            then
                let oldData = vertexToTokens.[newVertex]
                vertexToTokens.Remove(newVertex) |> ignore
                vertexToTokens.Add(newVertex, List.append oldData newData)
            else
                vertexToTokens.Add(newVertex, newData)
                expected.Enqueue newVertex
            
        let handleVertex vertex = 
            
            let data = 
                if not <| vertexToTokens.ContainsKey vertex 
                then 
                    vertexToTokens.Add (vertex, [[]])
                vertexToTokens.[vertex]
            
            this.OutEdges vertex
            |> Seq.iter(fun edge -> handleEdge edge data)

        while expected.Count > 0 do
            let vertex = expected.Dequeue()
            handleVertex vertex

        vertexToTokens.[endVertex]
        |> List.map (fun set -> set |> List.rev)
        

type LanguageSource = 
    val nodeToType : IDictionary<string, BlockType>
    val typeToDelimiters: IDictionary<BlockType, int list>
    val elseNumber : int
    val endIfNumber : int
    val eqNumber : int
    val isVariable : int -> bool

    new (nodeToType, typeToDelimiters) = 
        {
            nodeToType = nodeToType; 
            typeToDelimiters = typeToDelimiters;
            elseNumber = -1;
            endIfNumber = -1;
            eqNumber = -1;
            isVariable = fun _ -> false;
        }

    new (nodeToType, typeToDelimiters, elseNumber, endIfNumber) = 
        {
            nodeToType = nodeToType; 
            typeToDelimiters = typeToDelimiters;
            elseNumber = elseNumber;
            endIfNumber = endIfNumber;
            eqNumber = -1;
            isVariable = fun _ -> false;
        }

    new (nodeToType, typeToDelimiters, elseNumber, endIfNumber, eqNumber, isVariable) = 
        {
            nodeToType = nodeToType; 
            typeToDelimiters = typeToDelimiters;
            elseNumber = elseNumber;
            endIfNumber = endIfNumber;
            eqNumber = eqNumber;
            isVariable = isVariable;
        }

type Block<'TokenType> = 
    val blockType : BlockType
    val values : 'TokenType array
    val mutable children : InterNode<'TokenType> list

    val mutable parent : InterNode<'TokenType>

    new (blockType, vals, children, parent) = {blockType = blockType; values = vals; children = children; parent = parent}
    new (blockType, vals, children) = {blockType = blockType; values = vals; children = children; parent = new InterNode<_>()}
    new (blockType, vals) = {blockType = blockType; values = vals; children = []; parent = new InterNode<_>()}

    static member CreateSimpleBlock blockType vals = 
        let newBlock = new Block<_>(blockType, vals)
        
        let parent = new InterNode<_>()
        parent.AddChild newBlock
        newBlock.ReplaceParent parent

        let child = new InterNode<_>()
        newBlock.AddChild child
        child.AddParent newBlock
        
        newBlock

    member this.ReplaceParent parent = this.parent <- parent
    
    member this.AddChild child = 
        if this.children.IsEmpty 
        then this.children <- [child]
        else this.children <- this.children @ [child]

    member this.ReplaceChild oldChild newChildren = 
        this.children <- this.children |> List.filter (fun child -> child <> oldChild)
        this.children <- this.children @ newChildren

    member this.BlockToString (tokToString : 'TokenType -> string) = 
        let typeStr = 
            match this.blockType with
            | Assignment -> "Assignment"
            | Declaration -> "Declaration"
            | Definition -> "Definition"
            | IfStatement -> "IfStatement"
            | ThenStatement -> "ThenStatement"
            | ElseStatement -> "ElseStatement"
            | ForStatement -> "ForStatement"
            | WhileStatement -> "WhileStatement"
            | Start -> "Start"
            | NoneBlock -> "None"

        let strValues = 
            this.values
            |> Array.map (fun t -> tokToString t)
            |> String.concat ""

        sprintf "%s\n tokens: %s\n" typeStr strValues

and InterNode<'TokenType> =
    val mutable parents  : Block<'TokenType> list
    val mutable children : Block<'TokenType> list
    
    new (children, parents) = {children = children; parents = parents}
    new (children) = {children = children; parents = []}
    new ()         = {children = []; parents = []}

    member this.AddChild (block : Block<'TokenType>) : unit = 
        if this.children.IsEmpty
        then this.children <- [block]
        else this.children <- this.children @ [block]

    member this.AddParent (parent : Block<'TokenType>) : unit = 
        if this.parents.IsEmpty
        then this.parents <- [parent]
        else this.parents <- this.parents @ [parent]

    /// replace finish node on some other node
    member this.ReplaceMeForParents (node : InterNode<_>) = 
        node.parents <- node.parents @ this.parents
        
        this.parents
        |> List.iter (fun block -> block.ReplaceChild this [node])

    /// replace start node on some other node
    member this.ReplaceMeForChildren (node : InterNode<_>) = 
        node.children <- node.children @ this.children
        
        this.children
        |> List.iter (fun block -> block.ReplaceParent node)

    member this.IsEmpty = this.parents.IsEmpty && this.children.IsEmpty 

    override this.ToString() = 
        if this.IsEmpty
        then "Empty Node"
        else 
            if this.parents.IsEmpty
            then "Start Node"
            elif this.children.IsEmpty
            then "Finish Node"
            else "Node"

type ControlFlow<'TokenType> (tree : Tree<'TokenType>
                            , parserSource : ParserSource<'TokenType>
                            , langSource : LanguageSource
                            , input : array<'TokenType>
                            , tokToSourceString : _ -> string) = 
    
    let blockToString (block : Block<'TokenType>) = 
        let typeStr = 
            match block.blockType with
            | Assignment -> "Assignment"
            | Declaration -> "Declaration"
            | Definition -> "Definition"
            | IfStatement -> "IfStatement"
            | ThenStatement -> "ThenStatement"
            | ElseStatement -> "ElseStatement"
            | ForStatement -> "ForStatement"
            | WhileStatement -> "WhileStatement"
            | Start -> "Start"
            | NoneBlock -> "None"

        let strValues = 
            block.values
            |> Array.map (fun t -> tokToSourceString t)
            |> Array.fold (fun prev tokName -> prev + " " + tokName) System.String.Empty

        sprintf "%s\n tokens: %s\n" typeStr strValues
    
    let start, finish = 
        let treeRoot = 
            match tree.Root with
            | :? AST as ast -> ast
            | _ -> null

        let createNewBlock tokens blockType = 
            let toks = 
                tokens
                |> List.map (fun t -> tree.Tokens.[t])
                |> List.toArray

            let block = Block.CreateSimpleBlock blockType toks
            block.parent, block.children.Head

        let concatBlocks (blocks : list<InterNode<'TokenType> * InterNode<'TokenType>>) = 
            let startNode = fst blocks.Head
            let mutable finishNode = snd blocks.Head
                            
            for first, last in blocks.Tail do
                finishNode.ReplaceMeForParents first
                finishNode <- last
                            
            startNode, finishNode

        let createIfBlock (blocks : list<InterNode<_> * InterNode<_>>) = 
            let outNode = new InterNode<_>()

            let startNode = fst blocks.Head
            
            let ifBlockEnd = snd blocks.Head
            let startThenBlock, endThenBlock = blocks.Tail.Head

            ifBlockEnd.ReplaceMeForParents startThenBlock

            endThenBlock.ReplaceMeForParents outNode

            let elseExists = blocks.Length > 2

            if elseExists 
            then
                let startElseBlock, endElseBlock = List.nth blocks 2
                
                ifBlockEnd.parents
                |> List.iter 
                        (
                            fun block -> 
                                block.AddChild startElseBlock
                                startElseBlock.AddParent block
                        )
                
                endElseBlock.ReplaceMeForParents outNode

            else 
                startNode.children
                |> List.iter 
                        (
                            fun block -> 
                                block.AddChild outNode
                                outNode.AddParent block
                        )

            startNode, outNode

        let stack = Stack<_>()
        let blockType = ref Start
        let blocks = ref []
        let tokens = ref []
        let wasElse = ref false

        let restoreContext() = 
            let oldType, oldBlocks, oldTokens, oldWasElse = stack.Pop()
                        
            blockType := oldType
            blocks := oldBlocks
            tokens := oldTokens
            wasElse := oldWasElse

        let rec collectTokens (node : obj) (graph : CfgInnerGraph) startVertex endVertex = 
            
            match node with 
            | :? int as t -> 
                let tagOpt = if t >= 0 then Some t else None
                let edge = new Edge(!startVertex, !endVertex, tagOpt)
                graph.AddEdgeForced edge 

                startVertex := !endVertex
                incr endVertex

            | :? AST as ast -> 

                let commonStartVertex = !startVertex
                
                let newEndVertex = extractTokensFromFamily ast.first graph (ref commonStartVertex) endVertex
                        
                if ast.other <> null 
                then 
                    let allEndVertex = 
                        ast.other
                        |> Array.map (fun fam -> extractTokensFromFamily fam graph (ref commonStartVertex) endVertex)
                        |> List.ofArray
                        |> List.append [newEndVertex]

                    let commonEndVertex = !endVertex

                    allEndVertex
                    |> List.iter 
                            (
                                fun num -> 
                                    let edge = Edge(num, commonEndVertex, None)
                                    graph.AddEdgeForced edge
                            )
                    startVertex := !endVertex
                    incr endVertex
                else
                    startVertex := newEndVertex

            | _ -> failwith "Unexpected AST node type in Control-Flow construction"

        and extractTokensFromFamily (fam : Family) graph startVertex endVertex = 
            
            fam.nodes.doForAll (fun node -> collectTokens node graph startVertex endVertex)
            !endVertex - 1 //current it because of algorithm realization. Ideally, it should be !endVertex
        

        let rec handleAst (ast : AST) : (InterNode<_> * InterNode<_>) = 
        
            let handleFamily (family : Family) : (InterNode<_> * InterNode<_>) = 
                let familyName = parserSource.leftSides.[family.prod] |> parserSource.numToString

                let handle (node : obj) = 
                    match node with 
                    | :? int as t when t < 0 -> () 
                    | :? int as t -> 
                        let delimiters = langSource.typeToDelimiters.[!blockType]
                        let tokNumber = parserSource.tokenToNumber input.[t]

                        if List.exists (fun num -> num = tokNumber) delimiters
                        then
                            if not <| tokens.Value.IsEmpty 
                            then
                                let nodeIn, nodeOut = createNewBlock <| List.rev !tokens <| !blockType
                                tokens := []
                                blocks := (nodeIn, nodeOut) :: !blocks
                        else
                            tokens := t :: !tokens

                        if tokNumber = langSource.elseNumber || (tokNumber = langSource.endIfNumber && not <| !wasElse)
                        then
                            blocks := List.rev <| !blocks
                            let head = blocks.Value.Head
                            let thenBranch = concatBlocks blocks.Value.Tail
                            blocks := [thenBranch; head;]
                            wasElse := true
                        
                        elif tokNumber = langSource.endIfNumber
                        then
                            blocks := List.rev <| !blocks
                            let head = blocks.Value.Head
                            let thenBranch = blocks.Value.Tail.Head
                            let elseBranch = concatBlocks blocks.Value.Tail.Tail

                            blocks := [elseBranch; thenBranch; head; ]

                    | :? AST as ast ->  
                        let pairBlocks = handleAst ast
                        let fstBlock = fst pairBlocks
                        if not <| fstBlock.IsEmpty
                        then
                            blocks := pairBlocks :: !blocks
                    | _ -> failwithf "Unexpected AST node type in Control-Flow construction"

                if langSource.nodeToType.ContainsKey familyName 
                then 
                    match langSource.nodeToType.[familyName] with
                    | IfStatement -> 
                        stack.Push (!blockType, !blocks, !tokens, !wasElse)
                        blockType := IfStatement
                        blocks := []
                        tokens := []
                        wasElse := false

                        family.nodes.doForAll(fun node -> handle node)

                        blocks := List.rev !blocks
                        // only for debug
//                        let temp = Array.ofList !blocks

                        let ifStart, ifEnd = createIfBlock !blocks
                        restoreContext()
                        
                        ifStart, ifEnd

                    | Assignment -> 
                        stack.Push (!blockType, !blocks, !tokens, !wasElse)
                        blockType := Assignment
                        blocks := []
                        tokens := []
                        wasElse := false

                        let toksGraph = new CfgInnerGraph()
                        let startVertex = ref CfgInnerGraph.StartVertex
                        let endVertex = ref 1
                        family.nodes.doForAll(fun node -> collectTokens node toksGraph startVertex endVertex)
                        
                        let mutable newBlocks = 
                            toksGraph.CollectAllTokens()
                            |> List.map 
                                    (
                                        fun set -> 
                                            let toksFromBlock = 
                                                set 
                                                |> List.map (fun tok -> tree.Tokens.[tok])
                                                |> Array.ofList
                                            Block.CreateSimpleBlock Assignment toksFromBlock
                                    )

                        
                        let commonParent = newBlocks.Head.parent
                        let commonChildren = newBlocks.Head.children.Head

                        
                        newBlocks <- 
                            newBlocks 
                            |> List.tail
                            |> List.map
                                    (
                                        fun block -> 
                                            block.parent.ReplaceMeForChildren commonParent
                                            block.children.Head.ReplaceMeForParents commonChildren
                                            block
                                    )

//                        let assignBlock = concatBlocks !blocks
                        
                        restoreContext()

                        commonParent, commonChildren

                    | _ -> 
                        InterNode<_>(), InterNode<_>()

                else 
                    family.nodes.doForAll (fun node -> handle node)
                    InterNode<_>(), InterNode<_>()
            
            let blocksList = ref []

            if ast.other <> null 
            then
                ast.other
                |> Array.iter 
                        (
                            fun family -> 
                                let res = handleFamily family
                                blocksList := res :: !blocksList
                        )
            
            let family = ast.first
            let startNode, finishNode = handleFamily family

            blocksList.Value
            |> List.rev
            |> List.iter 
                    (
                        fun block -> 
                            let startBlock = fst block
                            startBlock.ReplaceMeForChildren startNode

                            let endBlock = snd block
                            endBlock.ReplaceMeForParents finishNode
                     )

            startNode, finishNode

        handleAst treeRoot |> ignore
        
        let result = concatBlocks <| List.rev blocks.Value
        
        result

    let findUndefVariable() = 
        
        let blockToVars = new Dictionary<_, _>()
        let errorList = ref []
        
        let rec processBlock (block : Block<_>) = 
            
            let prevVars = blockToVars.[block]
            let defVars = ref prevVars
            let mutable newVar = None

            let tokens = 
                match block.blockType with 
                | Assignment -> 
                    let leftPart = 
                         block.values 
                         |> Seq.takeWhile (fun tok -> parserSource.tokenToNumber tok <> langSource.eqNumber)
                         |> List.ofSeq

                    if leftPart.Length = 1 
                    then
                        let varName = leftPart.Head |> tokToSourceString
                        newVar <- Some <| varName
                    
                    block.values 
                    |> Seq.skipWhile (fun tok -> parserSource.tokenToNumber tok <> langSource.eqNumber)
                    |> List.ofSeq
                    |> List.tail

                | _ -> block.values |> List.ofArray

            for tok in tokens do 
                let tokNumber = parserSource.tokenToNumber tok

                if langSource.isVariable tokNumber
                then
                    let varName = tok |> tokToSourceString

                    if not <| List.exists (fun t -> t = varName) !defVars //it's error
                    then
                        let tokData = parserSource.tokenData tok
                        //if it's new error
                        if not <| List.exists (fun t -> parserSource.tokenData t = tokData) !errorList     
                        then errorList := tok :: !errorList 

            if newVar.IsSome 
            then
                defVars := newVar.Value :: !defVars
                
            block.children 
            |> List.iter (fun child -> processInterNode child !defVars)

        and processInterNode node defVars = 
            
            let intersect one two = 
                one 
                |> List.filter (fun elem1 -> List.exists (fun elem2 -> elem1 = elem2) two)

            node.children 
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
                                blockToVars.Add(child, defVars)
                                processBlock child
                    )

        processInterNode start []

        !errorList

    member this.Start = start
    member this.Finish = finish

    member this.FindUndefVariable() = findUndefVariable()

    member this.PrintToDot name = 
        let count = ref -1
        let blockToNumber = new System.Collections.Generic.Dictionary<_, _>()
        let interNodeToNumber = new System.Collections.Generic.Dictionary<_, _>()
        
        use out = new System.IO.StreamWriter (name : string)
        out.WriteLine("digraph AST {")

        let rec printBlock (block : Block<'TokenType>) parentNumber = 
            let getBlockNumber block = 
                if blockToNumber.ContainsKey block 
                then 
                    blockToNumber.[block], false
                else
                    incr count

                    let blockString = blockToString block

                    out.WriteLine (sprintf "%d [label=\"%s\",shape=box]"  !count blockString)
                    blockToNumber.Add (block, !count)
                    !count, true

            let nodeNumber, isNew = getBlockNumber block
            
            out.WriteLine (sprintf ("%d -> %d") parentNumber nodeNumber)

            if isNew then
                block.children
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
                        if node.children.Length <= 1 then ""
                        else ",style=\"filled\",fillcolor=red"
                    out.WriteLine (sprintf "%d [label=\"%s\"%s]" !count label color)
                    interNodeToNumber.Add (node, !count)
                    !count, true
            
            let nodeNumber, isNew = getNodeNumber interNode

            if parentNumber <> -1 
            then out.WriteLine (sprintf ("%d -> %d") parentNumber nodeNumber)

            if isNew 
            then
                interNode.children
                |> List.iter (fun block -> printBlock block nodeNumber)
                    
        printInterNode this.Start -1

        out.WriteLine("}")
        out.Close()