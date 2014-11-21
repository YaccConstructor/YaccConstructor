module ControlFlowGraph

open Yard.Generators.Common.AST

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
    | None 

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

type LanguageSource = 
    val nodeToType : System.Collections.Generic.IDictionary<string, BlockType>
    val typeToDelimiters: System.Collections.Generic.IDictionary<BlockType, int list>
    val elseNumber : int
    val endIfNumber : int

    new (nodeToType, typeToDelimiters) = 
        {
            nodeToType = nodeToType; 
            typeToDelimiters = typeToDelimiters;
            elseNumber = -1;
            endIfNumber = -1;
        }

    new (nodeToType, typeToDelimiters, elseNumber, endIfNumber) = 
        {
            nodeToType = nodeToType; 
            typeToDelimiters = typeToDelimiters;
            elseNumber = elseNumber;
            endIfNumber = endIfNumber;
        }

type Block<'TokenType> = 
    val blockType : BlockType
    val values : 'TokenType array
    val mutable children : InterNode<'TokenType> list

    val mutable parent : InterNode<'TokenType>

    new (blockType, vals, children, parent) = {blockType = blockType; values = vals; children = children; parent = parent}
    new (blockType, vals, children) = {blockType = blockType; values = vals; children = children; parent = new InterNode<_>()}
    new (blockType, vals) = {blockType = blockType; values = vals; children = []; parent = new InterNode<_>()}

    override this.ToString() = 
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
            | None -> "None"

        let strValues = 
            this.values
            |> Array.map (fun t -> t.ToString())
            |> String.concat ""

        sprintf "%s\n tokens: %A\n" typeStr this.values //strValues

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

type ControlFlow<'TokenType> (tree : Tree<'TokenType>, parserSource : ParserSource<'TokenType>, langSource : LanguageSource, input : array<'TokenType>) = 
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
                |> List.iter (fun block -> 
                                block.AddChild startElseBlock
                                startElseBlock.AddParent block
                            )
                
                endElseBlock.ReplaceMeForParents outNode

            else 
                startNode.children
                |> List.iter (fun block -> 
                                block.AddChild outNode
                                outNode.AddParent block
                            )

            startNode, outNode

        let stack = System.Collections.Generic.Stack<_>()
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
                        let temp = Array.ofList !blocks

                        let ifStart, ifEnd = createIfBlock !blocks
                        restoreContext()
                        
                        ifStart, ifEnd

                    | Assignment -> 
                        stack.Push (!blockType, !blocks, !tokens, !wasElse)
                        blockType := Assignment
                        blocks := []
                        tokens := []
                        wasElse := false

                        family.nodes.doForAll(fun node -> handle node)
                        
                        blocks := List.rev !blocks
                        let assignBlock = concatBlocks !blocks
                        
                        restoreContext()

                        assignBlock
                    | _ -> 
                        InterNode<_>(), InterNode<_>()

                else 
                    family.nodes.doForAll (fun node -> handle node)
                    InterNode<_>(), InterNode<_>()
            
            let blocksList = ref []

            if ast.other <> null 
            then
                ast.other
                |> Array.iter (fun family -> 
                                        let res = handleFamily family
                                        blocksList := res :: !blocksList
                            )
            
            let family = ast.first
            let startNode, finishNode = handleFamily family

            blocksList.Value
            |> List.rev
            |> List.iter (fun block -> 
                                let startBlock = fst block
                                startBlock.ReplaceMeForChildren startNode

                                let endBlock = snd block
                                endBlock.ReplaceMeForParents finishNode
                         )

            startNode, finishNode

        handleAst treeRoot |> ignore
        
        let result = concatBlocks <| List.rev blocks.Value
        
        result

    member this.Start = start
    member this.Finish = finish

    member this.PrintToDot name = 
        let count = ref -1
        let blockToNumber = new System.Collections.Generic.Dictionary<_, _>()
        let interNodeToNumber = new System.Collections.Generic.Dictionary<_, _>()
        
        use out = new System.IO.StreamWriter (name : string)
        out.WriteLine("digraph AST {")

        let rec printBlock (block : Block<'TokenType>) parentNumber = 
            let getBlockNumber block= 
                if blockToNumber.ContainsKey block 
                then 
                    blockToNumber.[block], false
                else
                    incr count

                    out.WriteLine (sprintf "%d [label=\"%s\",shape=box]" <| !count <| block.ToString())
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