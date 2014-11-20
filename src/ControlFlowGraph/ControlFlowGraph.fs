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
    val mutable children : InterNode<'TokenType>

//    val mutable parents : UsualOne<InterNode<'TokenType>>

    new (block, vals, children) = {blockType = block; values = vals; children = children(*; parents = Unchecked.defaultof<UsualOne<_>>*)}

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

    static member defaultBlock = Block<'TokenType>(None, [||], new InterNode<'TokenType>([]))

//    static member addParent (parent : InterNode<'TokenType>) = 
//        if this.parents.first = null 
//        then 
//            this.parents <- UsualOne (parent)
//        else
//            this.parent 

and InterNode<'TokenType> =
    val mutable children : Block<'TokenType> list
    
    new (children) = {children = children}

    member this.AddChild(block) = 
        if this.children.Length = 0 
        then 
            this.children <- [ block ]
        else
            this.children <- List.append <| this.children <| [ block ]

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

            let block = new Block<'TokenType>(blockType, toks, new InterNode<'TokenType>([]))
            block

        let concatBlocks (blocks : list<Block<_> * Block<_>>) = 
            let startBlock = fst blocks.Head
            let mutable finishBlock = snd blocks.Head
                            
            for first, last in blocks.Tail do
                finishBlock.children.AddChild first
                finishBlock <- last
                            
            startBlock, finishBlock

        let createIfBlock (blocks : list<Block<_> * Block<_>>) = 
            let outBlock = Block.defaultBlock

            let ifBlock = fst blocks.Head
            let startThenBlock, endThenBlock = blocks.Tail.Head

            ifBlock.children.AddChild startThenBlock
            
            let elseExists = blocks.Length > 1
            
            if elseExists 
            then
                let startElseBlock, endElseBlock = List.nth blocks 2
                ifBlock.children.AddChild startElseBlock

                endElseBlock.children.AddChild outBlock

            endThenBlock.children.AddChild outBlock

            ifBlock, outBlock

        let stack = System.Collections.Generic.Stack<_>()
        let blockType = ref Start
        let blocks = ref []
        let tokens = ref []

        let restoreContext() = 
            let oldType, oldBlocks, oldTokens = stack.Pop()
                        
            blockType := oldType
            blocks := oldBlocks
            tokens := oldTokens
        
        let rec handleAst (ast : AST) = 
            let family = ast.first

            let handleFamily (family : Family) = 
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
                                
                                let newBlock = createNewBlock <| List.rev !tokens <| !blockType
                                tokens := []
                                blocks := (newBlock, newBlock) :: !blocks
                        else
                            tokens := t :: !tokens

                        if tokNumber = langSource.elseNumber
                        then
                            blocks := List.rev <| !blocks
                            let head = blocks.Value.Head
                            let thenBranch = concatBlocks blocks.Value.Tail
                            blocks := [thenBranch; head;]
                        elif tokNumber = langSource.endIfNumber
                        then
                            blocks := List.rev <| !blocks
                            let head = blocks.Value.Head
                            let thenBranch = blocks.Value.Tail.Head
                            let elseBranch = concatBlocks blocks.Value.Tail.Tail

                            blocks := [head; thenBranch; elseBranch]

                    | :? AST as ast ->  
                        let pairBlocks = handleAst ast
                        if fst pairBlocks <> Unchecked.defaultof<Block<_>>
                        then
                            blocks := pairBlocks :: !blocks
                    | _ -> failwithf "Unexpected AST node type in Control-Flow construction"

                if langSource.nodeToType.ContainsKey familyName 
                then 
                    match langSource.nodeToType.[familyName] with
                    | IfStatement -> 
                        stack.Push (!blockType, !blocks, !tokens)
                        blockType := IfStatement
                        blocks := []
                        tokens := []

                        family.nodes.doForAll(fun node -> handle node)

                        let temp = Array.ofList !blocks

                        let ifBlock = createIfBlock !blocks
                        restoreContext()
                        
                        ifBlock

                    | Assignment -> 
                        stack.Push (!blockType, !blocks, !tokens)
                        blockType := Assignment
                        blocks := []
                        tokens := []

                        family.nodes.doForAll(fun node -> handle node)
                        
                        blocks := List.rev !blocks
                        let assignBlock = concatBlocks !blocks
                        
                        restoreContext()

                        assignBlock
                    | _ -> 
                        Unchecked.defaultof<Block<_>>, Unchecked.defaultof<Block<_>>

                else 
                    family.nodes.doForAll (fun node -> handle node)
                    Unchecked.defaultof<Block<_>>, Unchecked.defaultof<Block<_>>

            handleFamily family

        handleAst treeRoot |> ignore
        
        let result = concatBlocks <| List.rev blocks.Value
        
        result

    member this.Start = start
    member this.Finish = finish

    member this.PrintToDot name = 
        let count = ref 0
        let blockToNumber = new System.Collections.Generic.Dictionary<_, _>()
        let interNodeToNumber = new System.Collections.Generic.Dictionary<_, _>()
        
        use out = new System.IO.StreamWriter (name : string)
        out.WriteLine("digraph AST {")

        let rec printBlock (block : Block<'TokenType>) num = 
            let isNew = ref true
            let nodeNumber = 
                if blockToNumber.ContainsKey block 
                then 
                    isNew := false
                    blockToNumber.[block]
                else
                    incr count

                    out.WriteLine (sprintf "%d [label=\"%s\",shape=box]" <| !count <| block.ToString())
                    blockToNumber.Add (block, !count)
                    !count

            let interNodeNumber node = 
                if interNodeToNumber.ContainsKey node 
                then
                    isNew := false
                    interNodeToNumber.[node]
                else
                    incr count
                    out.WriteLine (sprintf "%d [label=\"Node\"]" !count)
                    interNodeToNumber.Add (node, !count)
                    !count
            
            if num <> -1 
            then out.WriteLine (sprintf ("%d -> %d") num nodeNumber)

            let child = block.children
            let childNumber = interNodeNumber child
            out.WriteLine (sprintf ("%d -> %d") nodeNumber childNumber)

            if !isNew 
            then
                for child in child.children do
                    printBlock child childNumber

        printBlock this.Start -1

        out.WriteLine("}")
        out.Close()