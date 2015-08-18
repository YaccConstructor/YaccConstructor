module ControlFlowGraph.CfgElements

open ControlFlowGraph.Common

/// <summary>
/// Control flow graph blocks.
/// </summary>
type Block<'TokenType>(blockType, toks) = 
    let mutable blockType = blockType
    let mutable tokens = toks
    let mutable parent = new InterNode<'TokenType>()
    let mutable children = []

    member this.Tokens = tokens
    member this.Parent 
        with get() = parent
        and set(value) = parent <- value
    
    member this.Children 
        with get() = children
        and set(value) = children <- value

    member this.BlockType = blockType

    member this.ReplaceParent parent = this.Parent <- parent
    
    member this.AddChild (child : InterNode<_>) = 
        if children.IsEmpty 
        then children <- [child]
        //checks if it's new child
        elif children |> List.forall ((<>) child)
        then children <- children @ [child]

    member this.ReplaceChild oldChild newChildren = 
        children <- children |> List.filter ((<>) oldChild)
        children <- children @ newChildren

    member this.BlockToString (tokToString : 'TokenType -> string) = 
        let typeStr = BlockType.BlockTypeToString blockType

        let strValues = 
            this.Tokens
            |> Array.map tokToString
            |> String.concat " "

        sprintf "%s\n tokens: %s\n" typeStr strValues

    static member AttachParentAndChild (newBlock : Block<'TokenType>)= 
        let parent = new InterNode<_>()
        parent.AddChild newBlock
        newBlock.ReplaceParent parent

        let child = new InterNode<_>()
        newBlock.AddChild child
        child.AddParent newBlock
        
        newBlock
    
/// <summary>
/// Intermediate nodes between Blocks. 
/// </summary>
and InterNode<'TokenType>(children : list<_>, parents : list<_>) =
    let mutable parents  = parents
    let mutable children = children
   
    new () = InterNode<_>([], [])

    member this.Parents 
        with get() = parents
        and set(value) = parents <- value
    
    member this.Children 
        with get() = children
        and set(value) = children <- value

    member this.AddChild (block : Block<'TokenType>) : unit = 
        if children.IsEmpty
        then children <- [block]
        elif children |> List.forall ((<>) block)
        then children <- children @ [block]

    member this.AddParent (parent : Block<'TokenType>) : unit = 
        if parents.IsEmpty
        then parents <- [parent]
        else parents <- parents @ [parent]

    /// <summary>
    /// <para>Replaces exit node on some other node.</para><br />
    /// <para>Possible duplicated are ignored.</para>
    /// </summary>
    member this.ReplaceMeForParentsOn (node : InterNode<_>) = 
        
        if node <> this
        then
            let temp = 
                this.Parents
                |> List.fold
                    (
                        fun acc parent ->
                            if acc |> List.forall ((<>) parent)
                            then acc @ [parent]
                            else acc
                    ) 
                    node.Parents
            
            node.Parents <- temp

            this.Parents
            |> List.iter (fun block -> block.ReplaceChild this [node])

    /// <summary>
    /// <para>Replaces entry node on some other node.</para><br />
    /// <para>Possible duplicated are ignored.</para>
    /// </summary>
    member this.ReplaceMeForChildrenOn (node : InterNode<_>) = 

        if node <> this
        then
            let temp = 
                this.Children
                |> List.fold
                    (
                        fun acc child -> 
                            if acc |> List.forall ((<>) child)
                            then acc @ [child]
                            else acc
                    ) node.Children

            node.Children <- temp
            
            this.Children
            |> List.iter (fun block -> block.ReplaceParent node)

    member this.IsEmpty = this.Parents.IsEmpty && this.Children.IsEmpty 

    override this.ToString() = 
        if this.IsEmpty
        then "Empty Node"
        else 
            if this.Parents.IsEmpty
            then "Entry Node"
            elif this.Children.IsEmpty
            then "Exit Node"
            else "Node"

type AssignmentBlock<'TokenType>(tokens) =
    inherit Block<'TokenType>(Assignment, tokens)

    static member Create tokens = 
        let assignBlock = new AssignmentBlock<'TokenType>(tokens) :> Block<'TokenType>

        let res = Block.AttachParentAndChild assignBlock
        res :?> AssignmentBlock<'TokenType>

type ConditionBlock<'TokenType>(tokens) =
    inherit Block<'TokenType>(Condition, tokens)

    static member Create tokens = 
        let condBlock = new ConditionBlock<'TokenType>(tokens) :> Block<'TokenType>

        let res = Block.AttachParentAndChild condBlock
        
        //conditional block has two exit nodes (for 'true' and 'false' cases)
        let sndChild = new InterNode<'TokenType>()
        res.AddChild sndChild
        sndChild.AddParent res

        res :?> ConditionBlock<'TokenType>