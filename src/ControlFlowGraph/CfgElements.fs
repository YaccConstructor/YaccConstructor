module ControlFlowGraph.CfgElements

open ControlFlowGraph.Common
open ControlFlowGraph.CfgTokensGraph
open ControlFlowGraph.Printers

/// <summary>
/// Control flow graph blocks.
/// </summary>
type Block<'TokenType>(blockType, graph : CfgTokensGraph<'TokenType>) = 
    let mutable blockType = blockType
    let mutable tokensGraph = graph
    let mutable parent = new InterNode<'TokenType>()
    let mutable children = []

    member this.TokensGraph = tokensGraph
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
        let typeStr = blockType.ToString()

        let strValues = 
            this.TokensGraph.GetAvailableTokens()
            |> Seq.fold (fun acc elem -> sprintf "%s %s" acc <| tokToString elem) ""
        
        sprintf "%s\n available tokens: %s\n" typeStr strValues

    member this.GetDotCluster tokToString (shift : int -> int) prefix = 
        let graph = this.TokensGraph
        getDotCluster graph tokToString shift prefix


    static member AttachParentAndChild (newBlock : Block<'TokenType>) = 
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

    static member Create (graph : CfgTokensGraph<_>) = 
        let assignBlock = new AssignmentBlock<'TokenType>(graph) :> Block<'TokenType>

        let res = Block.AttachParentAndChild assignBlock
        res/// :?> AssignmentBlock<'TokenType>

type ConditionBlock<'TokenType>(tokens) =
    inherit Block<'TokenType>(Condition, tokens)

    static member Create (graph : CfgTokensGraph<_>) = 
        let condBlock = new ConditionBlock<'TokenType>(graph) :> Block<'TokenType>

        let res = Block.AttachParentAndChild condBlock
        
        //conditional block has two exit nodes (for 'then' and 'else' branches)
        let sndChild = new InterNode<'TokenType>()
        res.AddChild sndChild
        sndChild.AddParent res

        res ///:?> ConditionBlock<'TokenType>