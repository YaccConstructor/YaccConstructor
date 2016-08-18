module ControlFlowGraph.CfgElements

open ControlFlowGraph.Common
open ControlFlowGraph.CfgTokensGraph
open ControlFlowGraph.Printers

//if acc doesn't contain elem 
//then elem will be added to acc
let addIfNew acc elem = 
    if acc |> List.forall ((<>) elem)
    then elem :: acc
    else acc

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
        and set value = parent <- value
    
    member this.Children 
        with get() = children
        and set value = children <- value

    member this.BlockType = blockType

    member this.ReplaceParent parent = this.Parent <- parent
    
    member this.AddChild child = 
        
        children <- addIfNew children child

    member this.ReplaceChild oldChild newChildren = 
        children <- children |> List.filter ((<>) oldChild)
        children <- 
            newChildren
            |> List.foldBack (fun elem acc -> elem :: acc) children

    member this.BlockToString tokToString = 
        let typeStr = string blockType

        let strValues = 
            this.TokensGraph.GetAvailableTokens()
            |> Seq.fold (fun acc elem -> sprintf "%s %s" acc <| tokToString elem) ""
        
        sprintf "%s\n available tokens: %s\n" typeStr strValues

    member this.GetDotCluster tokToString shift prefix = 
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
and InterNode<'TokenType>(children : Block<'TokenType> list, parents : Block<'TokenType> list) =
    let mutable parents  = parents
    let mutable children = children
   
    new () = InterNode<_>([], [])

    member this.Parents 
        with get() = parents
        and set value = parents <- value
    
    member this.Children 
        with get() = children
        and set value = children <- value

    member this.AddChild (newChild : Block<'TokenType>) : unit = 
        
        children <- addIfNew children newChild 

    member this.AddParent (newParent : Block<'TokenType>) : unit = 
        
        parents <- addIfNew parents newParent

    /// <summary>
    /// <para>Replaces exit node on some other node.</para><br />
    /// <para>Possible duplicated are ignored.</para>
    /// </summary>
    member this.ReplaceMeForParentsOn (newNode : InterNode<_>) = 
        
        if newNode <> this
        then
            let temp = 
                this.Parents
                |> List.foldBack (fun elem acc -> addIfNew acc elem) newNode.Parents
            
            newNode.Parents <- temp

            this.Parents
            |> List.iter (fun block -> block.ReplaceChild this [newNode])

    /// <summary>
    /// <para>Replaces entry node on some other node.</para><br />
    /// <para>Possible duplicated are ignored.</para>
    /// </summary>
    member this.ReplaceMeForChildrenOn (newNode : InterNode<_>) = 

        if newNode <> this
        then
            let temp = 
                this.Children
                |> List.foldBack (fun elem acc -> addIfNew acc elem) newNode.Children

            newNode.Children <- temp
            
            this.Children
            |> List.iter (fun block -> block.ReplaceParent newNode)

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

type AssignmentBlock<'TokenType>(tokens, leftPart, rightPart) =
    inherit Block<'TokenType>(Assignment, tokens)

    member this.Id = leftPart

    member this.RightPart = rightPart

    static member Create (leftPart : CfgTokensGraph<_>) (rightPart : InterNode<'TokenType> * InterNode<'TokenType>) = 
        
        //create a normal implemetation
        let assignBlock = new AssignmentBlock<'TokenType>(leftPart, leftPart, rightPart) :> Block<'TokenType>

        let res = Block.AttachParentAndChild assignBlock
        res

type ExpressionBlock<'TokenType>(tokens) =
    inherit Block<'TokenType>(Expression, tokens)

    static member Create (graph : CfgTokensGraph<_>) = 
        let expressionBlock = new ExpressionBlock<'TokenType>(graph) :> Block<'TokenType>

        let res = Block.AttachParentAndChild expressionBlock
        res

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