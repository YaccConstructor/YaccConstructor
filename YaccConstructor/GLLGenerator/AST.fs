// AST.fs contains a data structure to represent SPPF during parsing

module Yard.Generators.GLL.AST

type GrammarItem = Trm of int | Ntrm of int

/// <summary>
/// Contains a backward link for a left-to-right travsersal.
/// </summary>
[<AllowNullLiteral>]
type BackNode (node:Node, parentLink) =
    // SPPF node this BackNode stores links for
    member val Node = node with get, set
    // nodes that are previous for this in a left-to-right traversal
    member val PrevNodes = ResizeArray<BackNode> () with get
    // link to the Node's parent that should be removed if this BackNode is removed
    member val ParentLink = parentLink with get
   
    // adds a node that is previous for this in a left-to-right traversal
    member this.addPrev = this.PrevNodes.Add
    // removes this node from left-to-right traversals 
    member this.removePrev = this.PrevNodes.Remove >> ignore
    // removes all backward links from this node
    member this.clearPrev = this.PrevNodes.Clear

/// <summary>
/// SPPF Node that also contains forward links for left-to-right traversals.
/// Graph consisting of all Nodes (except FakeStart and FakeEnd) is a tree (DAG with Next links).
/// </summary>
/// <param name="item">
/// Terminal/nonterminal represented by this node
/// </param>
and [<AllowNullLiteral>] Node (item:GrammarItem) =
    // terminal/nonterminal represented by this node
    member val Item = item with get
    // input buffer position of the matched input terminal
    member val ItemPos = -1 with get, set
    // object that contains links required for deleting stuff
    member val BackNode : BackNode = null with get, set
    // nodes that are the next node for this in a left-to-right traversal
    member val NextNodes = ResizeArray<Node> () with get, set
    // parents of this node in the SPPF
    // each parent is associated with the production number and item index in the production; see (*1*)
    member val Parents = ResizeArray<int * int * Node> () with get
    
    // add a new parent node
    member this.addParent = this.Parents.Add
    // remove the parent node
    member this.removeParent = this.Parents.Remove >> ignore
    // add a node that is next in a left-to-right traversal
    member this.addNext nextNode =
        this.NextNodes.Add nextNode
        nextNode.BackNode.addPrev this.BackNode
    // removes a node that is next in a left-to-right traversal
    member this.removeNext nextNode =
        if this.NextNodes <> null
        then this.NextNodes.Remove nextNode |> ignore
        nextNode.BackNode.removePrev this.BackNode
    // changes a link to next nonterminal to a set of links to its children
    member this.reassignNext oldNode newNodes =
        this.removeNext oldNode
        Seq.iter this.addNext newNodes
        oldNode.NextNodes <- null
    // checks whether there are links to next nodes
    member this.hasNextNodes () =
        this.NextNodes <> null && this.NextNodes.Count > 0

    new (item, next : Node, parentLink) as this =
        Node(item) then
        this.BackNode <- BackNode(this, parentLink)

        next.BackNode.clearPrev ()

        this.addNext(next)
        this.addParent(parentLink)

(*1: condider the following ambiguous grammar:
       S -> AxxA
       A -> e | x
       Given the input xxx, second 'x' in the input can be either first or second 'x' in S production
*)
