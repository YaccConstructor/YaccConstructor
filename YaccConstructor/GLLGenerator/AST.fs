// AST.fs contains a data structure to represent SPPF during parsing

module Yard.Generators.GLL.AST

type GrammarItem = Trm of int | Ntrm of int

/// <summary>
/// SPPF Node that also contains forward links for left-to-right traversals.
/// Graph consisting of all Nodes (except FakeStart and FakeEnd) is a tree (DAG with Next links).
/// </summary>
/// <param name="item">
/// Terminal/nonterminal represented by this node
/// </param>
type Node (item:GrammarItem, next:Node option) =
    // terminal/nonterminal represented by this node
    member val Item = item with get
    // input buffer position of the matched input terminal
    member val ItemPos = -1 with get, set
    // nodes that are the next node for this in a left-to-right traversal
    member val NextNode = next with get, set
    // parents of this node in the SPPF
    // each parent is associated with the production number and item index in the production; see (*1*)
    member val Parents = ResizeArray<int * int * Node> () with get
    
    // add a new parent node
    member this.addParent = this.Parents.Add
    // remove the parent node
    member this.removeParent = this.Parents.Remove >> ignore
    // sets ItemPos for this node and its parents that start with the same terminal as this
    member this.setItemPos itemPos =
        if this.ItemPos = -1
        then
            this.ItemPos <- itemPos
            if this.Parents.Count = 1
            then
                let _,_,parent = this.Parents.[0]
                parent.setItemPos itemPos

    new (item, next : Node, parentLink) as this =
        Node(item, Some next) then
        this.addParent(parentLink)

/// <summary>
/// Represents a link to SPPF node with additional information
/// about left-to-right traversals to this node.
/// </sumary>
type NodeWithHistory (node:Node) =
    // SPPF node we currently point to
    member val Node = node with get, set
    // all left-to-right traversals that end with our SPPF node
    member val Traversals = ResizeArray<ResizeArray<Node>> () with get

(*1: condider the following ambiguous grammar:
       S -> AxxA
       A -> e | x
       Given the input xxx, second 'x' in the input can be either first or second 'x' in S production
*)
