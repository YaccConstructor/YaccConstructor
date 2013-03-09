// AST.fs contains a data structure to represent SPPF during parsing

module Yard.Generators.GLL.AST

type GrammarItem = Trm of int | Ntrm of int
// item : terminal/nonterminal represented by this node
[<AllowNullLiteral>]
type Node (item:GrammarItem)=
    // parents of this node in the SPPF
    // each parent is associated with the production number and item index in the production; see (*1*)
    let mutable parents : ResizeArray<int * int * Node> = ResizeArray<int * int * Node> ()

    // terminal/nonterminal represented by this node
    member val Item = item with get
    // input buffer position of the matched input terminal
    member val ItemPos = -1 with get, set
    // node that is previous for this in all left-to-right traversals
    member val PrevNode = null with get, set
    // nodes that are the next node for this in a left-to-right traversal
    member val NextNodes = ResizeArray<Node> () with get, set    
    
    // add a new parent node
    member this.addParent = parents.Add
    // add a node that is next in a left-to-right traversal    
    member this.addNext node =
        this.NextNodes.Add node
        node.PrevNode <- this
    // changes a link to next nonterminal to a set of links to its children
    member this.reassignNext oldNode newNodes =        
        this.NextNodes.Remove(oldNode) |> ignore
        Seq.iter this.addNext newNodes
        oldNode.NextNodes <- null
        oldNode.PrevNode <- null

    new (item, next, parentLink) as this =
        Node(item) then        
        this.addNext(next)
        this.addParent(parentLink)

(*1: condider the following ambiguous grammar:
       S -> AxxA
       A -> e | x       
       Given the input xxx, second 'x' in the input can be either first or second 'x' in S production
*)
