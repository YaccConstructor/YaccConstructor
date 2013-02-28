// AST.fs contains a data structure to represent SPPF during parsing

module Yard.Generators.GLL.AST

type GrammarItem = Trm of int | Ntrm of int
// item : terminal/nonterminal represented by this node
type Node (item) =    
    // parents of this node in the SPPF
    // each parent is associated with the production number and item index in the production; see (*1*)
    let mutable parents : ResizeArray<int * int * Node> = ResizeArray<int * int * Node> ()
    // terminal nodes that are the next node for this in a left-to-right traversal
    let mutable nextTerminals : ResizeArray<Node> = ResizeArray<Node> ()
    // nonterminal nodes that are the next node for this in a left-to-right traversal
    let mutable nextNonterminals : ResizeArray<Node> = ResizeArray<Node> ()
    
    // terminal/nonterminal represented by this node
    member this.Item with get() = item
    // input buffer position of the matched input terminal
    member val ItemPos = -1 with get, set
    // nodes that are the next node for this in a left-to-right traversal
    member this.NextNodes with get() = Seq.append nextTerminals nextNonterminals |> Seq.toArray
    
    // add a new parent node
    member this.addParent = parents.Add
    // add a node that is next in a left-to-right traversal    
    member this.addNext (node:Node) =
        match node.Item with
        | Trm _ -> nextTerminals.Add(node)
        | Ntrm _ -> nextNonterminals.Add(node)
    // changes a link to next nonterminal to a set of links to its children
    member this.reassignNext oldNode newNodes =
        nextNonterminals.Remove(oldNode) |> ignore        
        Seq.iter this.addNext newNodes

    new (item, next, parentLink) as this =
        Node(item) then
        this.addNext(next)
        this.addParent(parentLink)

(*1: condider the following ambiguous grammar:
       S -> AxxA
       A -> e | x       
       Given the input xxx, second 'x' in the input can be either first or second 'x' in S production
*)
