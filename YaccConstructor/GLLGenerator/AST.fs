module Yard.Generators.GLL.AST

type GrammarItem = Trm of int | Ntrm of int
// item : terminal/nonterminal represented by this node
type Node (item) =    
    // input buffer position of the matched input terminal
    let mutable _itemPos : int = -1
    // parents of this node in the SPPF
    // each parent is associated with the production number and item index in the production; see (*1*)
    let mutable _parents : (int * int * Node) list = []
    // terminal nodes that are the next node for this in a left-to-right traversal
    let mutable _nextTerminals : Node list = []
    // nonterminal nodes that are the next node for this in a left-to-right traversal
    let mutable _nextNonterminals : Node list = []

    // terminal/nonterminal represented by this node
    member this.Item with get() = item
    // matches this node with an input terminal (identified by its input buffer position)
    member this.setItemPos pos = _itemPos <- pos
    // add a new parent node
    member this.addParent parentLink = _parents <- parentLink :: _parents
    // add a node that is next in a left-to-right traversal    
    member this.addNext (node:Node) =
        match node.Item with
        | Trm _ -> _nextTerminals <- node :: _nextTerminals
        | Ntrm _ -> _nextNonterminals <- node :: _nextNonterminals
    // changes a link to next nonterminal to a set of links to its children
    member this.reassignNext oldNode newNodes =
        _nextNonterminals <- List.filter (fun x -> x <> oldNode) _nextNonterminals
        Seq.iter this.addNext newNodes        

(*1: condider the following ambiguous grammar:
       S -> AxxA
       A -> e | x       
       Given the input xxx, second 'x' in the input can be either first or second 'x' in S production
*)
