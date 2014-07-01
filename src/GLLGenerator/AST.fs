// AST.fs contains data structures to represent SPPF during parsing

module Yard.Generators.GLL.AST

type GrammarItem = Trm of int | Ntrm of int

/// <summary>
/// SPPF Node that also contains forward links for left-to-right traversals.
/// Graph consisting of all Nodes (except FakeEnd) is a tree (DAG with Next links).
/// </summary>
type Node (item:GrammarItem, next:Node option, parent:Node option, productionNumber:int, indexInProduction:int) as this =
    // terminal/nonterminal represented by this node
    member val Item = item with get
    // node that is the next node for this in a left-to-right traversal
    member val NextNode = (match next with |Some x -> x | None -> this) with get
    // parent of this node in the SPPF
    member val Parent = (match parent with |Some x -> x | None -> this) with get
    // number of production that was used to get this node from parent
    member val ParentProductionNumber = productionNumber with get
    // index of this node in parent's production
    member val ParentProductionIndex = indexInProduction with get
    
    override x.ToString () =
        match x.Item with
        | Trm num -> "Trm " + num.ToString()
        | Ntrm num -> "Ntrm " + num.ToString()

/// <summary>
/// Represents a list that can have more than one tail.
/// </summary>
type BranchedList<'a> =
    | Empty
    | Link of 'a * BranchedList<'a> list
    | IncompleteLink of BranchedList<'a> list

    // creates a branched list that combines two specified lists 
    member x.mergeWith (y:BranchedList<'a>) =
        match x, y with
        | x, Empty -> x
        | Empty, y -> y
        | Link (_), Link (_) -> IncompleteLink [x;y]
        | IncompleteLink xTails, Link (_) -> IncompleteLink (y::xTails)
        | Link(_), IncompleteLink yTails -> IncompleteLink (x::yTails)
        | IncompleteLink xTails, IncompleteLink yTails -> IncompleteLink (List.append xTails yTails)

    override this.ToString () =
        let toString = function
            | [] -> ""
            | [tail] -> "::" + tail.ToString()
            | tails -> List.fold (fun str tail -> str + "(" + tail.ToString() + ")") "" tails
        match this with
        | Empty -> "[]"
        | Link (item, tails) -> item.ToString() + toString tails
        | IncompleteLink tails -> toString tails

/// <summary>
/// Represents a link to SPPF node with additional information
/// about left-to-right traversals to this node.
/// </sumary>
type NodeWithHistory (node:Node, traversals : BranchedList<Node>) =
    // SPPF node we currently point to
    member val Node = node with get, set
    // all left-to-right traversals that end with our SPPF node
    member val Traversals = traversals with get, set

    // TODO: better reimplement Seq.distinct with merging than this
    override this.Equals (other:obj) =
        match other with
        | :? NodeWithHistory as otherNodeH ->
            let areEqual = this.Node = otherNodeH.Node
            if areEqual then this.Traversals <- this.Traversals.mergeWith otherNodeH.Traversals
            areEqual
        | _ -> false
    override this.GetHashCode () = node.GetHashCode()

    override this.ToString () = this.Node.ToString ()

(*1: condider the following ambiguous grammar:
       S -> AxxA
       A -> e | x
       Given the input xxx, second 'x' in the input can be either first or second 'x' in S production
*)
