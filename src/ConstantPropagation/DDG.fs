module DDG

open GenericGraphElements

type DDG = {
    Graph: BidirectGraph
    Root: GraphNode
    Exit: GraphNode }