module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximationBuilder

// Type representing control flow graph of source code
type ControlFlowGraph = NotImplemented

// Type representing control flow graph node 
type ControlFlowGraphNode = NotImplemented

// Enum representing control flow graph node type
type ControlFlowGraphNodeType = NotImplemented=0

// Find first node satisfying predicate "pred"
let findFirst (cfgEntryElem: ControlFlowGraphNode) (pred: ControlFlowGraphNode -> bool)
    : option<ControlFlowGraphNode> = 
    None
    
// Builds subgraph of "node" node's ancestors omitting nodes not 
// satisfying "nodePred" predicate (subgraph remains connected)
let getAncestorsSubgraph (node: ControlFlowGraphNode) (nodePred: ControlFlowGraphNode -> bool)
    : option<ControlFlowGraph> = 
    None