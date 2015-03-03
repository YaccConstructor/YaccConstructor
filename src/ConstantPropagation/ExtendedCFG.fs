module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ControlFlowGraph

open JetBrains.ReSharper.Psi.Tree

open YC.ReSharper.AbstractAnalysis.LanguageApproximation.DataDependencyGraph

// Represents control flow graph node 
type IExtendedCFGNode = 
    // Builds subgraph of node's ancestors omitting nodes not 
    // satisfying predicate (subgraph remains connected)
    abstract getAncestorsSubgraph: (IExtendedCFGNode -> bool) -> DataDependencyGraph
    // Corresponding elem in PSI
    abstract psiElem: ITreeNode with get

// Represents control flow graph of source code
and IExtendedCFG = 
    // Find first node satisfying predicate
    abstract findFirst: (IExtendedCFGNode -> bool) -> option<IExtendedCFGNode>