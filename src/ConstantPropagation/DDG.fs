module YC.ReSharper.AbstractAnalysis.LanguageApproximation.DDG

open System.Collections.Generic

// Data dependency graph node
type DataDependencyNode = 
    | Root of List<DataDependencyNode>
    | InnerNode of List<DataDependencyNode> * List<DataDependencyNode> * string
    | Leaf of List<DataDependencyNode> * string

// Data dependency graph. Constructor takes the root node
// as a parameter
type DataDependencyGraph = 
    | DDGraph of DataDependencyNode