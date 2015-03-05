module DDGraph

open QuickGraph
open System.Collections.Generic

type DDNodeType =
    | VarDecl = 0
    | AssigOp = 1
    | PlusAssignOp = 2
    | Concat = 3
    | Ref = 4
    // to be continued ...

/// Holds all info describing node. For ex., if node type
/// is Concat IDDNodeInfo contains info about arguments of 
/// the operation
type IDDNodeInfo = 
    abstract id: int
    abstract nodeType: DDNodeType

type DDGraph(root: int) =
    let graph = new AdjacencyGraph<int, Edge<int>>()
    let nodeInfoDict = new Dictionary<int, IDDNodeInfo>()

    member this.Root = root