module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximationBuilder

open JetBrains.ReSharper.Psi.ControlFlow
open Utils
open System.Collections.Generic

// Data dependency graph node
type DataDependencyNode = 
    | Root of List<DataDependencyNode>
    | InnerNode of List<DataDependencyNode> * List<DataDependencyNode> * string
    | Leaf of List<DataDependencyNode> * string

// Data dependency graph, constructor takes the root node
// as a parameter
type DataDependencyGraph = 
    | DDGraph of DataDependencyNode

// Type representing control flow graph node 
type IControlFlowGraphNode = 
    // Builds subgraph of node's ancestors omitting nodes not 
    // satisfying predicate (subgraph remains connected)
    abstract getAncestorsSubgraph: (IControlFlowGraphNode -> bool) -> option<DataDependencyGraph>

// Type representing control flow graph of source code
and IControlFlowGraph = 
    // Find first node satisfying predicate
    abstract findFirst: (IControlFlowGraphNode -> bool) -> option<IControlFlowGraphNode>

//// Enum representing control flow graph node type
//type ControlFlowGraphNodeType = NotImplemented=0

type ReSharperControlFlowGraphNode(node: IControlFlowElement) =
    // exception messages
    let nullAncestorMsg = "null ancestor encountered"
    let wrongAncestorNodeTypeToConnect = "leaf node is passed as an ancestor in connect function"
    let wrongNodeTypeToConnect = "root node is passed as a child in connect function"
    let wrongRootTypeMsg = "maybeRoot must contain object constructed by Root"

    interface IControlFlowGraphNode with
        member this.getAncestorsSubgraph pred =
            let connect (node: DataDependencyNode) (ancestor: DataDependencyNode) =
                match ancestor with
                | Root(children) -> do children.Add(node)
                | InnerNode(_, children, _) -> do children.Add(node)
                | _ -> failwith wrongAncestorNodeTypeToConnect
                match node with
                | Leaf(ancestors, _) -> do ancestors.Add(ancestor)
                | InnerNode(ancestors, _, _) -> do ancestors.Add(ancestor)
                | _ -> failwith wrongNodeTypeToConnect

            let getDataDepencencyNode (node: IControlFlowElement) 
                    (createdNodes: Dictionary<int, DataDependencyNode>) =
                if createdNodes.ContainsKey node.Id
                then createdNodes.[node.Id]
                else 
                    let newNode = InnerNode(new List<DataDependencyNode>(), 
                                            new List<DataDependencyNode>(), node.Id.ToString())
                    do createdNodes.Add (node.Id, newNode)
                    newNode

            let rec traverse (cfgNode: IControlFlowElement) 
                    (ddNode: DataDependencyNode) 
                    (createdNodes: Dictionary<int, DataDependencyNode>)
                    (maybeRoot: option<DataDependencyNode>) =
                match cfgNode with
                | null -> failwith nullAncestorMsg
                | n when n.Entries = null || n.Entries.Count = 0 ->
                    match maybeRoot with
                    | None -> Root(new List<DataDependencyNode>([ddNode]))
                    | Some (Root children as root) -> 
                        do children.Add ddNode
                        root
                    | _ -> failwith wrongRootTypeMsg
                | n -> 
                    n.Entries 
                    |> List.ofSeq 
                    |> List.map (fun rib -> rib.Source)
                    |> List.map 
                        (
                            fun elem -> 
                                let ddAncestorNode = getDataDepencencyNode elem createdNodes 
                                do connect ddNode ddAncestorNode
                                elem, ddAncestorNode
                        )
                    |> List.map 
                        (
                            fun (cfgAncestor, ddAncestor) -> 
                                traverse cfgAncestor ddAncestor createdNodes maybeRoot
                        )
                    |> List.head

            if node = null
            then None
            else 
                let ddLeafNode = Leaf (new List<DataDependencyNode>(), node.Id.ToString())
                let createdNodes = new Dictionary<int, DataDependencyNode>()
                let ddRootNode = traverse node ddLeafNode createdNodes None
                Some (DDGraph ddRootNode)