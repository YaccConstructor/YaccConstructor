module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximationBuilder

open JetBrains.ReSharper.Psi.ControlFlow
open System.Collections.Generic
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.Tree

// Data dependency graph node
type DataDependencyNode = 
    | Root of List<DataDependencyNode>
    | InnerNode of List<DataDependencyNode> * List<DataDependencyNode> * string
    | Leaf of List<DataDependencyNode> * string

// Data dependency graph. Constructor takes the root node
// as a parameter
type DataDependencyGraph = 
    | DDGraph of DataDependencyNode

// Represents control flow graph node 
type ICFGNode = 
    // Builds subgraph of node's ancestors omitting nodes not 
    // satisfying predicate (subgraph remains connected)
    abstract getAncestorsSubgraph: (ICFGNode -> bool) -> DataDependencyGraph
    // Corresponding elem in PSI
    abstract psiElem: ITreeNode with get

// Represents control flow graph of source code
and ICFG = 
    // Find first node satisfying predicate
    abstract findFirst: (ICFGNode -> bool) -> option<ICFGNode>

(* impelementations for CSharp *)

// Wraps ReSharper's IControlFlowElement and ad some new functionality
type ReSharperCFGNode(node: IControlFlowElement) =
    // exception messages
    let nullContructorArgMsg = "node argument can't be null"

    let nullAncestorMsg = "null ancestor encountered"
    let wrongAncestorNodeTypeToConnect = "leaf node is passed as an ancestor in connect function"
    let wrongNodeTypeToConnect = "root node is passed as a child in connect function"
    let wrongRootTypeMsg = "maybeRoot must contain object constructed by Root"

    do if node = null then failwith nullContructorArgMsg

    interface ICFGNode with
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

            let ddLeafNode = Leaf (new List<DataDependencyNode>(), node.Id.ToString())
            let createdNodes = new Dictionary<int, DataDependencyNode>()
            let ddRootNode = traverse node ddLeafNode createdNodes None
            DDGraph ddRootNode

        member this.psiElem = node.SourceElement

// Wraps ICSharpControlFlowGraf and add some new functionality
type CSharpCFG(cfg: ICSharpControlFlowGraf) = 
    // exception messages
    let nullContructorArgMsg = "cfg argument can't be null"

    do if cfg = null then failwith nullContructorArgMsg

    interface ICFG with 
        member this.findFirst pred =
            if cfg = null then None
            else
                let rec dfs (elem: IControlFlowElement) =
                    match elem with
                    | null -> None
                    | e when pred(ReSharperCFGNode(e)) -> Some (e)
                    | e when e.Exits <> null ->
                        e.Exits
                        |> List.ofSeq
                        |> List.map (fun rib -> rib.Target) 
                        |> List.tryPick dfs
                    | _ -> None
                dfs cfg.EntryElement 
                |> Option.map (fun elem -> ReSharperCFGNode(elem) :> ICFGNode)