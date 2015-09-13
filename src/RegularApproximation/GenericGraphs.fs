/// Types and functions for generic graphs representation
module GenericGraphs

open QuickGraph

open ArbitraryOperation

/// Represents the type of the string manipulating operation.
/// Functions, methods and operators in source code can all
/// be thought as operations
type OperationType<'OpInfo> =
| Replace
| Concat
| Arbitrary of ArbitraryOperation<'OpInfo>

/// Represents the type of the updater.
/// Different kinds of assignments in source code can all
/// be thought as updaters.
and UpdaterType =
// initializer node ID
| Assign of int
// type and operand nodes' IDs
| PlusAssign of int * int
// todo: functions with updatable arg passing

/// Represents the type of generic CFG and DDG node
and GraphNodeType<'Lit, 'OpInfo> = 
// declared name and initializer node ID
| Declaration of string * int
// update target and updater type
| Updater of string * UpdaterType
// type and operand nodes' IDs
| Operation of OperationType<'OpInfo> * list<int>
| Literal of string * 'Lit
| VarRef of string
| LoopNode
| LoopEnter
| LoopExit
| LoopBodyBeg
| LoopBodyEnd
| OtherNode
| ExitNode of list<GraphNode<'Lit, 'OpInfo>>
| StartNode

/// Represents generic CFG and DDG node
and [<CustomEquality; CustomComparison>] GraphNode<'Lit, 'OpInfo> = {
    Id: int
    Type: GraphNodeType<'Lit, 'OpInfo> }
    with
        override this.Equals other = 
            let getId = fun (n: GraphNode<_,_>) -> n.Id
            let typeCheckFailedFunc () = false
            Utils.applyToMappedTypedArgs (=) getId this other typeCheckFailedFunc

        override this.GetHashCode() = 
            13 * 7 + hash this.Id

        interface System.IComparable with
            member this.CompareTo other =
                let getId = fun (n: GraphNode<_,_>) -> n.Id
                let failFunc () = invalidArg "other" "cannot compare values of different types"
                Utils.applyToMappedTypedArgs compare getId this other failFunc

/// Bidirectional graph
and BidirectGraph<'Lit, 'OpInfo> = BidirectionalGraph<GraphNode<'Lit, 'OpInfo>, Edge<GraphNode<'Lit, 'OpInfo>>>

/// Bidirectional graph with multiple roots and exit nodes
type GraphWithEnds<'Lit, 'OpInfo> = {
    Graph: BidirectGraph<'Lit, 'OpInfo>
    Roots: list<GraphNode<'Lit, 'OpInfo>>
    Exits: list<GraphNode<'Lit, 'OpInfo>> }

/// Bidirectional graph with multiple roots and single exit nodes
type GraphWithSingleExit<'Lit, 'OpInfo> = {
    Graph: BidirectGraph<'Lit, 'OpInfo>
    Roots: list<GraphNode<'Lit, 'OpInfo>>
    Exit: GraphNode<'Lit, 'OpInfo> }

/// Bidirectional graph with single root and multiple exit nodes
type GraphWithSingleRoot<'Lit, 'OpInfo> = {
    Graph: BidirectGraph<'Lit, 'OpInfo>
    Root: GraphNode<'Lit, 'OpInfo>
    Exits: list<GraphNode<'Lit, 'OpInfo>> }

/// Bidirectional graph with single root and exit nodes
type GraphWithSingleEnds<'Lit, 'OpInfo> = {
    Graph: BidirectGraph<'Lit, 'OpInfo>
    Root: GraphNode<'Lit, 'OpInfo>
    Exit: GraphNode<'Lit, 'OpInfo> }

module GraphNodeFuncs =
    let rec toString (node: GraphNode<_,_>) =
        match node.Type with
        | Declaration(name, _) -> sprintf "decl(%s)" name
        | Updater(target, aType) -> 
            let typeStr =
                match aType with
                | Assign(_) -> "assign"
                | PlusAssign(_,_) -> "plusAssign"
            sprintf "%s(%s)" typeStr target
        | Operation(oType, operands) -> 
            match oType with
            | Replace -> "replace"
            | Concat -> "concat"
            | Arbitrary(_) -> sprintf "arbitrary(%d)" (List.length operands)
        | Literal(value, _) -> sprintf "literal(%s)" value
        | VarRef(name) -> sprintf "varRef(%s)" name
        | LoopNode -> "loopNode"
        | LoopEnter -> "loopEnter"
        | LoopExit -> "loopExit"
        | LoopBodyBeg -> "loopBodyBeg"
        | LoopBodyEnd -> "loopBodyEnd"
        | OtherNode -> "otherNode"
        | ExitNode(nodes) -> 
            let labels = nodes |> List.map toString
            sprintf "exit(%s)" <| Seq.fold (fun acc v -> acc + "," + v) "" labels
        | StartNode -> "start"
    
    /// Creates loop node's markers nodes. Marker nodes are 4 nodes connected directly
    /// to loop node, 2 as predecessors (LoopEnter and LoopBodyEnd) ans 2 as succeessors
    /// (LoopExit and LoopBodyBeg).
    let createLoopMarkers startId =
        let nodeId = ref (startId - 1)
        { Id = (incr nodeId; !nodeId); Type = LoopEnter },
        { Id = (incr nodeId; !nodeId); Type = LoopBodyEnd },
        { Id = (incr nodeId; !nodeId); Type = LoopExit },
        { Id = (incr nodeId; !nodeId); Type = LoopBodyBeg }, 
        !nodeId

module BidirectGraphFuns =
    open System.IO
    open GraphNodeFuncs

    /// Creates empty graph with no parallel edges allowed
    let create () = BidirectGraph(allowParallelEdges = false)
    let toDot (g: BidirectGraph<_,_>) name path =
        use file = FileInfo(path).CreateText()
        file.WriteLine("digraph " + name + " {")
        g.Vertices
        |> List.ofSeq
        |> List.map 
            (
                fun node ->
                    let text = sprintf "%d_%s" node.Id (toString node)
                    node.Id.ToString() + " [label=\"" + text + "\"]"
            )
        |> List.iter file.WriteLine
        g.Edges
        |> List.ofSeq
        |> List.map
            (
                fun edge -> edge.Source.Id.ToString() + " -> " + edge.Target.Id.ToString()
            )
        |> List.iter file.WriteLine
        file.WriteLine("}")
    let succs node (graph: BidirectGraph<_,_>) =
        graph.OutEdges node |> Seq.map (fun edge -> edge.Target)
    let preds node (graph: BidirectGraph<_,_>) =
        graph.InEdges node |> Seq.map (fun edge -> edge.Source)
    let addVerticesAndEdge fromNode toNode (graph: BidirectGraph<_,_>) =
        graph.AddVerticesAndEdge (Edge (fromNode, toNode)) |> ignore
    let addEdgeAndVertices (graph: BidirectGraph<_,_>) edge =
        graph.AddVerticesAndEdge edge |> ignore
    let addEdge (graph: BidirectGraph<_,_>) edge =
        graph.AddEdge edge |> ignore
    let removeEdge (graph: BidirectGraph<_,_>) edge =
        graph.RemoveEdge edge |> ignore
    let addVertex (graph: BidirectGraph<_,_>) node =
        graph.AddVertex node |> ignore
    let removeVertex (graph: BidirectGraph<_,_>) node =
        graph.RemoveVertex node |> ignore

/// Basic implementation of GraphUtils.TopoTraverser for BidirectGraph
module BidirectTopoTraverser =
    open GraphUtils.TopoTraverser

    let isLoopNode (node: GraphNode<_,_>) =
        match node.Type with LoopNode -> true | _ -> false
    let getId (node: GraphNode<_,_>) = node.Id

/// Implementation of GraphUtils.TopoTraverser for BidirectGraph
/// with traverses graph from predecessors to successors
module BidirectTopoDownTraverser =
    open GraphUtils.TopoTraverser
    open BidirectTopoTraverser

    let private getInputsNumber (cfg: BidirectGraph<_,_>) (node: GraphNode<_,_>) = 
        cfg.InDegree node
    let private getAllNextNodes (cfg: BidirectGraph<_,_>) (node: GraphNode<_,_>) = 
        cfg.OutEdges node
        |> List.ofSeq 
        |> List.map (fun e -> e.Target)
    let private getLoopNextNodes (cfg: BidirectGraph<_,_>) (node: GraphNode<_,_>) =
        let outNodes = cfg.OutEdges node |> Seq.map (fun e -> e.Target)
        let bodyBegNode = 
            outNodes 
            |> Seq.find 
                (fun n -> match n.Type with | LoopBodyBeg -> true | _ -> false)
        let exitNode = 
            outNodes 
            |> Seq.find 
                (fun n -> match n.Type with | LoopExit -> true | _ -> false)
        bodyBegNode, exitNode
    let nextNode cfg (tState: TraverserState<GraphNode<_,_>>) =
        nextNode getId (getInputsNumber cfg) (getAllNextNodes cfg) isLoopNode (getLoopNextNodes cfg) tState

/// Implementation of GraphUtils.TopoTraverser for BidirectGraph
/// with traverses graph from successors to predecessors
module BidirectTopoUpTraverser =
    open GraphUtils.TopoTraverser
    open BidirectTopoTraverser

    let private getInputsNumber (cfg: BidirectGraph<_,_>) (node: GraphNode<_,_>) = 
        cfg.OutDegree node
    let private getAllNextNodes (cfg: BidirectGraph<_,_>) (node: GraphNode<_,_>) = 
        cfg.InEdges node
        |> List.ofSeq 
        |> List.map (fun e -> e.Source)
    let private getLoopNextNodes (cfg: BidirectGraph<_,_>) (node: GraphNode<_,_>) =
        let inNodes = cfg.InEdges node |> Seq.map (fun e -> e.Source)
        let bodyEndNode = 
            inNodes 
            |> Seq.find 
                (fun n -> match n.Type with | LoopBodyEnd -> true | _ -> false)
        let enterNode = 
            inNodes 
            |> Seq.find 
                (fun n -> match n.Type with | LoopEnter -> true | _ -> false)
        bodyEndNode, enterNode
    let nextNode cfg (tState: TraverserState<GraphNode<_,_>>) =
        nextNode getId (getInputsNumber cfg) (getAllNextNodes cfg) isLoopNode (getLoopNextNodes cfg) tState