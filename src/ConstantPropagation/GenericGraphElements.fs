module GenericGraphElements

open QuickGraph

open UserDefOperationInfo

type OperationType =
| Replace
| Concat
| Arbitrary of ArbitraryOperation
// and so on ...

and UpdaterType =
// initializer node ID
| Assign of int
// type and operand nodes' IDs
| PlusAssign of int * int
// function with updatable arg passing

and GraphNodeType = 
// declared name and initializer node ID
| Declaration of string * int
// update target and updater type
| Updater of string * UpdaterType
// type and operand nodes' IDs
| Operation of OperationType * list<int>
| Literal of string
| VarRef of string
| LoopNode
| LoopEnter
| LoopExit
| LoopBodyBeg
| LoopBodyEnd
| OtherNode
| ExitNode of list<GraphNode>
| StartNode

and [<CustomEquality; CustomComparison>] GraphNode = {
    Id: int
    Type: GraphNodeType }
    with
        override this.Equals other = 
            let getId = fun (n: GraphNode) -> n.Id
            let typeCheckFailedFunc () = false
            Utils.applyToMappedTypedArgs (=) getId this other typeCheckFailedFunc

        override this.GetHashCode() = 
            13 * 7 + hash this.Id

        interface System.IComparable with
            member this.CompareTo other =
                let getId = fun (n: GraphNode) -> n.Id
                let failFunc () = invalidArg "other" "cannot compare values of different types"
                Utils.applyToMappedTypedArgs compare getId this other failFunc

and BidirectGraph = BidirectionalGraph<GraphNode, Edge<GraphNode>>

type GraphWithEnds = {
    Graph: BidirectGraph
    Roots: list<GraphNode>
    Exits: list<GraphNode> }

type GraphWithSingleExit = {
    Graph: BidirectGraph
    Roots: list<GraphNode>
    Exit: GraphNode }

type GraphWithSingleRoot = {
    Graph: BidirectGraph
    Root: GraphNode
    Exits: list<GraphNode> }

type GraphWithSingleEnds = {
    Graph: BidirectGraph
    Root: GraphNode
    Exit: GraphNode }

module GraphNodeFuncs =
    let rec toString (node: GraphNode) =
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
        | Literal(value) -> sprintf "literal(%s)" value
        | VarRef(name) -> sprintf "varRef(%s)" name
        | LoopNode -> "loopNode"
        | LoopEnter -> "loopEnter"
        | LoopExit -> "loopExit"
        | LoopBodyBeg -> "loopBodyBeg"
        | LoopBodyEnd -> "loopBodyEnd"
        | OtherNode -> "otherNode"
        | ExitNode(vars) -> 
            let labels = vars |> List.map toString
            sprintf "exit(%s)" <| Seq.fold (fun acc v -> acc + "," + v) "" labels
        | StartNode -> "start"

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

    let create () = BidirectGraph(allowParallelEdges = false)
    let toDot (g: BidirectGraph) name path =
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
    let succs node (graph: BidirectGraph) =
        graph.OutEdges node |> Seq.map (fun edge -> edge.Target)
    let preds node (graph: BidirectGraph) =
        graph.InEdges node |> Seq.map (fun edge -> edge.Source)
    let addVerticesAndEdge fromNode toNode (graph: BidirectGraph) =
        graph.AddVerticesAndEdge (Edge (fromNode, toNode)) |> ignore
    let addEdgeAndVertices (graph: BidirectGraph) edge =
        graph.AddVerticesAndEdge edge |> ignore
    let addEdge (graph: BidirectGraph) edge =
        graph.AddEdge edge |> ignore
    let removeEdge (graph: BidirectGraph) edge =
        graph.RemoveEdge edge |> ignore
    let addVertex (graph: BidirectGraph) node =
        graph.AddVertex node |> ignore
    let removeVertex (graph: BidirectGraph) node =
        graph.RemoveVertex node |> ignore

module CfgTopoTraverser =
    open GraphUtils.TopoTraverser

    let isLoopNode (node: GraphNode) =
        match node.Type with LoopNode -> true | _ -> false
    let getId (node: GraphNode) = node.Id

module CfgTopoDownTraverser =
    open GraphUtils.TopoTraverser
    open CfgTopoTraverser

    let private getInputsNumber (cfg: BidirectGraph) (node: GraphNode) = 
        cfg.InDegree node
    let private getAllNextNodes (cfg: BidirectGraph) (node: GraphNode) = 
        cfg.OutEdges node
        |> List.ofSeq 
        |> List.map (fun e -> e.Target)
    let private getLoopNextNodes (cfg: BidirectGraph) (node: GraphNode) =
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
    let nextNode cfg (tState: TraverserState<GraphNode>) =
        nextNode getId (getInputsNumber cfg) (getAllNextNodes cfg) isLoopNode (getLoopNextNodes cfg) tState

module CfgTopoUpTraverser =
    open GraphUtils.TopoTraverser
    open CfgTopoTraverser

    let private getInputsNumber (cfg: BidirectGraph) (node: GraphNode) = 
        cfg.OutDegree node
    let private getAllNextNodes (cfg: BidirectGraph) (node: GraphNode) = 
        cfg.InEdges node
        |> List.ofSeq 
        |> List.map (fun e -> e.Source)
    let private getLoopNextNodes (cfg: BidirectGraph) (node: GraphNode) =
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
    let nextNode cfg (tState: TraverserState<GraphNode>) =
        nextNode getId (getInputsNumber cfg) (getAllNextNodes cfg) isLoopNode (getLoopNextNodes cfg) tState