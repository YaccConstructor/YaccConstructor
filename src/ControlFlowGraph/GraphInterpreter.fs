module ControlFlowGraph.GraphInterpreter

open Microsoft.FSharp.Collections
open System.Collections.Generic

open ControlFlowGraph.Common
open ControlFlowGraph.InputStructures
open ControlFlowGraph.CfgElements
open ControlFlowGraph.InnerGraph

let private concatNodes (from : InterNode<_> * InterNode<_>) (_to : InterNode<_> * InterNode<_>) = 
    let entry, oldExit = from
    let oldEntry, exit = _to

    oldEntry.ReplaceMeForChildrenOn oldExit

    entry, exit

let private mergeTwoNodes (one : (InterNode<_> * InterNode<_>)) (two : (InterNode<_> * InterNode<_>)) = 
    let commonEntry, commonExit = one
    
    (fst two).ReplaceMeForChildrenOn commonEntry
    (snd two).ReplaceMeForParentsOn commonExit

    commonEntry, commonExit    

//sets common parent and common children for nodes
let private mergeNodes (nodes : (InterNode<_> * InterNode<_> list) list) = 
    let setCommonParent commonEntry = 
        nodes
        |> List.tail 
        |> List.iter
            (
                fun block -> 
                    let oldEntry = fst block
                    oldEntry.ReplaceMeForChildrenOn commonEntry
            )

    let setCommonChildren commonExit = 
        let replace (oldChild : InterNode<_>) newChild = 
            oldChild.ReplaceMeForParentsOn newChild
        
        nodes
        |> List.tail
        |> List.iter 
                (
                    fun block -> 
                        let oldExit = snd block
                        oldExit 
                        |> List.iter2 
                            (fun oldChild newChild -> replace oldChild newChild) commonExit 
                )    
    
    let commonEntry, commonExit = nodes.Head
    setCommonParent commonEntry
    setCommonChildren commonExit

    commonEntry, commonExit

let private processAssignmentGraph (graph : CfgBlocksGraph<_>) = 
    
    let start = graph.StartVertex
    let assigns = 
        graph.OutEdges(start)
        |> Seq.map
            (
                fun (edge : BlockEdge<_>)-> 
                    match edge.Tag with
                    | Simple toks -> AssignmentBlock.Create <| List.toArray toks
                    | x -> failwithf "Unexpected edge type in Assignment: %s" <| x.GetType().ToString()
            )
        //all assign blocks have only one child
        |> Seq.map (fun assign -> assign.Parent, assign.Children)
        |> List.ofSeq

    let entry, exits = mergeNodes assigns
    //all assignments has one child only
    entry, exits.Head

let private processConditionGraph (graph : CfgBlocksGraph<_>) = 
    
    let start = graph.StartVertex
    let conds = 
        graph.OutEdges(start)
        |> Seq.map
            (
                fun (edge : BlockEdge<_>)-> 
                    match edge.Tag with
                    | Simple toks -> ConditionBlock.Create <| List.toArray toks
                    | x -> failwithf "Unexpected edge type in Condition: %s" <| x.GetType().ToString()
            )
        |> Seq.map (fun cond -> cond.Parent, cond.Children)
        |> List.ofSeq
        
    mergeNodes conds

let rec processIfGraph (ifGraph : CfgBlocksGraph<_>) = 
        
    let condBlock = ref Unchecked.defaultof<_>
    let thenBlock = ref Unchecked.defaultof<_>
    let elseBlock = ref Unchecked.defaultof<_>

    let elseExists = ref false

    let ifQueue = new Queue<_>()
    ifQueue.Enqueue ifGraph.StartVertex

    while ifQueue.Count > 0 do
        let vertex = ifQueue.Dequeue()
                
        ifGraph.OutEdges(vertex)
        |> Seq.iter
            (
                fun edge ->
                    ifQueue.Enqueue edge.Target
                    match edge.Tag with
                    | Complicated (block, innerGraph) -> 
                        match block with
                        | Condition -> condBlock := processConditionGraph innerGraph
                        | ThenStatement -> thenBlock := processSeq innerGraph
                        | ElseStatement -> 
                            elseBlock := processSeq innerGraph
                            elseExists := true
                        | x -> failwithf "Unexpected statement type in ifStatement: %s" <| x.GetType().ToString()
                    | EmptyEdge -> ()
                    | x -> failwithf "Unexpected edge type in IfStatement: %s" <| x.GetType().ToString()
            )

    let exits = snd condBlock.Value
    exits.Head.ReplaceMeForParentsOn <| fst thenBlock.Value

    let mutable exitNode = Unchecked.defaultof<InterNode<_>>

    if !elseExists
    then
        let elseEntry = fst !elseBlock
        exits.[1].ReplaceMeForParentsOn elseEntry
        //exit node in 'then' block must be exactly the same as that of 'else' block
            
        let thenExit : InterNode<_> = snd !thenBlock
        let elseExit = snd !elseBlock
        elseExit.ReplaceMeForParentsOn thenExit
            
        exitNode <- thenExit
    else
        //exit node in 'then' block must be exactly the same as that of 'cond' block

        //supposed then block has only one exit node
        let thenExit = snd thenBlock.Value
        exits.[1].ReplaceMeForParentsOn thenExit
        exitNode <- thenExit

    fst condBlock.Value, exitNode

and processSeq (graph : CfgBlocksGraph<_>) = 

    let vertexToInterNode = new Dictionary<_, (InterNode<'TokenType> * InterNode<'TokenType>)>()
    let queue = new Queue<_>()

    let addToDictionary key value = 
        if vertexToInterNode.ContainsKey key 
        then
            let oldData = vertexToInterNode.[key]
            vertexToInterNode.[key] <- mergeTwoNodes oldData value
        else
            vertexToInterNode.[key] <- value

    let getOldValue target = 
        if vertexToInterNode.ContainsKey target
        then Some vertexToInterNode.[target]
        else None

    queue.Enqueue graph.StartVertex
    while queue.Count > 0 do
        let vertex = queue.Dequeue()
        graph.OutEdges(vertex)
        |> Seq.iter
            (
                fun edge -> 
                    let target = edge.Target
                    if not <| queue.Contains target
                    then queue.Enqueue target
                    
                    let oldValue = getOldValue vertex

                    match edge.Tag with
                    | Complicated (block, innerGraph) ->
                        let nodes = 
                            match block with
                            | IfStatement -> processIfGraph innerGraph
                            | Assignment -> processAssignmentGraph innerGraph
                            | x -> failwithf "Now this statement type isn't supported: %s" <| x.GetType().ToString()

                        let newData = 
                            if oldValue.IsSome
                            then concatNodes oldValue.Value nodes
                            else nodes

                        addToDictionary edge.Target newData

                    | EmptyEdge -> 
                        if oldValue.IsSome
                        then addToDictionary edge.Target oldValue.Value
                        else failwithf "Unexpected state during cfg building"

                    | x -> failwith "Unexpected edge tag: %s" <| x.GetType().ToString()
            )
    
    let endVertex = graph.VertexCount - 1
    vertexToInterNode.[endVertex]

let graphToCfg (graph : CfgBlocksGraph<'TokenType>) = 
    
    processSeq graph