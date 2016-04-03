module ControlFlowGraph.GraphInterpreter

open Microsoft.FSharp.Collections
open System.Collections.Generic

open ControlFlowGraph.Common
open ControlFlowGraph.CfgElements
open ControlFlowGraph.InnerGraph

/// <summary>
/// Contains entry and exit blocks of some part CFG
/// </summary>
type EntryExit<'T> = 
    val mutable Entry : InterNode<'T>
    val mutable Exit : InterNode<'T>

    new (entry, exit) = {Entry = entry; Exit = exit}

    ///Links two nodes: exit from first node is entry for second one
    static member ConcatNodes (first : EntryExit<_>) (second : EntryExit<_>) = 
        let entry, oldExit = first.Entry, first.Exit
        let oldEntry, exit = second.Entry, second.Exit
    
        oldEntry.ReplaceMeForChildrenOn oldExit
        second.Entry <- oldExit

        new EntryExit<_>(entry, exit)

    ///Sets for current nodes common entry node and exit node
    static member MergeTwoNodes (one : EntryExit<_>) (two : EntryExit<_>) = 
        let mutable commonStart, commonEnd = one.Entry, one.Exit

        if one.Entry <> two.Entry
        then 
            two.Entry.ReplaceMeForChildrenOn commonStart
            two.Entry <- commonStart
    
        if one.Exit <> two.Exit
        then 
            if two.Exit.Children.Length = 0
            then 
                two.Exit.ReplaceMeForParentsOn one.Exit
                two.Exit <- one.Exit
            elif one.Exit.Children.Length = 0
            then
                one.Exit.ReplaceMeForParentsOn two.Exit
                one.Exit <- two.Exit
            else
                failwithf "Finish nodes have children!"
                
        new EntryExit<_>(commonStart, commonEnd)

let private addExitNode (exitNode : InterNode<_>) = 
    let newExitNode = new InterNode<_>()
    exitNode.Parents
    |> List.iter 
        (
            fun parent -> 
                parent.AddChild newExitNode
                newExitNode.AddParent parent
        )
    newExitNode

//Sets common parent and common children for nodes
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
                    |> List.iter2 replace commonExit 
            )
    
    let commonEntry, commonExit = nodes.Head
    setCommonParent commonEntry
    setCommonChildren commonExit

    commonEntry, commonExit

let private processAssignmentGraph (graph : CfgBlocksGraph<_>) = 
    
    let processEdge (edge : BlockEdge<_>) = 
        match edge.Tag with
        | Simple toks -> 
            let tokens = toks |> List.toArray 
            AssignmentBlock.Create tokens
        | x -> failwithf "Unexpected edge type in Assignment: %s" <| x.GetType().ToString()

    let start = graph.FirstVertex
    let assigns = 
        let getEntryExit (block : Block<_>) = 
            //all assign blocks have only one child at this point
            new EntryExit<_>(block.Parent, block.Children.Head)

        graph.OutEdges(start)
        |> Seq.map (processEdge >> getEntryExit)
        |> List.ofSeq

    let one = assigns.Head

    assigns
    |> List.tail
    |> List.map (fun two -> EntryExit<_>.MergeTwoNodes one two)
    |> ignore
    one

let private processConditionGraph (graph : CfgBlocksGraph<_>) = 
    
    let processEdge (edge : BlockEdge<_>) = 
        match edge.Tag with
        | Simple toks -> ConditionBlock.Create <| List.toArray toks
        | x -> failwithf "Unexpected edge type in Condition: %A" x

    let start = graph.FirstVertex
    let conds = 
        let getParentAndChildren (cond : Block<_>) = cond.Parent, cond.Children
        graph.OutEdges(start)
        |> Seq.map (processEdge >> getParentAndChildren)
        |> List.ofSeq
        
    mergeNodes conds

let rec processIfGraph (ifGraph : CfgBlocksGraph<_>) = 
        
    let condBlock = ref Unchecked.defaultof<_>
    let thenBlock = ref Unchecked.defaultof<_>
    let elseBlockOpt = ref None

    let ifQueue = new Queue<_>()
    ifQueue.Enqueue ifGraph.FirstVertex

    let processEdge (edge : BlockEdge<_>)=
        ifQueue.Enqueue edge.Target
        match edge.Tag with
        | Complicated (block, innerGraph) -> 
            match block with
            | Condition -> condBlock := processConditionGraph innerGraph
            | ThenStatement -> thenBlock := processSeq innerGraph
            | ElseStatement -> elseBlockOpt := Some <| processSeq innerGraph
            | x -> failwithf "Unexpected statement type in ifStatement: %A" x
        | EmptyEdge -> ()
        | x -> failwithf "Unexpected edge type in IfStatement: %A" x

    while ifQueue.Count > 0 do
        let vertex = ifQueue.Dequeue()
                
        ifGraph.OutEdges(vertex)
        |> Seq.iter processEdge

    let exits = snd !condBlock
    exits.Head.ReplaceMeForParentsOn <| fst !thenBlock

    let mutable exitNode = Unchecked.defaultof<InterNode<_>>

    match !elseBlockOpt with
    | Some elseBlock -> 
        let elseEntry = fst elseBlock
        exits.[1].ReplaceMeForParentsOn elseEntry
        //exit node in 'then' block must be exactly the same as that of 'else' block
            
        let thenExit : InterNode<_> = snd !thenBlock
        let elseExit = snd elseBlock
        elseExit.ReplaceMeForParentsOn thenExit
            
        exitNode <- thenExit
    | None ->
        //exit node in 'then' block must be exactly the same as that of 'cond' block

        //supposed 'then' block has only one exit node
        let thenExit = snd !thenBlock
        exits.[1].ReplaceMeForParentsOn thenExit
        exitNode <- thenExit

    new EntryExit<_>(fst !condBlock, exitNode)

and processSeq (graph : CfgBlocksGraph<_>) = 

    let vertexToInterNode = new Dictionary<_, EntryExit<_>>()
    let queue = new Queue<_>()

    let addToDictionary key value = 
        if vertexToInterNode.ContainsKey key 
        then
            let oldData = vertexToInterNode.[key]
            vertexToInterNode.[key] <- EntryExit<_>.MergeTwoNodes oldData value
        else
            vertexToInterNode.[key] <- value

    let getOldValue target = 
        if vertexToInterNode.ContainsKey target
        then Some vertexToInterNode.[target]
        else None

    let updateQueue = 
        let processed = new ResizeArray<_>()
        fun vertex -> 
            if not <| processed.Contains vertex
            then 
                processed.Add vertex
                queue.Enqueue vertex

    updateQueue graph.FirstVertex
    while queue.Count > 0 do
        let vertex = queue.Dequeue()

        let processEdge (edge : BlockEdge<_>) = 
            updateQueue edge.Target
                    
            let oldValue = getOldValue vertex

            match edge.Tag with
            | Complicated (block, innerGraph) ->
                let nodes = 
                    match block with
                    | IfStatement -> processIfGraph innerGraph
                    | Assignment -> processAssignmentGraph innerGraph
                    | x -> failwithf "Now this statement type isn't supported: %s" <| x.GetType().ToString()

                let newData = 
                    match oldValue with
                    | Some oldNodes -> EntryExit<_>.ConcatNodes oldNodes nodes
                    | None -> nodes

                addToDictionary edge.Target newData

            | EmptyEdge -> 
                match oldValue with
                | Some oldNodes -> addToDictionary edge.Target oldNodes
                | None -> failwith "Unexpected state during cfg building"

            | x -> failwithf "Unexpected edge tag: %s" <| x.GetType().ToString()

        graph.OutEdges(vertex)
        |> Seq.iter processEdge
    
    let entry = vertexToInterNode.[graph.LastVertex].Entry
    let mutable exit = vertexToInterNode.[graph.LastVertex].Exit

    if exit.Children.Length > 0
    then exit <- addExitNode exit

    entry, exit

let graphToCfg (graph : CfgBlocksGraph<_>) tokenToString = 
    graph.RemoveDuplicateEpsilons()
    graph.RemoveEpsilonLoopEdges()
    //graph.RelaxedPrintToDot "``input.dot" tokenToString
    processSeq graph