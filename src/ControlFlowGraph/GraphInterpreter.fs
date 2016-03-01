module ControlFlowGraph.GraphInterpreter

open Microsoft.FSharp.Collections
open System.Collections.Generic

open ControlFlowGraph.Common
open ControlFlowGraph.CfgElements
open ControlFlowGraph.InnerGraph
open ControlFlowGraph.CfgTokensGraph
open ControlFlowGraph.Printers

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

/// <summary>
/// Contains entry and exit blocks of some part CFG
/// </summary>
type EntryExit<'T> = 
    val mutable Entry : InterNode<'T>
    val mutable Exit : InterNode<'T>

    new (entry, exit) = {Entry = entry; Exit = exit}

    ///Links two nodes: exit from the first node is entry for the second one
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
                commonEnd <- addExitNode one.Exit
                let tempEnd = addExitNode two.Exit

                tempEnd.ReplaceMeForParentsOn commonEnd
                //failwithf "Finish nodes have children!"
                
        new EntryExit<_>(commonStart, commonEnd)

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

let private processExpressionGraph (graph : CfgTokensGraph<_>) = 

    let expressions = 
        let getEntryExit (block : Block<_>) = 
            //all expression blocks have only one child at this point
            new EntryExit<_>(block.Parent, block.Children.Head)

        ExpressionBlock.Create graph
        |> getEntryExit
    expressions

let private processConditionGraph (graph : CfgBlocksGraph<_>) = 
    
    let processEdge (edge : BlockEdge<_>) = 
        match edge.Tag with
        | Simple (_, toksGraph) -> ConditionBlock.Create toksGraph
        | x -> failwithf "Unexpected edge type in Condition: %A" x

    let start = graph.FirstVertex
    let conds = 
        let getParentAndChildren (cond : Block<_>) = cond.Parent, cond.Children
        
        graph.OutEdges start
        |> Seq.map (processEdge >> getParentAndChildren)
        |> List.ofSeq
        
    mergeNodes conds

let rec processIfGraph (ifGraph : CfgBlocksGraph<_>) = 
        
    let condBlock = ref Unchecked.defaultof<_>
    let thenBlock = ref Unchecked.defaultof<_>
    let elseBlockOpt = ref None

    let processEdge (edge : BlockEdge<_>)=
        match edge.Tag with
        | Complicated (block, innerGraph) -> 
            match block with
            | Condition -> condBlock := processConditionGraph innerGraph
            | ThenStatement -> thenBlock := processSeq innerGraph
            | ElseStatement -> elseBlockOpt := Some <| processSeq innerGraph
            | x -> invalidArg "block" <| sprintf "Unexpected statement type in ifStatement: %A" x
        | EmptyEdge -> ()
        | x -> invalidArg "edge.Tag" <| sprintf "Unexpected edge type in IfStatement: %A" x

    let rec func queue = 
        match queue with
        | [] -> ()
        | head :: tail -> 
            let outEdges = ifGraph.OutEdges head 
            outEdges
            |> Seq.iter processEdge

            let outVertices = 
                outEdges 
                |> Seq.map (fun edge -> edge.Target)
                |> List.ofSeq
            func <| List.append tail outVertices

    func [ifGraph.FirstVertex]

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

and processAssignmentGraph idGraph (rightPartGraphOpt : CfgBlocksGraph<_> option) = 

    let nodes = 
        match rightPartGraphOpt with
        | Some rightPart -> processSeq rightPart
        | None -> new InterNode<_>(), new InterNode<_>()
    
    let assignBlock = 
        let getEntryExit (block : Block<_>) = 
            //all assign blocks have only one child at this point
            new EntryExit<_>(block.Parent, block.Children.Head)
        
        AssignmentBlock.Create idGraph nodes
        |> getEntryExit

    assignBlock

and processSeq (graph : CfgBlocksGraph<_>) = 

    let vertexToInterNode = new Dictionary<_, EntryExit<_>>()

    let addToDictionary key value = 
        
        vertexToInterNode.[key] <-
            match vertexToInterNode.TryGetValue key with
            | true, data -> EntryExit<_>.MergeTwoNodes data value
            | false, _ -> value

    let processEdge vertex (edge : BlockEdge<_>) = 
                    
        let oldValue = vertexToInterNode.TryGetValue vertex

        match edge.Tag with
        | Simple (block, tokensGraph) -> 
            let nodes = 
                match block with
                | Expression -> processExpressionGraph tokensGraph
                | x -> invalidOp <| sprintf "Now this statement type isn't supported: %A"x

            let newData = 
                match oldValue with
                | true,  oldNodes -> EntryExit<_>.ConcatNodes oldNodes nodes
                | false, _ -> nodes

            addToDictionary edge.Target newData

        | AssignmentEdge(block, idGraph, rightPartGraphOpt) -> 
            
            let nodes = 
                processAssignmentGraph idGraph rightPartGraphOpt

            let newData = 
                match oldValue with
                | true, oldNodes -> EntryExit<_>.ConcatNodes oldNodes nodes
                | false, _ -> nodes

            addToDictionary edge.Target newData
                
        | Complicated (block, innerGraph) ->
            let nodes = 
                match block with
                | IfStatement -> processIfGraph innerGraph
                | x -> invalidOp <| sprintf "Now this statement type isn't supported: %A"x

            let newData = 
                match oldValue with
                | true, oldNodes -> EntryExit<_>.ConcatNodes oldNodes nodes
                | false, _ -> nodes

            addToDictionary edge.Target newData

        | EmptyEdge -> 
            match oldValue with
            | true, oldNodes -> addToDictionary edge.Target oldNodes
            | false, _ -> failwith "Unexpected state during cfg building"

    let rec func processed queue = 
        match queue with
        | [] -> ()
        | head :: tail -> 
            if processed |> List.exists ((=) head)
            then
                func processed tail
            else
                let newQueue = 
                    graph.OutEdges head
                    |> Seq.map(fun edge -> edge.Target)
                    |> List.ofSeq
                    |> List.append tail

                graph.OutEdges head
                |> Seq.iter (processEdge head)

                func (head :: processed) newQueue

    func [] [graph.FirstVertex]
    
    let entry = vertexToInterNode.[graph.LastVertex].Entry
    let mutable exit = vertexToInterNode.[graph.LastVertex].Exit

    if exit.Children.Length > 0
    then exit <- addExitNode exit

    entry, exit

let graphToCfg (graph : CfgBlocksGraph<_>) tokenToString = 
    //InnerGraphPrinter.RelaxedPrintToDot graph "``input.dot" tokenToString
    processSeq graph