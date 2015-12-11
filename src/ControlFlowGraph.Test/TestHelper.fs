module ControlFlowGraph.TestHelper

open System.Collections.Generic

open ControlFlowGraph.CfgElements

let getTokens tokenToNumber (block : Block<_>) = 
    block.Tokens 
    |> Array.map tokenToNumber 

let commonCheck (tokenToNumber : _ -> int) (blockToChildren : IDictionary<_, _>) getBlocks (blocks : Block<_> array) = 
    
    let getTokens' = getTokens tokenToNumber
    let tryFindKey tokenSet = 
        tokenSet
        |> Array.map (fun num -> if blockToChildren.ContainsKey num then Some num else None)
        |> Array.tryPick id

    let condition expected tokenSet = 
        expected
        |> List.fold(fun acc num -> acc || tokenSet |> Array.exists((=) num)) false
        
    let foldArray acc pair = 
        let key, children = pair
        let expected = blockToChildren.[key]
        let res = 
            children
            |> List.map getTokens'
            |> List.forall (fun tokenSet -> condition expected tokenSet)
        res && acc

    blocks
    |> Array.map
        (
            fun block -> 
                let tokens = block |> getTokens'
                match tryFindKey tokens with
                | Some key -> Some <| (key, getBlocks block)
                | None -> None
        )
    |> Array.choose id
    |> Array.fold foldArray true

let checkParent (tokenToNumber : _ -> int) (blockToChildren : IDictionary<_, _>) (blocks : Block<_> array) = 
    let getPrevBlocks(block : Block<_>) = 
        block.Parent.Parents
    
    commonCheck tokenToNumber blockToChildren getPrevBlocks blocks
    

let checkChildren (tokenToNumber : _ -> int) (blockToChildren : IDictionary<_, _>) (blocks : Block<_> array) = 
    let getNextBlocks(block : Block<_>) = 
        block.Children
        |> List.fold (fun acc child -> List.append child.Children acc) []
    
    commonCheck tokenToNumber blockToChildren getNextBlocks blocks

let checkEntryNode tokenToNumber condition (entryNode : InterNode<_>) = 
    let getTokens' = getTokens tokenToNumber
    
    if not <| entryNode.Parents.IsEmpty
    then 
        false
    else
        entryNode.Children
        |> List.map getTokens'
        |> List.forall condition

let checkExitNode tokenToNumber condition (exitNode : InterNode<_>) = 
    let getTokens' = getTokens tokenToNumber
    
    if not <| exitNode.Children.IsEmpty
    then 
        false
    else
        exitNode.Parents
        |> List.map getTokens'
        |> List.forall condition