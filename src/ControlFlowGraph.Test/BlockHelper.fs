module ControlFlowGraph.Test.BlockHelper

open Microsoft.FSharp.Collections

open System.Collections.Generic

open ControlFlowGraph
open ControlFlowGraph.CfgElements

let getTokens tokenToNumber (block : Block<_>) = 
    block.TokensGraph.GetAvailableTokens() 
    |> Seq.map tokenToNumber 
    |> Array.ofSeq

let commonCheck (tokenToNumber : _ -> int) (blockToChildren : IDictionary<_, _>) getBlocks (blocks : Block<_> seq) = 
    
    let getTokens' = getTokens tokenToNumber
    let tryFindKey tokenSet = 
        tokenSet
        |> Array.map (fun num -> if blockToChildren.ContainsKey num then Some num else None)
        |> Array.tryPick id

    let condition expected tokenSet = 
        expected
        |> List.exists(fun num -> tokenSet |> Array.exists((=) num))
        
    let foldFunction pair = 
        let key, children = pair
        let expected = blockToChildren.[key]
        let res = 
            children
            |> List.map getTokens'
            |> List.forall (fun tokenSet -> condition expected tokenSet)
        res

    blocks
    |> Seq.map
        (
            fun block -> 
                let tokens = block |> getTokens'
                match tryFindKey tokens with
                | Some key -> Some <| (key, getBlocks block)
                | None -> None
        )
    |> Seq.choose id
    |> Seq.forall foldFunction

let checkParent (tokenToNumber : _ -> int) (blockToChildren : IDictionary<_, _>) (blocks : Block<_> seq) = 
    let getPrevBlocks(block : Block<_>) = 
        block.Parent.Parents

    commonCheck tokenToNumber blockToChildren getPrevBlocks blocks
    
let checkChildren (tokenToNumber : _ -> int) (blockToChildren : IDictionary<_, _>) (blocks : Block<_> seq) = 
    let getNextBlocks(block : Block<_>) = 
        block.Children
        |> List.fold (fun acc child -> List.append child.Children acc) []
    
    commonCheck tokenToNumber blockToChildren getNextBlocks blocks

let checkEntryNode tokenToNumber condition (entryNode : InterNode<_>) = 
    let getTokens' = getTokens tokenToNumber
    
    if entryNode.Parents.IsEmpty
    then 
        entryNode.Children
        |> List.map getTokens'
        |> List.forall condition
    else 
        false

let checkExitNode tokenToNumber condition (exitNode : InterNode<_>) = 
    let getTokens' = getTokens tokenToNumber
    
    if exitNode.Children.IsEmpty
    then 
        exitNode.Parents
        |> List.map getTokens'
        |> List.forall condition
    else
        false

let checkExistence tokenToNumber expected (blocks : Block<_> list) = 

    let blocksTokens = 
        blocks
        |> List.map (fun block -> getTokens tokenToNumber block)
        |> Seq.concat


    //one of the expected tokens must be in block
    expected
    |> List.forall(fun num -> blocksTokens |> Seq.exists ((=) num))