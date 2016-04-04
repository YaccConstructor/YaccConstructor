module ControlFlowGraph.Test.BlockHelper

open Microsoft.FSharp.Collections

open System.Collections.Generic

open ControlFlowGraph
open ControlFlowGraph.CfgElements

let getTokens (block : Block<_>) = 
    block.TokensGraph.GetAvailableTokens() 
    //|> Seq.map tokenToNumber 
    |> Array.ofSeq

let commonCheck areTokensEqual (blockToChildren : IDictionary<_, _>) getBlocks (blocks : Block<_> seq) = 
    
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
            |> List.map getTokens
            |> List.forall (fun tokenSet -> condition expected tokenSet)
        res

    blocks
    |> Seq.map
        (
            fun block -> 
                let tokens = block |> getTokens
                match tryFindKey tokens with
                | Some key -> Some <| (key, getBlocks block)
                | None -> None
        )
    |> Seq.choose id
    |> Seq.forall foldFunction

let checkParent areTokensEqual (blockToChildren : IDictionary<_, _>) (blocks : Block<_> seq) = 
    let getPrevBlocks(block : Block<_>) = 
        block.Parent.Parents

    commonCheck areTokensEqual blockToChildren getPrevBlocks blocks
    
let checkChildren areTokensEqual (blockToChildren : IDictionary<_, _>) (blocks : Block<_> seq) = 
    let getNextBlocks(block : Block<_>) = 
        block.Children
        |> List.fold (fun acc child -> List.append child.Children acc) []
    
    commonCheck areTokensEqual blockToChildren getNextBlocks blocks

let checkEntryNode condition (entryNode : InterNode<_>) = 
    
    if entryNode.Parents.IsEmpty
    then 
        entryNode.Children
        |> List.map getTokens
        |> List.forall condition
    else 
        false

let checkExitNode condition (exitNode : InterNode<_>) = 
    
    if exitNode.Children.IsEmpty
    then 
        exitNode.Parents
        |> List.map getTokens
        |> List.forall condition
    else
        false

let checkExistence areEqualTokens (expected : 'TokenType list) (blocks : Block<'TokenType> list) = 

    let blocksTokens = 
        blocks
        |> List.map getTokens
        |> Seq.concat

    //one of the expected tokens must be in block
    expected
    |> List.forall(fun num -> blocksTokens |> Seq.exists (areEqualTokens num))