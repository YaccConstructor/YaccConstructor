module ControlFlowGraph.TestHelper

open Microsoft.FSharp.Collections
open NUnit.Framework
open System
open System.Collections.Generic

open AbstractAnalysis.Common
open ControlFlowGraph.CfgElements
open QuickGraph
open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.FSA.FsaApproximation
open QuickGraph.FST.GraphBasedFst
open YC.Tests.Helper


let baseInputGraphsPath = "../../../Tests/ControlFlowGraph/"
let needPrint = true
let inputName = sprintf "%s input.dot"
let astName = sprintf "%s ast.dot"
let cfgName = sprintf "%s cfg.dot"

let private quickGraphToFST (lexerInputGraph : AdjacencyGraph<int, TaggedEdge<_,string>>)= 
    let initialStates = ResizeArray.singleton 0
    let finishStates = ResizeArray.singleton <| lexerInputGraph.VertexCount - 1

    let transform x = 
        match x with 
        | Smbl(y : char, _) when y <> (char 65535) -> x, Smbl(int <| Convert.ToUInt32(y)) 
        | Smbl(y : char, _) when y =  (char 65535) -> x, Smbl 65535 
        | _ -> x, Eps
    let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)

    let transitions = new ResizeArray<_>()
    lexerInputGraph.Edges
    |> Seq.iter(fun edge -> transitions.Add(edge.Source, (edge.Tag, edge.Tag), edge.Target))

    let appr = new Appr<_>(initialStates, finishStates, transitions)
    let nfaFsa = appr.ApprToFSA()
    let fsa = nfaFsa.NfaToDfa()
    FST<_, int>.FSAtoFST(fsa, transform, smblEOF)

let createParserInputGraph tokenize parserEOF name =
    let fstInput = quickGraphToFST <| loadDotToQG baseInputGraphsPath name

    let lexResult = tokenize parserEOF fstInput

    match lexResult with
    | Success parserInput -> parserInput
    | Error e -> 
        Assert.Fail("Lexer error")
        failwithf "Lexer error"



let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let thr3 (_, _, c) = c

let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)

let buildCfg (graph : ParserInputGraph<_>) parse createCfg astToDot tokToString prefix = 
    if needPrint 
    then graph.PrintToDot <| inputName prefix <| tokToString
        
    let parseResult = parse graph
        
    match parseResult with 
    | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> 
        printErr(num, tok, err)
        failwithf "Syntax error"
    | Yard.Generators.ARNGLR.Parser.Success (mAst) ->
        if needPrint
        then astToDot mAst <| astName prefix

        let cfg : ControlFlow<_> = createCfg mAst
            
        if needPrint
        then cfg.PrintToDot <| cfgName prefix
        cfg

let getTokens tokenToNumber (block : Block<_>) = 
    block.TokensGraph.GetAvailableTokens() 
    |> Seq.map tokenToNumber 
    |> Array.ofSeq

let commonCheck (tokenToNumber : _ -> int) (blockToChildren : IDictionary<_, _>) getBlocks (blocks : Block<_> array) = 
    
    let getTokens' = getTokens tokenToNumber
    let tryFindKey tokenSet = 
        tokenSet
        |> Array.map (fun num -> if blockToChildren.ContainsKey num then Some num else None)
        |> Array.tryPick id

    let condition expected tokenSet = 
        expected
        |> List.exists(fun num -> tokenSet |> Array.exists((=) num))
        
    let foldArray pair = 
        let key, children = pair
        let expected = blockToChildren.[key]
        let res = 
            children
            |> List.map getTokens'
            |> List.forall (fun tokenSet -> condition expected tokenSet)
        res

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
    |> Array.forall foldArray

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