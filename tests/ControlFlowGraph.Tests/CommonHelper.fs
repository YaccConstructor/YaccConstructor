module ControlFlowGraph.Test.CommonHelper

open Microsoft.FSharp.Collections
open NUnit.Framework

open AbstractAnalysis.Common
open ControlFlowGraph

open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.FST.GraphBasedFst

open Yard.Utils.StructClass

open YC.FST.AbstractLexing.Tests.CommonTestChecker


let needPrint = false

let baseInputGraphsPath = "../../../Tests/ControlFlowGraph/"

let inputName = sprintf "%s input.dot"
let astName = sprintf "%s ast.dot"
let cfgName = sprintf "%s cfg.dot"

let createFSA character = 
    let startState = ResizeArray.singleton 0
    let finishState = ResizeArray.singleton 1
    let transitions = new ResizeArray<_>()
    transitions.Add(0, Smbl(character, Unchecked.defaultof<_>), 1)
    new FSA<_>(startState, finishState, transitions)

let areEqualFSA one two fsaInfo = 
        
    let isSub fsa1 fsa2 = 
        FSA<_>.IsSubFsa (fsa1, fsa2, fsaInfo)
        
    isSub one two && isSub two one

let private dotToFST path = 
    
    let graph = loadDotToQG path

    let transform x = 
        match x with 
        | Smbl(y : char, _) when y <> (char 65535) -> x, Smbl(int <| uint32 y)
        | Smbl(y : char, _) when y =  (char 65535) -> x, Smbl 65535 
        | _ -> x, Eps
    let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)

    FST<_,int>.FSAtoFST(approximateQG(graph), transform, smblEOF)
    

let createParserInputGraph tokenize parserEOF name =
    let fullPath = baseInputGraphsPath + name
    let fstInput = dotToFST fullPath

    let lexResult = tokenize parserEOF fstInput

    match lexResult with
    | Success parserInput -> parserInput
    | Error e -> 
        Assert.Fail("Lexer error")
        failwithf "Lexer error"

let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)

let buildCfg parse createCfg astToDot tokToString (graph : ParserInputGraph<_>) prefix = 
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

        let cfg : ControlFlow<_, _> = createCfg mAst
            
        if needPrint
        then cfg.PrintToDot <| cfgName prefix
        cfg