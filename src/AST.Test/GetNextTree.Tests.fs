module ASTExtraction.Test

open AbstractAnalysis.Common

open Yard.Generators.Common.AstNode
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.Parser
open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.AbstractParser

open System.Collections.Generic

open NUnit.Framework
open QuickGraph

let needPrint = false
let threshold = 10
let createEdge source target label = new ParserEdge<_>(source, target, label)

let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)

let inline translate (f : TranslateArguments<_,_> -> 'b -> 'c) (ast : 'b) =
    let args = {
        tokenToRange = fun _ -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }
    f args ast

let filter = fun _ -> true

[<TestFixture>]
type ``AST GetNextTree tests``() =
    let runTest graph parse toDot translator errDict expected testName = 
        let parseResult = parse graph
        
        match parseResult with 
        | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success (ast) ->
            if needPrint then toDot ast <| sprintf "%s.dot" testName
            let mutable callsCount = 0

            let mutable unprocessed = List.init ast.TokensCount (fun i -> new Terminal(i))
            let mutable extracted = []

            while not <| unprocessed.IsEmpty do
                let tree, unproc = ast.GetNextTree unprocessed (fun _ -> true)
                if needPrint 
                then toDot tree <| sprintf "%s_tree_%d.dot" testName callsCount
                callsCount <- callsCount + 1
                unprocessed <- unproc
                extracted <- tree :: extracted

                if callsCount > expected * threshold
                then Assert.Fail(sprintf "Extracted trees count is %d times higher than expected. Cycle?" threshold)
            
            Assert.AreEqual(expected, extracted.Length)

            //checks if extracted trees are valid
            extracted
            |> List.map (fun ast -> translate translator ast errDict)
            |> List.iter (fun i -> if needPrint then printf "%A " i)

    [<Test>]
    member this.``Cycles A + (A + ... + A)``()= 
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseCalc.A 0)
                createEdge 1 0 (RNGLR.ParseCalc.L_plus_ 1)
                createEdge 1 2 (RNGLR.ParseCalc.RNGLR_EOF 2)
            ] |> ignore

        let parse = RNGLR.ParseCalc.buildAstAbstract
        let toDot = RNGLR.ParseCalc.defaultAstToDot
        let translator = RNGLR.ParseCalc.translate
        let errDict = new Dictionary<_,_>()
        runTest qGraph parse toDot translator errDict 1 "Cycles A + (A + ... + A)"

    [<Test>]
    member this.``Cycles A + B * A + B * A + B * ... + B``()= 
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseCalc.A 0)
                createEdge 1 2 (RNGLR.ParseCalc.L_plus_ 1)
                createEdge 2 3 (RNGLR.ParseCalc.B 2)
                createEdge 3 0 (RNGLR.ParseCalc.L_star_ 3)
                createEdge 3 4 (RNGLR.ParseCalc.RNGLR_EOF 4)
            ] |> ignore

        let parse = RNGLR.ParseCalc.buildAstAbstract
        let toDot = RNGLR.ParseCalc.defaultAstToDot
        let translator = RNGLR.ParseCalc.translate
        let errDict = new Dictionary<_,_>()
        runTest qGraph parse toDot translator errDict 1 "Cycles A plus B mul A plus B mul ... plus B"

    [<Test>]
    member this.``Branches``() = 
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseCalc.A 0)
                createEdge 0 1 (RNGLR.ParseCalc.A 1)
                createEdge 1 2 (RNGLR.ParseCalc.L_plus_ 2)
                createEdge 2 3 (RNGLR.ParseCalc.B 3)
                createEdge 2 3 (RNGLR.ParseCalc.C 4)
                createEdge 2 3 (RNGLR.ParseCalc.D 5)
                createEdge 3 4 (RNGLR.ParseCalc.RNGLR_EOF 6)
            ] |> ignore

        let parse = RNGLR.ParseCalc.buildAstAbstract
        let toDot = RNGLR.ParseCalc.defaultAstToDot
        let translator = RNGLR.ParseCalc.translate
        let errDict = new Dictionary<_,_>()
        runTest qGraph parse toDot translator errDict 3 "``Branches``"

    [<Test>]
    member this.``Simple Cycle``() = 
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseCycle.A 0)
                createEdge 1 1 (RNGLR.ParseCycle.B 1)
                createEdge 1 2 (RNGLR.ParseCycle.RNGLR_EOF 2)
            ] |> ignore

        let parse = RNGLR.ParseCycle.buildAstAbstract
        let toDot = RNGLR.ParseCycle.defaultAstToDot
        let translator = RNGLR.ParseCycle.translate
        let errDict = new Dictionary<_,_>()
        runTest qGraph parse toDot translator errDict 1 "``Simple Cycle``"

//[<EntryPoint>]
let f x = 
    let tester = new ``AST GetNextTree tests``()
    tester.``Cycles A + (A + ... + A)``()
    0