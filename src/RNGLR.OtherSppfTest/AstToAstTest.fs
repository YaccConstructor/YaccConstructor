module RNGLRAstToAstTest

open AbstractAnalysis.Common
open QuickGraph
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open Yard.Generators.RNGLR.AbstractParser
open Yard.Generators.RNGLR.OtherSPPF
open NUnit.Framework
open Yard.Generators

let dir = @"../../../../../yc/Tests/AstToAst/"

let createEdge from _to label = new ParserEdge<_>(from, _to, label)

let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)

let inline translate (f : TranslateArguments<_, _> -> 'b -> 'c) (ast : 'b) =
    let args = {
        tokenToRange = fun _ -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }
    f args ast

let tokenToPos (tokenData : _ -> obj) token = 
    let t = tokenData token
    match t with
    | :? int as i -> [i] |> Seq.ofList
    | _ -> failwith ""

[<TestFixture>]
type ``RNGLR ast to otherSPPF translation test`` () =

    [<Test>]
    member test.``Elementary test``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3;4] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [createEdge 0 1 (RNGLR.ParseElementary.A 0)
             createEdge 1 2 (RNGLR.ParseElementary.B 1)
             createEdge 2 3 (RNGLR.ParseElementary.C 2)
             createEdge 3 4 (RNGLR.ParseElementary.D 3)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseElementary.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
//            RNGLR.ParseElementary.defaultAstToDot mAst "Elementary before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseElementary.otherAstToDot other "Elementary after.dot"
            printfn "Elementary test: PASSED"
            Assert.Pass()

    [<Test>]
    member test.``Epsilon test``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [createEdge 0 1 (RNGLR.ParseElementary.A 0)
             createEdge 1 2 (RNGLR.ParseElementary.C 1)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseElementary.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
//            RNGLR.ParseElementary.defaultAstToDot mAst "Epsilon before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseElementary.otherAstToDot other "Epsilon after.dot"
            printfn "Epsilon test: PASSED"
            Assert.Pass()

    [<Test>]
    member test.``Ambiguous test``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [createEdge 0 1 (RNGLR.ParseAmbiguous.A 0)
             createEdge 1 2 (RNGLR.ParseAmbiguous.A 1)
             createEdge 2 3 (RNGLR.ParseAmbiguous.A 2)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseAmbiguous.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
//            RNGLR.ParseAmbiguous.defaultAstToDot mAst "Ambiguous before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseAmbiguous.otherAstToDot other "Ambiguous after.dot"
            printfn "Ambiguous test: PASSED"
            Assert.Pass()

    [<Test>]
    member test.``Parents test``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [createEdge 0 1 (RNGLR.ParseAmbiguous.B 0)
             createEdge 1 2 (RNGLR.ParseAmbiguous.RNGLR_EOF 1)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseAmbiguous.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
//            RNGLR.ParseAmbiguous.defaultAstToDot mAst "Parents before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseAmbiguous.otherAstToDot other "Parents after.dot"
            printfn "Parents test: PASSED"
            Assert.Pass()

    [<Test>]
    member test.``Cycles test``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [createEdge 0 1 (RNGLR.ParseCycles.A 0)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseCycles.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
//            RNGLR.ParseCycles.defaultAstToDot mAst "Cycles before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseCycles.otherAstToDot other "Cycles after.dot"
            printfn "Cycles test: PASSED"
            Assert.Pass()


[<TestFixture>]
type ``Classic case: matching brackets``() =
    [<Test>]
    member test.``Classic case. Simple test``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0; 1; 2; 3; 4; 5] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
                createEdge 2 3 (RNGLR.ParseSummator.PLUS 2)
                createEdge 3 4 (RNGLR.ParseSummator.NUMBER 3)
                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
//            RNGLR.ParseSummator.defaultAstToDot mAst "Classic case. Simple test before.dot"
            let other = new OtherTree<_>(mAst)
//            RNGLR.ParseSummator.otherAstToDot other "Classic case. Simple test after.dot"
            
            let tokToNumber = RNGLR.ParseSummator.tokenToNumber
            let leftBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.LBRACE -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.RBRACE -1
            let tokToPos = tokenToPos RNGLR.ParseSummator.tokenData

            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 0 true tokToNumber tokToPos
            
            printfn "%d pair(s) was found" pairTokens.Count
            Assert.AreEqual (1, pairTokens.Count)

            match pairTokens.[0] with
            | RNGLR.ParseSummator.Token.RBRACE pos -> Assert.AreEqual (4, pos)
            | _ -> Assert.Fail ("Expected CLOSE token")

        printfn "Classic case. Simple test: PASSED"

    [<Test>]
    member test.``Classic case. Many brackets 1``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3;4;5] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 2)
                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 3)
                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
//            RNGLR.ParseSummator.defaultAstToDot mAst "Classic case. Many brackets 1 before.dot"
            let other = new OtherTree<_>(mAst)
//            RNGLR.ParseSummator.otherAstToDot other "Classic case. Many brackets 1 after.dot"
            
            let tokToNumber = RNGLR.ParseSummator.tokenToNumber
            let leftBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.LBRACE -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.RBRACE -1
            let tokToPos = tokenToPos RNGLR.ParseSummator.tokenData

            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 0 true tokToNumber tokToPos

            printfn "%d pair(s) was found" pairTokens.Count
            Assert.AreEqual (1, pairTokens.Count)
            
            match pairTokens.[0] with
            | RNGLR.ParseSummator.Token.RBRACE pos -> Assert.AreEqual (4, pos)
            | _ -> Assert.Fail ("Expected CLOSE token")

        printfn "Classic case. Many brackets 1: PASSED"

    [<Test>]
    member test.``Classic case. Many brackets 2``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3;4;5] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 2)
                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 3)
                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse RNGLR.ParseSummator.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
//            RNGLR.ParseSummator.defaultAstToDot mAst "Classic case. Many brackets 2 before.dot"
            let other = new OtherTree<_>(mAst)
//            RNGLR.ParseSummator.otherAstToDot other "Classic case. Many brackets 2 after.dot"
            
            let tokToNumber = RNGLR.ParseSummator.tokenToNumber
            let leftBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.LBRACE -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.RBRACE -1

            let tokToPos = tokenToPos RNGLR.ParseSummator.tokenData

            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 1 true tokToNumber tokToPos

            printfn "%d pair(s) was found" pairTokens.Count
            Assert.AreEqual (1, pairTokens.Count)
            
            match pairTokens.[0] with
            | RNGLR.ParseSummator.Token.RBRACE pos -> Assert.AreEqual (3, pos)
            | _ -> Assert.Fail ("Expected CLOSE token")

        printfn "Classic case. Many brackets 2: PASSED"

    [<Test>]
    member test.``Classic case. Right to left``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
             createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
             createEdge 2 3 (RNGLR.ParseSummator.RBRACE 2)
             ] |> ignore

        let result = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
        match result with
        | Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!"
        | Parser.Success(mAst, _, _) ->
//            RNGLR.ParseSummator.defaultAstToDot mAst "Simple right to left before.dot"
            let other = new OtherTree<_>(mAst)
//            RNGLR.ParseSummator.otherAstToDot other "Simple right to left after.dot"
            
            let tokToNumber = RNGLR.ParseSummator.tokenToNumber
            let leftBraceNumber  = tokToNumber <| RNGLR.ParseSummator.Token.LBRACE -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.RBRACE -1

            let tokToPos = tokenToPos RNGLR.ParseSummator.tokenData

            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 2 false tokToNumber tokToPos
            
            printfn "%d pair(s) was found" pairTokens.Count
            Assert.AreEqual (1, pairTokens.Count)

            let expected = [|0|]
            for right in pairTokens do
                match right with
                | RNGLR.ParseSummator.LBRACE pos -> 
                    if not <| Array.exists (fun num -> pos = num) expected
                    then Assert.Fail()
                | _ -> Assert.Fail()

            printfn "Classic case. Right to left: PASSED"
            Assert.Pass()

[<TestFixture>]
type ``Abstract case: matching brackets``() =
    
    [<Test>]
    member test.``Abstract case. Two parentheses``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
             createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
             createEdge 2 3 (RNGLR.ParseSummator.RBRACE 2)
             createEdge 2 3 (RNGLR.ParseSummator.RBRACE 3)
             ] |> ignore

        let result = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
        match result with
        | Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!"
        | Parser.Success(mAst, _, _) ->
//            RNGLR.ParseSummator.defaultAstToDot mAst "Abstract case. Two parentheses before.dot"
            let other = new OtherTree<_>(mAst)
//            RNGLR.ParseSummator.otherAstToDot other "Abstract case. Two parentheses after.dot"
            
            let tokToNumber = RNGLR.ParseSummator.tokenToNumber
            let leftBraceNumber  = tokToNumber <| RNGLR.ParseSummator.Token.LBRACE -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.RBRACE -1

            let tokToPos = tokenToPos RNGLR.ParseSummator.tokenData

            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 0 true tokToNumber tokToPos

            printfn "%d pair(s) was found" pairTokens.Count
            Assert.AreEqual (2, pairTokens.Count)

            let actualValues = 
                pairTokens 
                |> Seq.map (fun tok -> 
                    match tok with
                    | RNGLR.ParseSummator.RBRACE pos -> pos
                    | _ -> -1)
                |> Seq.sort
                |> Array.ofSeq        

            let expectedValues = [|2; 3|]
            Assert.AreEqual (expectedValues, actualValues)
            printfn "Abstract case. Two parentheses PASSED"

    [<Test>]
    member test.``Abstract case. Two parentheses light``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0; 1; 4; 5; 6; 7] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
//             createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
//             createEdge 2 3 (RNGLR.ParseSummator.PLUS 2)
//             createEdge 3 4 (RNGLR.ParseSummator.NUMBER 3)
//             createEdge 3 5 (RNGLR.ParseSummator.NUMBER 4)
             createEdge 1 4 (RNGLR.ParseSummator.NUMBER 3)
             createEdge 1 5 (RNGLR.ParseSummator.NUMBER 4)
             createEdge 4 6 (RNGLR.ParseSummator.RBRACE 5)
             createEdge 5 6 (RNGLR.ParseSummator.RBRACE 6)
             createEdge 6 7 (RNGLR.ParseSummator.RNGLR_EOF 7)
             ] |> ignore

        let result = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
        match result with
        | Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!"
        | Parser.Success(mAst, _, _) ->
//            RNGLR.ParseSummator.defaultAstToDot mAst "Abstract case. Two parentheses light 2 before.dot"
            let other = new OtherTree<_>(mAst)
//            RNGLR.ParseSummator.otherAstToDot other "Abstract case. Two parentheses light 2 after.dot"
            
            let tokToNumber = RNGLR.ParseSummator.tokenToNumber
            let leftBraceNumber  = tokToNumber <| RNGLR.ParseSummator.Token.LBRACE -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.RBRACE -1

            let tokToPos = tokenToPos RNGLR.ParseSummator.tokenData

            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 0 true tokToNumber tokToPos

            printfn "%d pair(s) was found" pairTokens.Count
            Assert.AreEqual (2, pairTokens.Count, "Error: expected two pairs")
            
            let expectedValues = [|5; 6|]
            
            let actualValues = 
                pairTokens 
                |> Seq.map (fun tok -> 
                    match tok with
                    | RNGLR.ParseSummator.RBRACE pos -> pos
                    | _ -> -1)
                |> Seq.sort
                |> Array.ofSeq     

            Assert.AreEqual (expectedValues, actualValues)
            printfn "Abstract case. Two parentheses 2 light PASSED"

    [<Test>]
    member test.``Abstract case. Right to left``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
             createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
             createEdge 2 3 (RNGLR.ParseSummator.PLUS 2)
             createEdge 3 4 (RNGLR.ParseSummator.NUMBER 3)
             createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
             createEdge 3 6 (RNGLR.ParseSummator.NUMBER 5)
             createEdge 6 5 (RNGLR.ParseSummator.RBRACE 6)
             ] |> ignore

        let result = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
        
        match result with
        | Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!"
        | Parser.Success(mAst, _, _) ->
//            RNGLR.ParseSummator.defaultAstToDot mAst "Abstract case. Right to left (before).dot"
            let other = new OtherTree<_>(mAst)
//            RNGLR.ParseSummator.otherAstToDot other "Abstract case. Right to left (after).dot"
            
            let tokToNumber = RNGLR.ParseSummator.tokenToNumber
            let leftBraceNumber  = tokToNumber <| RNGLR.ParseSummator.Token.LBRACE -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.RBRACE -1

            let tokToPos = tokenToPos RNGLR.ParseSummator.tokenData

            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 6 false tokToNumber tokToPos
            
            printfn "%d pair(s) was found" pairTokens.Count
            Assert.AreEqual (1, pairTokens.Count)

            let expected = [|0|]
            for right in pairTokens do
                match right with
                | RNGLR.ParseSummator.LBRACE pos -> 
                    if not <| Array.exists (fun num -> pos = num) expected
                    then Assert.Fail()
                | _ -> Assert.Fail()

            printfn "Abstract case. Right to left: PASSED"
            Assert.Pass()

[<EntryPoint>]
let f x = 
    let elementary = new ``RNGLR ast to otherSPPF translation test``()
//    elementary.``Parents test``()
    (*test.``Elementary test``()
    test.``Epsilon test``()
    test.``Ambiguous test``()
    test.``Cycles test``()
    *)

    let classic = new ``Classic case: matching brackets``()
//    classic.``Classic case. Many brackets 1``()
//    let brackets = new ``Abstract case: matching brackets``()
    (*brackets.``Simple test``()
    brackets.``More complicated test``()*)
//    brackets.``Many brackets 1``()
    (*brackets.``Many brackets 2``()
    brackets.``AbstractAnalysis case``()*)
//    brackets.``Abstract case. Two parentheses 2``()
//    brackets.``Simple right to left``()
//    brackets.``AbstractAnalysis case. Right to left``()
//    brackets.``Abstract case. Right to left``()
    let abstr = new ``Abstract case: matching brackets``()
    abstr.``Abstract case. Two parentheses light``()
    1