module RNGLRAstToAstTest

open AbstractAnalysis.Common
open Yard.Generators.ARNGLR
open Yard.Generators.RNGLR.AbstractParser
open Yard.Generators.RNGLR.OtherSPPF

open NUnit.Framework

let createEdge source target label = new ParserEdge<_>(source, target, label)

let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)

let tokenToPos (tokenData : _ -> obj) token = 
    let t = tokenData token
    match t with
    | :? int as i -> [i] |> Seq.ofList
    | _ -> failwithf "Unexpected token data: %A" <| t.GetType()

let needPrintToDot = false

[<TestFixture>]
type ``AST to otherSPPF translation test`` () =

    [<Test>]
    member test.``Elementary test``() =
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseElementary.A 0)
                createEdge 1 2 (RNGLR.ParseElementary.B 1)
                createEdge 2 3 (RNGLR.ParseElementary.C 2)
                createEdge 3 4 (RNGLR.ParseElementary.D 3)
                createEdge 4 5 (RNGLR.ParseElementary.RNGLR_EOF 4)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseElementary.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Parser.Success (mAst) ->
            
            if needPrintToDot 
            then RNGLR.ParseElementary.defaultAstToDot mAst "Elementary before.dot"
            let other = new OtherTree<_>(mAst)
            if needPrintToDot 
            then 
                other.PrintAst()
                
                let indToString = RNGLR.ParseElementary.numToString
                let tokToNumber = RNGLR.ParseElementary.tokenToNumber
                let leftSide = RNGLR.ParseElementary.leftSide

                other.ToDot indToString tokToNumber leftSide "Elementary after.dot"
            
            Assert.Pass "Elementary test: PASSED"


    [<Test>]
    member test.``Epsilon test``() =
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseElementary.A 0)
                createEdge 1 2 (RNGLR.ParseElementary.C 1)
                createEdge 2 3 (RNGLR.ParseElementary.RNGLR_EOF 2)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseElementary.buildAstAbstract qGraph
        match parseResult with 
        | Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Parser.Success (mAst) ->
            if needPrintToDot 
            then RNGLR.ParseElementary.defaultAstToDot mAst "Epsilon before.dot"
            let other = new OtherTree<_>(mAst)
            if needPrintToDot 
            then 
                other.PrintAst()

                let indToString = RNGLR.ParseElementary.numToString
                let tokToNumber = RNGLR.ParseElementary.tokenToNumber
                let leftSide = RNGLR.ParseElementary.leftSide

                other.ToDot indToString tokToNumber leftSide "Epsilon after.dot"
                
            Assert.Pass "Epsilon test: PASSED"

    [<Test>]
    member test.``Ambiguous test``() =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseAmbiguous.A 0)
                createEdge 1 2 (RNGLR.ParseAmbiguous.A 1)
                createEdge 2 3 (RNGLR.ParseAmbiguous.A 2)
                createEdge 3 4 (RNGLR.ParseAmbiguous.RNGLR_EOF 3)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseAmbiguous.buildAstAbstract qGraph
        match parseResult with 
        | Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Parser.Success (mAst) ->
            if needPrintToDot 
            then RNGLR.ParseAmbiguous.defaultAstToDot mAst "Ambiguous before.dot"
            
            let other = new OtherTree<_>(mAst)
            
            if needPrintToDot 
            then 
                let indToString = RNGLR.ParseAmbiguous.numToString
                let tokToNumber = RNGLR.ParseAmbiguous.tokenToNumber
                let leftSide = RNGLR.ParseAmbiguous.leftSide

                other.PrintAst()
                other.ToDot indToString tokToNumber leftSide "Ambiguous after.dot"
            
            Assert.Pass "Ambiguous test: PASSED"

    [<Test>]
    member test.``Parents test``() =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseAmbiguous.B 0)
                createEdge 1 2 (RNGLR.ParseAmbiguous.RNGLR_EOF 1)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseAmbiguous.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Parser.Success (mAst) ->
            let other = new OtherTree<_>(mAst)
            if needPrintToDot 
            then 
                let indToString = RNGLR.ParseAmbiguous.numToString
                let tokToNumber = RNGLR.ParseAmbiguous.tokenToNumber
                let leftSide = RNGLR.ParseAmbiguous.leftSide

                other.PrintAst()
                other.ToDot indToString tokToNumber leftSide "Parents after.dot"
                
            Assert.Pass "Parents test: PASSED"

    [<Test>]
    member test.``Cycles test``() =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseCycles.A 0)
                createEdge 1 2 (RNGLR.ParseCycles.RNGLR_EOF 1)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseCycles.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Parser.Success (mAst) ->
            if needPrintToDot 
            then RNGLR.ParseCycles.defaultAstToDot mAst "Cycles before.dot"

            let other = new OtherTree<_>(mAst)
            
            if needPrintToDot 
            then 
                let indToString = RNGLR.ParseCycles.numToString
                let tokToNumber = RNGLR.ParseCycles.tokenToNumber
                let leftSide = RNGLR.ParseCycles.leftSide
                other.PrintAst()
                other.ToDot indToString tokToNumber leftSide "Cycles after.dot"
            
            Assert.Pass "Cycles test: PASSED"

[<TestFixture>]
type ``Classic case: matching brackets``() =
    let tokToNumber = RNGLR.ParseSummator.tokenToNumber
    let leftBraceNumber  = tokToNumber <| RNGLR.ParseSummator.Token.LBRACE -1
    let rightBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.RBRACE -1
    let tokToPos = tokenToPos RNGLR.ParseSummator.tokenData
    
    let errorMessage = "Expected bracket wasn't found"

    let notBracketIsFound token = 
        let tokenName = RNGLR.ParseSummator.numToString <| tokToNumber token
        Assert.Fail <| sprintf "%s is found" tokenName
        -1

    let printToDot (otherTree : OtherTree<_>) name = 
        let indToString = RNGLR.ParseSummator.numToString
        let leftSide = RNGLR.ParseSummator.leftSide

        otherTree.ToDot indToString tokToNumber leftSide name 

    [<Test>]
    member test.``Classic case. Simple test``() =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
                createEdge 2 3 (RNGLR.ParseSummator.PLUS 2)
                createEdge 3 4 (RNGLR.ParseSummator.NUMBER 3)
                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
                createEdge 5 6 (RNGLR.ParseSummator.RNGLR_EOF 5)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Parser.Success (mAst) ->
            let other = new OtherTree<_>(mAst)
            
            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 0, true)

            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos

            let expectedPairs = 1
            Assert.AreEqual (expectedPairs, pairTokens.Count)
            
            let actual = 
                match pairTokens.[0] with
                | RNGLR.ParseSummator.RBRACE pos -> pos
                | _ -> notBracketIsFound pairTokens.[0]
            
            let expectedPos = 4
            Assert.AreEqual (expectedPos, actual, errorMessage)
            Assert.Pass "Classic case. Simple test: PASSED"

    [<Test>]
    member test.``Classic case. Many brackets 1``() =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 2)
                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 3)
                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
                createEdge 5 6 (RNGLR.ParseSummator.RNGLR_EOF 5)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Parser.Success (mAst) ->
            let other = new OtherTree<_>(mAst)

            let expectedPairs = 1

            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 0, true)
            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos

            Assert.AreEqual (expectedPairs, pairTokens.Count)
            
            let actual = 
                match pairTokens.[0] with
                | RNGLR.ParseSummator.RBRACE pos -> pos
                | _ -> notBracketIsFound pairTokens.[0]
            
            let expectedPos = 4
            Assert.AreEqual (expectedPos, actual, errorMessage)
            Assert.Pass "Classic case. Many brackets 1: PASSED"

    [<Test>]
    member test.``Classic case. Many brackets 2``() =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 2)
                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 3)
                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
                createEdge 5 6 (RNGLR.ParseSummator.RNGLR_EOF 5)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse RNGLR.ParseSummator.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Parser.Success (mAst) ->
            
            let other = new OtherTree<_>(mAst)

            let expectedPairs = 1
            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 1, true)
            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos

            Assert.AreEqual (expectedPairs, pairTokens.Count)
            
            let actual = 
                match pairTokens.[0] with
                | RNGLR.ParseSummator.RBRACE pos -> pos
                | _ -> notBracketIsFound pairTokens.[0]
            
            let expectedPos = 3
            Assert.AreEqual (expectedPos, actual, errorMessage)
            Assert.Pass "Classic case. Many brackets 2: PASSED"

    [<Test>]
    member test.``Classic case. Right to left 1``() =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 2)
                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 3)
                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
                createEdge 5 6 (RNGLR.ParseSummator.RNGLR_EOF 5)
            ] |> ignore

        let result = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
        match result with
        | Parser.Error (num, tok, message) -> printErr (num, tok, message)
        | Parser.Success(mAst) ->
            let other = new OtherTree<_>(mAst)
            
            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 3, false)
            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos
            
            let expectedPairs = 1
            Assert.AreEqual (expectedPairs, pairTokens.Count)

            let actual = 
                match pairTokens.[0] with
                | RNGLR.ParseSummator.LBRACE pos -> pos
                | _ -> notBracketIsFound pairTokens.[0]

            let expectedPos = 1
            Assert.AreEqual (expectedPos, actual, errorMessage)
            Assert.Pass <| "Classic case. Right to left: PASSED"

    [<Test>]
    member test.``Classic case. Right to left 2``() =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 2)
                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 3)
                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
                createEdge 5 6 (RNGLR.ParseSummator.RNGLR_EOF 5)
            ] |> ignore

        let result = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
        match result with
        | Parser.Error (num, tok, message) -> printErr (num, tok, message)
        | Parser.Success(mAst) ->
            let other = new OtherTree<_>(mAst)

            if needPrintToDot 
            then printToDot other "Classic case right to left 2.dot"

            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 4, false)
            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos
            
            let expectedCount = 1
            Assert.AreEqual (expectedCount, pairTokens.Count)

            let actual = 
                match pairTokens.[0] with
                | RNGLR.ParseSummator.LBRACE pos -> pos
                | _ -> notBracketIsFound pairTokens.[0]
            
            let expected = 0
            Assert.AreEqual (expected, actual, errorMessage)
            Assert.Pass "Classic case. Right to left 2: PASSED"

[<TestFixture>]
type ``Simple case: brackets are located deep``() =
    let tokToNumber = RNGLR.ParseSummator2.tokenToNumber
    let leftBraceNumber  = tokToNumber <| RNGLR.ParseSummator2.Token.LBRACE -1
    let rightBraceNumber = tokToNumber <| RNGLR.ParseSummator2.Token.RBRACE -1
    let tokToPos = tokenToPos RNGLR.ParseSummator2.tokenData
    
    let errorMessage = "Expected bracket wasn't found"

    let notBracketIsFound token = 
        let tokenName = RNGLR.ParseSummator2.numToString <| tokToNumber token
        Assert.Fail <| sprintf "%s is found" tokenName
        -1

    let printToDot (otherTree : OtherTree<_>) name = 
        let indToString = RNGLR.ParseSummator2.numToString
        let leftSide = RNGLR.ParseSummator2.leftSide

        otherTree.ToDot indToString tokToNumber leftSide name 

    [<Test>]
    member test.``Classic case. Simple test with deep brackets``() =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator2.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator2.NUMBER 1)
                createEdge 2 3 (RNGLR.ParseSummator2.PLUS 2)
                createEdge 3 4 (RNGLR.ParseSummator2.NUMBER 3)
                createEdge 4 5 (RNGLR.ParseSummator2.RBRACE 4)
                createEdge 5 6 (RNGLR.ParseSummator2.RNGLR_EOF 5)
            ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseSummator2.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Parser.Success (mAst) ->
            let other = new OtherTree<_>(mAst)
            
            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 0, true)

            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos

            let expectedPairs = 1
            Assert.AreEqual (expectedPairs, pairTokens.Count)

[<TestFixture>]
type ``Abstract case: matching brackets``() =
    let tokToNumber = RNGLR.ParseSummator.tokenToNumber
    let numToString = RNGLR.ParseSummator.numToString
    let leftBraceNumber  = tokToNumber <| RNGLR.ParseSummator.Token.LBRACE -1
    let rightBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.RBRACE -1
    let tokToPos = tokenToPos RNGLR.ParseSummator.tokenData
    let parse graph = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract graph

    let infoAboutError = "Some expected brackets weren't found"

    let notBracketIsFound token = 
        let tokenName = RNGLR.ParseSummator.numToString <| tokToNumber token
        Assert.Fail <| sprintf "%s is found" tokenName
        -1

    let astToDot ast dotName = 
        RNGLR.ParseSummator.defaultAstToDot ast dotName

    let otherToDot (otherSppf : OtherTree<_>) dotName = 
        otherSppf.ToDot numToString tokToNumber RNGLR.ParseSummator.leftSide dotName

    [<Test>]
    member test.``Abstract case. Left to right. Two parentheses 1``() =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
                createEdge 2 3 (RNGLR.ParseSummator.RBRACE 2)
                createEdge 2 3 (RNGLR.ParseSummator.RBRACE 3)
                createEdge 3 4 (RNGLR.ParseSummator.RNGLR_EOF 4)
            ] |> ignore

        let result = parse qGraph
        match result with
        | Parser.Error (num, tok, message) -> printErr (num, tok, message)
        | Parser.Success(mAst) ->
            
            let expected = [|2; 3|]
            let other = new OtherTree<_>(mAst)

            if needPrintToDot 
            then 
                astToDot mAst "ast.dot"
                otherToDot other "other.dot"
            
            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 0, true)
            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos

            Assert.AreEqual (expected.Length, pairTokens.Count)

            let actual = 
                pairTokens 
                |> Seq.map 
                    (
                        fun token -> 
                            match token with
                            | RNGLR.ParseSummator.RBRACE pos -> pos
                            | _ -> notBracketIsFound token
                    )
                |> Seq.sort
                |> Array.ofSeq

            Assert.AreEqual (expected, actual, infoAboutError)
            Assert.Pass "Abstract case. Left to right. Two parentheses 1 PASSED"

    [<Test>]
    member test.``Abstract case. Left to right. Two parentheses 2``() =
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
                createEdge 1 3 (RNGLR.ParseSummator.NUMBER 2)
                createEdge 2 4 (RNGLR.ParseSummator.RBRACE 3)
                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 4)
                createEdge 4 5 (RNGLR.ParseSummator.RNGLR_EOF 5)
            ] |> ignore

        let result = parse qGraph
        match result with
        | Parser.Error (num, tok, message) -> printErr (num, tok, message)
        | Parser.Success(mAst) ->
            let other = new OtherTree<_>(mAst)
            
            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 0, true)
            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos

            let expected = [|3; 4|]

            Assert.AreEqual (expected.Length, pairTokens.Count)
            
            let actual = 
                pairTokens 
                |> Seq.map 
                    (fun token -> 
                        match token with
                        | RNGLR.ParseSummator.RBRACE pos -> pos
                        | _ -> notBracketIsFound token
                     )
                |> Seq.sort
                |> Array.ofSeq

            Assert.AreEqual (expected, actual, infoAboutError)
            Assert.Pass "Abstract case. Left to right. Two parentheses 2 PASSED"

    [<Test>]
    member test.``Abstract case. Right to left. Two parentheses 1``() =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 1)
                createEdge 1 2 (RNGLR.ParseSummator.NUMBER 2)
                createEdge 2 3 (RNGLR.ParseSummator.RBRACE 3)
                createEdge 3 4 (RNGLR.ParseSummator.RNGLR_EOF 4)
            ] |> ignore

        let result = parse qGraph
        
        match result with
        | Parser.Error (num, tok, message) -> printErr (num, tok, message)
        | Parser.Success(mAst) ->
            
            let other = new OtherTree<_>(mAst)
            
            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 3, false)
            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos
            
            let expected = [|0; 1;|]
            Assert.AreEqual (expected.Length, pairTokens.Count)

            let actual = 
                pairTokens 
                |> Seq.map 
                    (fun token -> 
                        match token with
                        | RNGLR.ParseSummator.LBRACE pos -> pos
                        | _ -> notBracketIsFound token
                     )
                |> Seq.sort
                |> Array.ofSeq

            Assert.AreEqual (expected, actual, infoAboutError)
            Assert.Pass "Abstract case. Right to left. Two parentheses 1 PASSED"

    [<Test>]
    member test.``Abstract case. Right to left. Two parentheses 2``() =
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 0 2 (RNGLR.ParseSummator.LBRACE 1)
                createEdge 1 3 (RNGLR.ParseSummator.NUMBER 2)
                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 3)
                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 4)
                createEdge 4 5 (RNGLR.ParseSummator.RNGLR_EOF 5)
            ] |> ignore

        let result = parse qGraph
        
        match result with
        | Parser.Error (num, tok, message) -> printErr (num, tok, message)
        | Parser.Success(mAst) ->
            
            let other = new OtherTree<_>(mAst)
            
            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 4, false)
            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos
            
            let expected = [|0; 1;|]
            Assert.AreEqual (expected.Length, pairTokens.Count)

            let actual = 
                pairTokens 
                |> Seq.map 
                    (fun token -> 
                        match token with
                        | RNGLR.ParseSummator.LBRACE pos -> pos
                        | _ -> notBracketIsFound token
                     )
                |> Seq.sort
                |> Array.ofSeq

            Assert.AreEqual (expected, actual, infoAboutError)
            Assert.Pass "Abstract case. Right to left. Two parentheses 2 PASSED"

    [<Test>]
    member test.``Abstract case. Right to left. One parenthesis 1``() =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 2)
                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 3)
                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 4)
                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 5)
                createEdge 5 6 (RNGLR.ParseSummator.RNGLR_EOF 6)
            ] |> ignore

        let result = parse qGraph
        
        match result with
        | Parser.Error (num, tok, message) -> printErr (num, tok, message)
        | Parser.Success(mAst) ->
            
            let other = new OtherTree<_>(mAst)
            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 5, false)
            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos
            
            let expectedPairs = 1
            Assert.AreEqual (expectedPairs, pairTokens.Count)

            let actual = 
                match pairTokens.[0] with
                | RNGLR.ParseSummator.LBRACE pos -> pos
                | _ -> notBracketIsFound pairTokens.[0]

            let expectedPos = 0
            Assert.AreEqual (expectedPos, actual, infoAboutError)
            Assert.Pass "Abstract case. Right to left. One parenthesis 1 PASSED"

    [<Test>]
    member test.``Abstract case. Left brace cycle right brace. Find right brace``() =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
                createEdge 2 3 (RNGLR.ParseSummator.RBRACE 2)
                createEdge 2 1 (RNGLR.ParseSummator.PLUS 3)
                createEdge 3 4 (RNGLR.ParseSummator.RNGLR_EOF 4)
            ] |> ignore

        let result = parse qGraph
        
        match result with
        | Parser.Error (num, tok, message) -> printErr (num, tok, message)
        | Parser.Success(mAst) ->
            
            let other = new OtherTree<_>(mAst)
            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 0, true)
            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos
            
            let expectedPairs = 1
            Assert.AreEqual(expectedPairs, pairTokens.Count)

            let actual = 
                match pairTokens.[0] with
                | RNGLR.ParseSummator.RBRACE pos -> pos
                | _ -> notBracketIsFound pairTokens.[0]

            let expectedPos = 2
            Assert.AreEqual(expectedPos, actual, infoAboutError)
            Assert.Pass "Abstract case. Left brace cycle right brace. Find right brace PASSED"

    [<Test>]
    member test.``Abstract case. Left brace cycle right brace. Find left brace``() =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
                createEdge 2 3 (RNGLR.ParseSummator.RBRACE 2)
                createEdge 2 1 (RNGLR.ParseSummator.PLUS 3)
                createEdge 3 4 (RNGLR.ParseSummator.RNGLR_EOF 4)
            ] |> ignore

        let result = parse qGraph
        
        match result with
        | Parser.Error (num, tok, message) -> printErr(num, tok, message)
        | Parser.Success(mAst) ->
            
            let other = new OtherTree<_>(mAst)
            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 2, false)
            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos
            
            let expectedPairs = 1
            Assert.AreEqual(expectedPairs, pairTokens.Count)

            let actual = 
                match pairTokens.[0] with
                | RNGLR.ParseSummator.LBRACE pos -> pos
                | _ -> notBracketIsFound pairTokens.[0]

            let expectedPos = 0
            Assert.AreEqual(expectedPos, actual, infoAboutError)
            Assert.Pass "Abstract case. Left brace cycle right brace. Find right brace PASSED"

    [<Test>]
    member test.``Abstract case. Braces inside cycle``() =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 2)
                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 3)
                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
                createEdge 4 1 (RNGLR.ParseSummator.PLUS 5)
                createEdge 5 6 (RNGLR.ParseSummator.RNGLR_EOF 6)
            ] |> ignore

        let result = parse qGraph
        
        match result with
        | Parser.Error (num, tok, message) -> printErr(num, tok, message)
        | Parser.Success(mAst) ->
            let other = new OtherTree<_>(mAst)
            let bracketInfo = new BracketSearchInfo<_>(leftBraceNumber, rightBraceNumber, 1, true)
            let pairTokens = other.FindAllPair bracketInfo tokToNumber tokToPos
            
            let expectedPairs = 1
            Assert.AreEqual(expectedPairs, pairTokens.Count)

            let actual = 
                match pairTokens.[0] with
                | RNGLR.ParseSummator.RBRACE pos -> pos
                | _ -> notBracketIsFound pairTokens.[0]

            let expectedPos = 3
            Assert.AreEqual(expectedPos, actual, infoAboutError)
            Assert.Pass "Abstract case. Braces inside cycle PASSED"

//[<EntryPoint>]
let f x = 
    let elementary = new ``AST to otherSPPF translation test``()
//    elementary.``Parents test``()
//    elementary.``Elementary test``()
//    elementary.``Epsilon test``()
//    elementary.``Ambiguous test``()
//    elementary.``Cycles test``()

    let classic = new ``Classic case: matching brackets``()
//    classic.``Classic case. Right to left 2``()
//    classic.``Classic case. Many brackets 1``()
//    classic.``Classic case. Simple test``()

//    let brackets = new ``Abstract case: matching brackets``()
//    brackets.``More complicated test``()
//    brackets.``Many brackets 1``()
//    brackets.``Many brackets 2``()
//    brackets.``AbstractAnalysis case``()
//    brackets.``Abstract case. Two parentheses 2``()
//    brackets.``Simple right to left``()
//    brackets.``AbstractAnalysis case. Right to left``()
//    brackets.``Abstract case. Right to left``()

    let abstr = new ``Abstract case: matching brackets``()
    abstr.``Abstract case. Left to right. Two parentheses 1``()
//    abstr.``Abstract case. Two parentheses``()
//    abstr.``Abstract case. Two parentheses light``()
    1