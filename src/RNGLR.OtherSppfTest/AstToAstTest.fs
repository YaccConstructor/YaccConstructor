//module RNGLRAstToAstTest
//
//open AbstractAnalysis.Common
//open QuickGraph
//open Yard.Generators.RNGLR
//open Yard.Generators.Common.AST
//open Yard.Generators.RNGLR.AbstractParser
//open Yard.Generators.RNGLR.OtherSPPF
//open NUnit.Framework
//
//let createEdge from _to label = new ParserEdge<_>(from, _to, label)
//
//let inline printErr (num, token : 'a, msg) =
//    printfn "Error in position %d on Token %A: %s" num token msg
//    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)
//
//let inline translate (f : TranslateArguments<_, _> -> 'b -> 'c) (ast : 'b) =
//    let args = {
//        tokenToRange = fun _ -> 0,0
//        zeroPosition = 0
//        clearAST = false
//        filterEpsilons = true
//    }
//    f args ast
//
//let tokenToPos (tokenData : _ -> obj) token = 
//    let t = tokenData token
//    match t with
//    | :? int as i -> [i] |> Seq.ofList
//    | _ -> failwith ""
//
//[<TestFixture>]
//type ``RNGLR ast to otherSPPF translation test`` () =
//
//    [<Test>]
//    member test.``Elementary test``() =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2; 3; 4;] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseElementary.A 0)
//                createEdge 1 2 (RNGLR.ParseElementary.B 1)
//                createEdge 2 3 (RNGLR.ParseElementary.C 2)
//                createEdge 3 4 (RNGLR.ParseElementary.D 3)
//             ] |> ignore
//
//        let parseResult = RNGLR.ParseElementary.buildAst qGraph
//        
//        match parseResult with 
//        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, _, _) ->
////            RNGLR.ParseElementary.defaultAstToDot mAst "Elementary before.dot"
//            let other = new OtherTree<_>(mAst)
//            RNGLR.ParseElementary.otherAstToDot other "Elementary after.dot"
//            Assert.Pass "Elementary test: PASSED"
//
//    [<Test>]
//    member test.``Epsilon test``() =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseElementary.A 0)
//                createEdge 1 2 (RNGLR.ParseElementary.C 1)
//            ] |> ignore
//
//        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseElementary.buildAstAbstract qGraph
//        
//        match parseResult with 
//        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, _, _) ->
////            RNGLR.ParseElementary.defaultAstToDot mAst "Epsilon before.dot"
//            let other = new OtherTree<_>(mAst)
//            RNGLR.ParseElementary.otherAstToDot other "Epsilon after.dot"
//            Assert.Pass "Epsilon test: PASSED"
//
//    [<Test>]
//    member test.``Ambiguous test``() =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2; 3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseAmbiguous.A 0)
//                createEdge 1 2 (RNGLR.ParseAmbiguous.A 1)
//                createEdge 2 3 (RNGLR.ParseAmbiguous.A 2)
//            ] |> ignore
//
//        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseAmbiguous.buildAstAbstract qGraph
//        
//        match parseResult with 
//        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, _, _) ->
////            RNGLR.ParseAmbiguous.defaultAstToDot mAst "Ambiguous before.dot"
//            let other = new OtherTree<_>(mAst)
//            RNGLR.ParseAmbiguous.otherAstToDot other "Ambiguous after.dot"
//            Assert.Pass "Ambiguous test: PASSED"
//
//    [<Test>]
//    member test.``Parents test``() =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseAmbiguous.B 0)
//                createEdge 1 2 (RNGLR.ParseAmbiguous.RNGLR_EOF 1)
//            ] |> ignore
//
//        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseAmbiguous.buildAstAbstract qGraph
//        
//        match parseResult with 
//        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, _, _) ->
//            let other = new OtherTree<_>(mAst)
//            RNGLR.ParseAmbiguous.otherAstToDot other "Parents after.dot"
//            Assert.Pass "Parents test: PASSED"
//
//    [<Test>]
//    member test.``Cycles test``() =
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseCycles.A 0)
//            ] |> ignore
//
//        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseCycles.buildAstAbstract qGraph
//        
//        match parseResult with 
//        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, _, _) ->
////            RNGLR.ParseCycles.defaultAstToDot mAst "Cycles before.dot"
//
//            let other = new OtherTree<_>(mAst)
//            
//            RNGLR.ParseCycles.otherAstToDot other "Cycles after.dot"
//            Assert.Pass "Cycles test: PASSED"
//
//[<TestFixture>]
//type ``Classic case: matching brackets``() =
//    let tokToNumber = RNGLR.ParseSummator.tokenToNumber
//    let leftBraceNumber  = tokToNumber <| RNGLR.ParseSummator.Token.LBRACE -1
//    let rightBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.RBRACE -1
//    let tokToPos = tokenToPos RNGLR.ParseSummator.tokenData
//    
//    let infoAboutError = "Expected bracket wasn't founded"
//
//
//    [<Test>]
//    member test.``Classic case. Simple test``() =
//        printfn "[caret](1 + 2)"
//        
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2; 3; 4; 5] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
//                createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
//                createEdge 2 3 (RNGLR.ParseSummator.PLUS 2)
//                createEdge 3 4 (RNGLR.ParseSummator.NUMBER 3)
//                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
//            ] |> ignore
//
//        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
//        
//        match parseResult with 
//        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, _, _) ->
//            let other = new OtherTree<_>(mAst)
//            
//            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 0 true tokToNumber tokToPos
//            Assert.AreEqual (1, pairTokens.Count, "Error: expected one pair but %d pairs were founded")
//
//            let expected = 4
//            let actual = 
//                match pairTokens.[0] with
//                | RNGLR.ParseSummator.RBRACE pos -> pos
//                | _ -> 
//                    let token = RNGLR.ParseSummator.numToString <| tokToNumber pairTokens.[0]
//                    Assert.Fail <| sprintf "%s was founded" token
//                    -1
//            
//            Assert.AreEqual (expected, actual, infoAboutError)
//            Assert.Pass "Classic case. Simple test: PASSED"
//
//    [<Test>]
//    member test.``Classic case. Many brackets 1``() =
//        printfn "[caret] ( ( 2 ) )"
//        
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2; 3; 4; 5] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
//                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
//                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 2)
//                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 3)
//                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
//            ] |> ignore
//
//        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
//        
//        match parseResult with 
//        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, _, _) ->
//            let other = new OtherTree<_>(mAst)
//
//            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 0 true tokToNumber tokToPos
//
//            Assert.AreEqual (1, pairTokens.Count, "Error: expected one pair but %d pairs were founded")
//            
//            let expected = 4
//            let actual = 
//                match pairTokens.[0] with
//                | RNGLR.ParseSummator.RBRACE pos -> pos
//                | _ -> 
//                    let token = RNGLR.ParseSummator.numToString <| tokToNumber pairTokens.[0]
//                    Assert.Fail <| sprintf "%s was founded" token
//                    -1
//            
//            Assert.AreEqual (expected, actual, infoAboutError)
//            Assert.Pass "Classic case. Many brackets 1: PASSED"
//
//    [<Test>]
//    member test.``Classic case. Many brackets 2``() =
//        printfn " ( [caret] ( 2 ) )"
//        
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2; 3; 4; 5] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
//                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
//                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 2)
//                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 3)
//                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
//            ] |> ignore
//
//        let parseResult = (new Parser<_>()).Parse RNGLR.ParseSummator.buildAstAbstract qGraph
//        
//        match parseResult with 
//        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
//        | Parser.Success (mAst, _, _) ->
//            
//            let other = new OtherTree<_>(mAst)
//
//            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 1 true tokToNumber tokToPos
//
//            Assert.AreEqual (1, pairTokens.Count, "Error: expected one pair but %d pairs were founded")
//            
//            let expected = 3
//            let actual = 
//                match pairTokens.[0] with
//                | RNGLR.ParseSummator.RBRACE pos -> pos
//                | _ -> 
//                    let token = RNGLR.ParseSummator.numToString <| tokToNumber pairTokens.[0]
//                    Assert.Fail <| sprintf "%s was founded" token
//                    -1
//            
//            Assert.AreEqual (expected, actual, infoAboutError)
//            Assert.Pass "Classic case. Many brackets 2: PASSED"
//
//    [<Test>]
//    member test.``Classic case. Right to left 1``() =
//        printfn " ( ( 2 ) [caret] )"
//        
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2; 3; 4;] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
//                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
//                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 2)
//                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 3)
//                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
//            ] |> ignore
//
//        let result = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
//        match result with
//        | Parser.Error (num, tok, message, debug, _) -> printErr (num, tok, message)
//        | Parser.Success(mAst, _, _) ->
//            let other = new OtherTree<_>(mAst)
//            
//            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 3 false tokToNumber tokToPos
//            
//            Assert.AreEqual (1, pairTokens.Count, "Error: expected one pair but %d pairs were founded")
//
//            let expected = 1
//            let actual = 
//                match pairTokens.[0] with
//                | RNGLR.ParseSummator.LBRACE pos -> pos
//                | _ -> 
//                    let token = RNGLR.ParseSummator.numToString <| tokToNumber pairTokens.[0]
//                    Assert.Fail <| sprintf "%s was founded" token
//                    -1
//
//            Assert.AreEqual (expected, actual, infoAboutError)
//            Assert.Pass <| "Classic case. Right to left: PASSED"
//
//    [<Test>]
//    member test.``Classic case. Right to left 2``() =
//        printfn "( ( 2 ) ) [caret]"
//        
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2; 3; 4; ] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
//                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
//                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 2)
//                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 3)
//                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 4)
//            ] |> ignore
//
//        let result = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract qGraph
//        match result with
//        | Parser.Error (num, tok, message, debug, _) -> printErr (num, tok, message)
//        | Parser.Success(mAst, _, _) ->
//            let other = new OtherTree<_>(mAst)
//
//            RNGLR.ParseSummator.otherAstToDot other "Classic case right to left 2.dot"
//
//            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 4 false tokToNumber tokToPos
//            
//            Assert.AreEqual (1, pairTokens.Count, "Error: expected one pair but %d pairs were founded")
//
//            let expected = 0
//            let actual = 
//                match pairTokens.[0] with
//                | RNGLR.ParseSummator.LBRACE pos -> pos
//                | _ -> 
//                    let token = RNGLR.ParseSummator.numToString <| tokToNumber pairTokens.[0]
//                    Assert.Fail <| sprintf "%s was founded" token
//                    -1
//            
//            Assert.AreEqual (expected, actual, infoAboutError)
//            Assert.Pass "Classic case. Right to left 2: PASSED"
//
//[<TestFixture>]
//type ``Abstract case: matching brackets``() =
//    let tokToNumber = RNGLR.ParseSummator.tokenToNumber
//    let leftBraceNumber  = tokToNumber <| RNGLR.ParseSummator.Token.LBRACE -1
//    let rightBraceNumber = tokToNumber <| RNGLR.ParseSummator.Token.RBRACE -1
//    let tokToPos = tokenToPos RNGLR.ParseSummator.tokenData
//    let parse graph = (new Parser<_>()).Parse  RNGLR.ParseSummator.buildAstAbstract graph
//
//    let infoAboutError = "Some expected brackets weren't founded"
//
//    let foundedNotBracket token = 
//        let tokenName = RNGLR.ParseSummator.numToString <| tokToNumber token
//        Assert.Fail <| sprintf "%s is founded" tokenName
//        -1
//
//    [<Test>]
//    member test.``Abstract case. Left to right. Two parentheses 1``() =
//        printfn "[caret]'(' -> '1' -> ')'"
//        printfn "                  -> ')'"
//        
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2; 3] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
//                createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
//                createEdge 2 3 (RNGLR.ParseSummator.RBRACE 2)
//                createEdge 2 3 (RNGLR.ParseSummator.RBRACE 3)
//            ] |> ignore
//
//        let result = parse qGraph
//        match result with
//        | Parser.Error (num, tok, message, debug, _) -> printErr (num, tok, message)
//        | Parser.Success(mAst, _, _) ->
//            let other = new OtherTree<_>(mAst)
//
//            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 0 true tokToNumber tokToPos
//
//            Assert.AreEqual (2, pairTokens.Count, "Error: expected two pairs but %d pairs were founded")
//
//            let actual = 
//                pairTokens 
//                |> Seq.map 
//                    (fun token -> 
//                        match token with
//                        | RNGLR.ParseSummator.RBRACE pos -> pos
//                        | _ -> foundedNotBracket token
//                    )
//                |> Seq.sort
//                |> Array.ofSeq        
//
//            let expected = [|2; 3|]
//            Assert.AreEqual (expected, actual, infoAboutError)
//            Assert.Pass "Abstract case. Left to right. Two parentheses 1 PASSED"
//
//    [<Test>]
//    member test.``Abstract case. Left to right. Two parentheses 2``() =
//        printfn "[caret] '(' -> '1' -> ')'"
//        printfn "            -> '2' -> ')'"
//        
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2; 3; 4; ] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
//                createEdge 1 2 (RNGLR.ParseSummator.NUMBER 1)
//                createEdge 1 3 (RNGLR.ParseSummator.NUMBER 2)
//                createEdge 2 4 (RNGLR.ParseSummator.RBRACE 3)
//                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 4)
//            ] |> ignore
//
//        let result = parse qGraph
//        match result with
//        | Parser.Error (num, tok, message, debug, _) -> printErr (num, tok, message)
//        | Parser.Success(mAst, _, _) ->
//            let other = new OtherTree<_>(mAst)
//            
//            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 0 true tokToNumber tokToPos
//
//            let expected = [|3; 4|]
//            Assert.AreEqual (expected.Length, pairTokens.Count, "Expected two pairs")
//            
//            let actual = 
//                pairTokens 
//                |> Seq.map 
//                    (fun token -> 
//                        match token with
//                        | RNGLR.ParseSummator.RBRACE pos -> pos
//                        | _ -> foundedNotBracket token
//                     )
//                |> Seq.sort
//                |> Array.ofSeq
//
//            Assert.AreEqual (expected, actual, infoAboutError)
//            Assert.Pass "Abstract case. Left to right. Two parentheses 2 PASSED"
//
//    [<Test>]
//    member test.``Abstract case. Right to left. Two parentheses 1``() =
//        printfn " '(' -> "
//        printfn "        '2' -> ')'[caret]"
//        printfn " '(' -> "
//        
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2; 3; ] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
//                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 1)
//                createEdge 1 2 (RNGLR.ParseSummator.NUMBER 2)
//                createEdge 2 3 (RNGLR.ParseSummator.RBRACE 3)
//            ] |> ignore
//
//        let result = parse qGraph
//        
//        match result with
//        | Parser.Error (num, tok, message, debug, _) -> printErr (num, tok, message)
//        | Parser.Success(mAst, _, _) ->
//            
//            let other = new OtherTree<_>(mAst)
//            
//            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 3 false tokToNumber tokToPos
//            
//            let expected = [|0; 1;|]
//            Assert.AreEqual (expected.Length, pairTokens.Count)
//
//            let actual = 
//                pairTokens 
//                |> Seq.map 
//                    (fun token -> 
//                        match token with
//                        | RNGLR.ParseSummator.LBRACE pos -> pos
//                        | _ -> foundedNotBracket token
//                     )
//                |> Seq.sort
//                |> Array.ofSeq
//
//            Assert.AreEqual (expected, actual, infoAboutError)
//            Assert.Pass "Abstract case. Right to left. Two parentheses 1 PASSED"
//
//    [<Test>]
//    member test.``Abstract case. Right to left. Two parentheses 2``() =
//        printfn " '(' -> '2' ->"
//        printfn "               ')'[caret]"
//        printfn " '(' -> '3' ->"
//        
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2; 3; ] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
//                createEdge 0 2 (RNGLR.ParseSummator.LBRACE 1)
//                createEdge 1 3 (RNGLR.ParseSummator.NUMBER 2)
//                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 3)
//                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 4)
//            ] |> ignore
//
//        let result = parse qGraph
//        
//        match result with
//        | Parser.Error (num, tok, message, debug, _) -> printErr (num, tok, message)
//        | Parser.Success(mAst, _, _) ->
//            
//            let other = new OtherTree<_>(mAst)
//            
//            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 4 false tokToNumber tokToPos
//            
//            let expected = [|0; 1;|]
//            Assert.AreEqual (expected.Length, pairTokens.Count)
//
//            let actual = 
//                pairTokens 
//                |> Seq.map 
//                    (fun token -> 
//                        match token with
//                        | RNGLR.ParseSummator.LBRACE pos -> pos
//                        | _ -> foundedNotBracket token
//                     )
//                |> Seq.sort
//                |> Array.ofSeq
//
//            Assert.AreEqual (expected, actual, infoAboutError)
//            Assert.Pass "Abstract case. Right to left. Two parentheses 2 PASSED"
//
//    [<Test>]
//    member test.``Abstract case. Right to left. One parenthesis 1``() =
//        printfn "        '(' -> "
//        printfn " '(' ->        '3' -> ')' -> ')'[caret]"
//        printfn "        '(' -> "
//        
//        let qGraph = new ParserInputGraph<_>()
//        qGraph.AddVertexRange[0; 1; 2; 3; ] |> ignore
//        qGraph.AddVerticesAndEdgeRange
//            [
//                createEdge 0 1 (RNGLR.ParseSummator.LBRACE 0)
//                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 1)
//                createEdge 1 2 (RNGLR.ParseSummator.LBRACE 2)
//                createEdge 2 3 (RNGLR.ParseSummator.NUMBER 3)
//                createEdge 3 4 (RNGLR.ParseSummator.RBRACE 4)
//                createEdge 4 5 (RNGLR.ParseSummator.RBRACE 5)
//            ] |> ignore
//
//        let result = parse qGraph
//        
//        match result with
//        | Parser.Error (num, tok, message, debug, _) -> printErr (num, tok, message)
//        | Parser.Success(mAst, _, _) ->
//            
//            let other = new OtherTree<_>(mAst)
//            let pairTokens = other.FindAllPair leftBraceNumber rightBraceNumber 5 false tokToNumber tokToPos
//            
//            let expected = 0
//            Assert.AreEqual (1, pairTokens.Count)
//
//            let actual = 
//                match pairTokens.[0] with
//                | RNGLR.ParseSummator.LBRACE pos -> pos
//                | _ -> foundedNotBracket pairTokens.[0]
//
//            Assert.AreEqual (expected, actual, infoAboutError)
//            Assert.Pass "Abstract case. Right to left. One parenthesis 1 PASSED"
//
//[<EntryPoint>]
//let f x = 
//    let elementary = new ``RNGLR ast to otherSPPF translation test``()
////    elementary.``Parents test``()
////    elementary.``Elementary test``()
////    elementary.``Epsilon test``()
////    elementary.``Ambiguous test``()
////    elementary.``Cycles test``()
//
//    let classic = new ``Classic case: matching brackets``()
////    classic.``Classic case. Right to left 2``()
////    classic.``Classic case. Many brackets 1``()
////    let brackets = new ``Abstract case: matching brackets``()
////    brackets.``Simple test``()
////    brackets.``More complicated test``()
////    brackets.``Many brackets 1``()
////    brackets.``Many brackets 2``()
////    brackets.``AbstractAnalysis case``()
////    brackets.``Abstract case. Two parentheses 2``()
////    brackets.``Simple right to left``()
////    brackets.``AbstractAnalysis case. Right to left``()
////    brackets.``Abstract case. Right to left``()
//    let abstr = new ``Abstract case: matching brackets``()
//    abstr.``Abstract case. Right to left. Two parentheses 2``()
////    abstr.``Abstract case. Two parentheses``()
////    abstr.``Abstract case. Two parentheses light``()
//    1