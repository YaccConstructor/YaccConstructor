//module RNGLRAstToAstTest
//
//open AbstractAnalysis.Common
//open QuickGraph
//open Yard.Generators.RNGLR
//open Yard.Generators.Common.ARNGLR.AST
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
//        let input = seq [
//                             RNGLR.ParseElementary.B 0;
//                             RNGLR.ParseElementary.C 1;
//                             RNGLR.ParseElementary.D 2;
//                             RNGLR.ParseElementary.A 3;
//                        ]
//
//        let parseResult = RNGLR.ParseElementary.buildAst input
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
//        let input = seq [
//                            RNGLR.ParseElementary.A 0;
//                            RNGLR.ParseElementary.C 1;
//                        ]
//
//        let parseResult = RNGLR.ParseElementary.buildAst input
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
//        let input = seq [
//                            RNGLR.ParseAmbiguous.A 0;
//                            RNGLR.ParseAmbiguous.A 1;
//                            RNGLR.ParseAmbiguous.A 2;
//                        ]
//
//        let parseResult = RNGLR.ParseAmbiguous.buildAst input
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
//        let input = seq [
//                            RNGLR.ParseAmbiguous.B 0;
//                            RNGLR.ParseAmbiguous.RNGLR_EOF 1;
//                        ]
//
//        let parseResult = RNGLR.ParseAmbiguous.buildAst input
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
//        let input = seq [
//                            RNGLR.ParseCycles.A 0
//                        ]
//
//        let parseResult = RNGLR.ParseCycles.buildAst input
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
//        let input = seq [
//                            RNGLR.ParseSummator.LBRACE 0;
//                            RNGLR.ParseSummator.NUMBER 1;
//                            RNGLR.ParseSummator.PLUS 2;
//                            RNGLR.ParseSummator.NUMBER 3;
//                            RNGLR.ParseSummator.RBRACE 4;
//                        ]
//
//        let parseResult = RNGLR.ParseSummator.buildAst input
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
//        let input = seq [
//                            RNGLR.ParseSummator.LBRACE 0;
//                            RNGLR.ParseSummator.LBRACE 1;
//                            RNGLR.ParseSummator.NUMBER 2;
//                            RNGLR.ParseSummator.RBRACE 3;
//                            RNGLR.ParseSummator.RBRACE 4;
//                        ]
//
//        let parseResult = RNGLR.ParseSummator.buildAst input
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
//        let input = seq [
//                            RNGLR.ParseSummator.LBRACE 0;
//                            RNGLR.ParseSummator.LBRACE 1;
//                            RNGLR.ParseSummator.NUMBER 2;
//                            RNGLR.ParseSummator.RBRACE 3;
//                            RNGLR.ParseSummator.RBRACE 4;
//                        ]
//
//        let parseResult = RNGLR.ParseSummator.buildAst input
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
//        let input = seq [
//                            RNGLR.ParseSummator.LBRACE 0;
//                            RNGLR.ParseSummator.LBRACE 1;
//                            RNGLR.ParseSummator.NUMBER 2;
//                            RNGLR.ParseSummator.RBRACE 3;
//                            RNGLR.ParseSummator.RBRACE 4;
//                        ]
//
//        let result = RNGLR.ParseSummator.buildAst input
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
//        let input = seq [
//                            RNGLR.ParseSummator.LBRACE 0;
//                            RNGLR.ParseSummator.LBRACE 1;
//                            RNGLR.ParseSummator.NUMBER 2;
//                            RNGLR.ParseSummator.RBRACE 3;
//                            RNGLR.ParseSummator.RBRACE 4;
//                        ]
//
//        let result = RNGLR.ParseSummator.buildAst input
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
//    let parse input = RNGLR.ParseSummator.buildAst input
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
//        let input = seq [
//                            RNGLR.ParseSummator.LBRACE 0;
//                            RNGLR.ParseSummator.NUMBER 1;
//                            RNGLR.ParseSummator.RBRACE 2;
//                            RNGLR.ParseSummator.RBRACE 3;
//                        ]
//
//        let result = parse input
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
//        let input = seq [
//                            RNGLR.ParseSummator.LBRACE 0;
//                            RNGLR.ParseSummator.NUMBER 1;
//                            RNGLR.ParseSummator.NUMBER 2;
//                            RNGLR.ParseSummator.RBRACE 3;
//                            RNGLR.ParseSummator.RBRACE 4;
//                        ]
//
//        let result = parse input
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
//        let input = seq [
//                            RNGLR.ParseSummator.LBRACE 0;
//                            RNGLR.ParseSummator.LBRACE 1;
//                            RNGLR.ParseSummator.NUMBER 2;
//                            RNGLR.ParseSummator.RBRACE 3;
//                        ]
//
//        let result = parse input
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
//        let input = seq [
//                            RNGLR.ParseSummator.LBRACE 0;
//                            RNGLR.ParseSummator.LBRACE 1;
//                            RNGLR.ParseSummator.NUMBER 2;
//                            RNGLR.ParseSummator.NUMBER 3;
//                            RNGLR.ParseSummator.RBRACE 4;
//                        ]
//
//        let result = parse input
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
//        let input = seq [
//                            RNGLR.ParseSummator.LBRACE 0;
//                            RNGLR.ParseSummator.LBRACE 1;
//                            RNGLR.ParseSummator.LBRACE 2;
//                            RNGLR.ParseSummator.NUMBER 3;
//                            RNGLR.ParseSummator.RBRACE 4;
//                            RNGLR.ParseSummator.RBRACE 5;
//                        ]
//
//        let result = parse input
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