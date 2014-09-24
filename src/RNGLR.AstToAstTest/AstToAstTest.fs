module RNGLRAstToAstTest

open AbstractAnalysis.Common
open QuickGraph
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open Yard.Generators.RNGLR.AbstractParser
open Yard.Generators.RNGLR.OtherSPPF
open NUnit.Framework
open Yard.Generators
open LexCommon

let run path astBuilder =
    let tokens = LexCommon.tokens(path) 
    astBuilder tokens

let dir = @"../../../../../yc/Tests/AstToAst/"

let lbl tokenId = tokenId
let edg from _to label = new ParserEdge<_>(from, _to, lbl label)

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

[<TestFixture>]
type ``RNGLR ast to ast translation test`` () =

    [<Test>]
    member test.``Elementary test``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3;4] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.ParseElementary.A 0)
             edg 1 2 (RNGLR.ParseElementary.B 1)
             edg 2 3 (RNGLR.ParseElementary.C 2)
             edg 3 4 (RNGLR.ParseElementary.D 3)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseElementary.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseElementary.defaultAstToDot mAst "Elementary before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseElementary.otherAstToDot other "Elementary after.dot"
            Assert.Pass()

    [<Test>]
    member test.``Epsilon test``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.ParseElementary.A 0)
             edg 1 2 (RNGLR.ParseElementary.C 1)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseElementary.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseElementary.defaultAstToDot mAst "Epsilon before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseElementary.otherAstToDot other "Epsilon after.dot"
            Assert.Pass()

    [<Test>]
    member test.``Ambiguous test``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.ParseAmbiguous.A 0)
             edg 1 2 (RNGLR.ParseAmbiguous.A 1)
             edg 2 3 (RNGLR.ParseAmbiguous.A 2)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseAmbiguous.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseAmbiguous.defaultAstToDot mAst "Ambiguous before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseAmbiguous.otherAstToDot other "Ambiguous after.dot"
            Assert.Pass()

    [<Test>]
    member test.``Cycles test``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.ParseCycles.A 0)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseCycles.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseCycles.defaultAstToDot mAst "Cycles before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseCycles.otherAstToDot other "Cycles after.dot"
            Assert.Pass()


[<TestFixture>]
type ``Brackets matching`` () =
    
    let tokenToPos (tokenData : _ -> obj) token = 
        let t = tokenData token
        match t with
        | :? int as i -> [i] |> Seq.ofList
        | _ -> failwith ""

    [<Test>]
    member test.``Simple test``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3;4;5;6] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                edg 0 1 (RNGLR.ParseBrackets_1.OPEN 0)
                edg 1 2 (RNGLR.ParseBrackets_1.A 1)
                edg 2 3 (RNGLR.ParseBrackets_1.B 2)
                edg 3 4 (RNGLR.ParseBrackets_1.C 3)
                edg 4 5 (RNGLR.ParseBrackets_1.D 4)
                edg 5 6 (RNGLR.ParseBrackets_1.CLOSE 5)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseBrackets_1.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseBrackets_1.defaultAstToDot mAst "Bracket before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseBrackets_1.otherAstToDot other "Bracket after.dot"
            
            let tokToNumber = RNGLR.ParseBrackets_1.tokenToNumber
            let leftBraceNumber = tokToNumber <| RNGLR.ParseBrackets_1.Token.OPEN -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseBrackets_1.Token.CLOSE -1
            let tokToPos = tokenToPos RNGLR.ParseBrackets_1.tokenData

            let pairs = other.FindAllPair leftBraceNumber rightBraceNumber 0 true tokToNumber tokToPos
            Assert.AreEqual (1, pairs.Count)

            match pairs.[0] with
            | RNGLR.ParseBrackets_1.Token.CLOSE pos -> Assert.AreEqual (5, pos)
            | _ -> Assert.Fail ("Expected CLOSE token")

    [<Test>]
    member test.``More complicated test``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3;4;5;6] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                edg 0 1 (RNGLR.ParseBrackets_2.OPEN 0)
                edg 1 2 (RNGLR.ParseBrackets_2.A 1)
                edg 2 3 (RNGLR.ParseBrackets_2.B 2)
                edg 3 4 (RNGLR.ParseBrackets_2.C 3)
                edg 4 5 (RNGLR.ParseBrackets_2.D 4)
                edg 5 6 (RNGLR.ParseBrackets_2.CLOSE 5)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseBrackets_2.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseBrackets_2.defaultAstToDot mAst "Complicated Bracket before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseBrackets_2.otherAstToDot other "Complicated Bracket after.dot"

            let tokToNumber = RNGLR.ParseBrackets_2.tokenToNumber
            let leftBraceNumber = tokToNumber <| RNGLR.ParseBrackets_2.Token.OPEN -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseBrackets_2.Token.CLOSE -1
            let tokToPos = tokenToPos RNGLR.ParseBrackets_2.tokenData

            let pairs = other.FindAllPair leftBraceNumber rightBraceNumber 0 true tokToNumber tokToPos
            Assert.AreEqual (1, pairs.Count)
            
            match pairs.[0] with
            | RNGLR.ParseBrackets_2.Token.CLOSE pos -> Assert.AreEqual (5, pos)
            | _ -> Assert.Fail ("Expected CLOSE token")

    [<Test>]
    member test.``Many brackets 1``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3;4;5] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                edg 0 1 (RNGLR.ParseBrackets_3.OPEN 0)
                edg 1 2 (RNGLR.ParseBrackets_3.OPEN 1)
                edg 2 3 (RNGLR.ParseBrackets_3.A 2)
                edg 3 4 (RNGLR.ParseBrackets_3.CLOSE 3)
                edg 4 5 (RNGLR.ParseBrackets_3.CLOSE 4)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseBrackets_3.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseBrackets_3.defaultAstToDot mAst "Many brackets before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseBrackets_3.otherAstToDot other "Many brackets after.dot"
            
            let tokToNumber = RNGLR.ParseBrackets_3.tokenToNumber
            let leftBraceNumber = tokToNumber <| RNGLR.ParseBrackets_3.Token.OPEN -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseBrackets_3.Token.CLOSE -1
            let tokToPos = tokenToPos RNGLR.ParseBrackets_3.tokenData

            let pairs = other.FindAllPair leftBraceNumber rightBraceNumber 0 true tokToNumber tokToPos

            Assert.AreEqual (1, pairs.Count)
            
            match pairs.[0] with
            | RNGLR.ParseBrackets_3.Token.CLOSE pos -> Assert.AreEqual (4, pos)
            | _ -> Assert.Fail ("Expected CLOSE token")

    [<Test>]
    member test.``Many brackets 2``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3;4;5] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                edg 0 1 (RNGLR.ParseBrackets_3.OPEN 0)
                edg 1 2 (RNGLR.ParseBrackets_3.OPEN 1)
                edg 2 3 (RNGLR.ParseBrackets_3.A 2)
                edg 3 4 (RNGLR.ParseBrackets_3.CLOSE 3)
                edg 4 5 (RNGLR.ParseBrackets_3.CLOSE 4)
             ] |> ignore

        let parseResult = (new Parser<_>()).Parse  RNGLR.ParseBrackets_3.buildAstAbstract qGraph
        
        match parseResult with 
        | Parser.Error (num, tok, err, _, _) -> printErr (num, tok, err)
        | Parser.Success (mAst, _, _) ->
            RNGLR.ParseBrackets_3.defaultAstToDot mAst "Many brackets before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseBrackets_3.otherAstToDot other "Many brackets after.dot"
            
            let tokToNumber = RNGLR.ParseBrackets_3.tokenToNumber
            let leftBraceNumber = tokToNumber <| RNGLR.ParseBrackets_3.Token.OPEN -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseBrackets_3.Token.CLOSE -1

            let tokToPos = tokenToPos RNGLR.ParseBrackets_3.tokenData

            let pairs = other.FindAllPair leftBraceNumber rightBraceNumber 1 true tokToNumber tokToPos

            Assert.AreEqual (1, pairs.Count)
            
            match pairs.[0] with
            | RNGLR.ParseBrackets_3.Token.CLOSE pos -> Assert.AreEqual (3, pos)
            | _ -> Assert.Fail ("Expected CLOSE token")

    [<Test>]
    member test.``AbstractAnalysis case``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.ParseBrackets_3.OPEN  0)
             edg 1 2 (RNGLR.ParseBrackets_3.A 1)
             edg 2 3 (RNGLR.ParseBrackets_3.CLOSE 2)
             edg 2 3 (RNGLR.ParseBrackets_3.CLOSE 3)
             ] |> ignore

        let result = (new Parser<_>()).Parse  RNGLR.ParseBrackets_3.buildAstAbstract qGraph
        printfn "%A" result
        match result with
        | Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!"
        | Parser.Success(mAst, _, _) ->
            RNGLR.ParseBrackets_3.defaultAstToDot mAst "Abstract brackets before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseBrackets_3.otherAstToDot other "Abstract brackets after.dot"
            
            let tokToNumber = RNGLR.ParseBrackets_3.tokenToNumber
            let leftBraceNumber  = tokToNumber <| RNGLR.ParseBrackets_3.Token.OPEN -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseBrackets_3.Token.CLOSE -1

            let tokToPos = tokenToPos RNGLR.ParseBrackets_3.tokenData

            let pairs = other.FindAllPair leftBraceNumber rightBraceNumber 0 true tokToNumber tokToPos

            Assert.AreEqual (2, pairs.Count)

            let expected = [|2; 3|]
            for right in pairs do
                match right with
                | RNGLR.ParseBrackets_3.CLOSE pos -> 
                    if not <| Array.exists (fun num -> pos = num) expected
                    then Assert.Fail()
                | _ -> Assert.Fail()

            Assert.Pass()

    [<Test>]
    member test.``Simple right to left``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.ParseBrackets_3.OPEN  0)
             edg 1 2 (RNGLR.ParseBrackets_3.A 1)
             edg 2 3 (RNGLR.ParseBrackets_3.CLOSE 2)
             ] |> ignore

        let result = (new Parser<_>()).Parse  RNGLR.ParseBrackets_3.buildAstAbstract qGraph
        printfn "%A" result
        match result with
        | Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!"
        | Parser.Success(mAst, _, _) ->
            RNGLR.ParseBrackets_3.defaultAstToDot mAst "Simple right to left before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseBrackets_3.otherAstToDot other "Simple right to left after.dot"
            
            let tokToNumber = RNGLR.ParseBrackets_3.tokenToNumber
            let leftBraceNumber  = tokToNumber <| RNGLR.ParseBrackets_3.Token.OPEN -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseBrackets_3.Token.CLOSE -1

            let tokToPos = tokenToPos RNGLR.ParseBrackets_3.tokenData

            let pairs = other.FindAllPair leftBraceNumber rightBraceNumber 2 false tokToNumber tokToPos

            Assert.AreEqual (1, pairs.Count)

            let expected = [|0|]
            for right in pairs do
                match right with
                | RNGLR.ParseBrackets_3.OPEN pos -> 
                    if not <| Array.exists (fun num -> pos = num) expected
                    then Assert.Fail()
                | _ -> Assert.Fail()

            Assert.Pass()

    [<Test>]
    member test.``AbstractAnalysis case. Right to left``() =
        let qGraph = new ParserInputGraph<_>()
        qGraph.AddVertexRange[0;1;2;3] |> ignore
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.ParseBrackets_3.OPEN  0)
             edg 1 2 (RNGLR.ParseBrackets_3.A 1)
             edg 2 3 (RNGLR.ParseBrackets_3.CLOSE 2)
             edg 2 3 (RNGLR.ParseBrackets_3.CLOSE 3)
             ] |> ignore

        let result = (new Parser<_>()).Parse  RNGLR.ParseBrackets_3.buildAstAbstract qGraph
        printfn "%A" result
        match result with
        | Parser.Error (num, tok, message, debug, _) ->
            printfn "Error in position %d on Token %A: %s" num tok message
            debug.drawGSSDot "out.dot"
            Assert.Fail "!!!!"
        | Parser.Success(mAst, _, _) ->
            RNGLR.ParseBrackets_3.defaultAstToDot mAst "Abstract brackets before.dot"
            let other = new OtherTree<_>(mAst)
            RNGLR.ParseBrackets_3.otherAstToDot other "Abstract brackets after.dot"
            
            let tokToNumber = RNGLR.ParseBrackets_3.tokenToNumber
            let leftBraceNumber  = tokToNumber <| RNGLR.ParseBrackets_3.Token.OPEN -1
            let rightBraceNumber = tokToNumber <| RNGLR.ParseBrackets_3.Token.CLOSE -1

            let tokToPos = tokenToPos RNGLR.ParseBrackets_3.tokenData

            let pairs = other.FindAllPair leftBraceNumber rightBraceNumber 2 false tokToNumber tokToPos

            Assert.AreEqual (1, pairs.Count)

            let expected = [|0|]
            for right in pairs do
                match right with
                | RNGLR.ParseBrackets_3.OPEN pos -> 
                    if not <| Array.exists (fun num -> pos = num) expected
                    then Assert.Fail()
                | _ -> Assert.Fail()

            Assert.Pass()

[<EntryPoint>]
let f x = 
    let test = new ``RNGLR ast to ast translation test``()
    (*test.``Elementary test``()
    test.``Epsilon test``()
    test.``Ambiguous test``()
    test.``Cycles test``()
    *)
    let brackets = new ``Brackets matching``()
    (*brackets.``Simple test``()
    brackets.``More complicated test``()
    brackets.``Many brackets 1``()
    brackets.``Many brackets 2``()
    brackets.``AbstractAnalysis case``()*)
    brackets.``Simple right to left``()
//    brackets.``AbstractAnalysis case. Right to left``()
    1