module RNGLREBNFDFAParserSimpleTest

open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.EBNF.DFA.Parser
open Yard.Generators.RNGLR.EBNF.DFA
open NUnit.Framework
open LexCommon

open System.Collections.Generic
open System.IO

type TestExpectedResult =
    | TER_Success
    | TER_Error

let run path genLiteral astBuilder =
    let tokens = LexCommon.tokens(path, genLiteral)
    astBuilder tokens

let dir = @"../../../Tests/RNGLR.EBNF/"
let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg


[<TestFixture>]
type ``RNGLREBNF parser tests with simple lexer`` () =

//    let translateAndCheck toDot (expected:List<_>) (ast:Tree<_>) file errors = 
//        ast.PrintAst()
//        toDot ast (file + ".dot")
//        let res = translate RNGLR.ParseOmit.translate ast errors
//        printfn "Result: %A" res
//        Assert.AreEqual(expected, res)

    let runTest parser genLiteral file expected = 
        let path = dir + file
        match run path genLiteral parser, expected with
        | Error (num, tok, err, _), TER_Error -> printErr (num, tok, err)
        | Success (tree, _), TER_Success -> printfn "Success"
        | Error (num, tok, err, _), TER_Success ->  
            printErr (num, tok, err)
            Assert.Fail()
        | Success (tree, _), TER_Error -> 
            printfn "Wrong chain was accessed"
            Assert.Fail()

    [<Test>]
    member test.``1.0 One``() =
        let parser = RNGLR.EBNF.DFAParser.One.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.One.genLiteral
        let file = "One.txt"
        runTest parser genLiteral file TER_Success

    [<Test>]
    member test.``1.1 One wrong1``() =
        let parser = RNGLR.EBNF.DFAParser.One.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.One.genLiteral
        let file = "SeqOfTwoWrong1.txt"
        runTest parser genLiteral file TER_Error

    [<Test>]
    member test.``2.0 SeqOfTwo``() =
        let parser = RNGLR.EBNF.DFAParser.SeqOfTwo.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.SeqOfTwo.genLiteral
        let file = "SeqOfTwo.txt"
        runTest parser genLiteral file TER_Success

    [<Test>]
    member test.``2.1 SeqOfTwo wrong1``() =
        let parser = RNGLR.EBNF.DFAParser.SeqOfTwo.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.SeqOfTwo.genLiteral
        let file = "SeqOfTwoWrong1.txt"
        runTest parser genLiteral file TER_Error

    [<Test>]
    member test.``2.2 SeqOfTwo wrong2``() =
        let parser = RNGLR.EBNF.DFAParser.SeqOfTwo.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.SeqOfTwo.genLiteral
        let file = "SeqOfTwoWrong2.txt"
        runTest parser genLiteral file TER_Error

    [<Test>]
    member test.``2.3 SeqOfTwo wrong3``() =
        let parser = RNGLR.EBNF.DFAParser.SeqOfTwo.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.SeqOfTwo.genLiteral
        let file = "SeqOfTwoWrong3.txt"
        runTest parser genLiteral file TER_Error

    [<Test>]
    member test.``3.0 Seq of two nonterminals``() =
        let parser = RNGLR.EBNF.DFAParser.SeqOfTwoNonTerms.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.SeqOfTwoNonTerms.genLiteral
        //let translate = Some RNGLR.EBNF.DFAParser.SeqOfTwoNonTerms.translateAst
        let file = "SeqOfTwo.txt"
        runTest parser genLiteral file TER_Success
    
    [<Test>]
    member test.``4.0 Two parents`` () = 
        let parser = RNGLR.EBNF.DFAParser.TwoParents.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.TwoParents.genLiteral
        let file = "One.txt"
        runTest parser genLiteral file TER_Success

    [<Test>]
    member test.``5.0 First alternative in the middle``() =
        let parser = RNGLR.EBNF.DFAParser.AlternativeInMiddle.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.AlternativeInMiddle.genLiteral
        let file = "AlternativeInMiddle1.txt"
        runTest parser genLiteral file TER_Success

    [<Test>]
    member test.``5.1 Second alternative in the middle``() =
        let parser = RNGLR.EBNF.DFAParser.AlternativeInMiddle.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.AlternativeInMiddle.genLiteral
        let file = "AlternativeInMiddle2.txt"
        runTest parser genLiteral file TER_Success
    
    [<Test>]
    member test.``6.0 Two many nonterminals`` () = 
        let parser = RNGLR.EBNF.DFAParser.TwoManyNonTerms.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.TwoManyNonTerms.genLiteral
        let file = "SeqOfThree.txt"
        runTest parser genLiteral file TER_Success

        
    [<Test>]
    member test.``7.0 Two many terminals`` () = 
        let parser = RNGLR.EBNF.DFAParser.TwoManyTerms.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.TwoManyTerms.genLiteral
        let file = "SeqOfThree.txt"
        runTest parser genLiteral file TER_Success

    [<Test>]
    member test.``7.1 Two many one terminal`` () = 
        let parser = RNGLR.EBNF.DFAParser.TwoManyTerms.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.TwoManyTerms.genLiteral
        let file = "One.txt"
        runTest parser genLiteral file TER_Success
    
    [<Test>]
    member test.``8.0 CalcEBNF`` () = 
        let parser = RNGLR.EBNF.DFAParser.CalcEBNF.buildAst
        let genLiteral = RNGLR.EBNF.DFAParser.CalcEBNF.genLiteral
        let file = "CalcEBNF.txt"
        runTest parser genLiteral file TER_Success

[<EntryPoint>]
let main argv = 
    (new ``RNGLREBNF parser tests with simple lexer``()).``1.0 One``();
    0 // return an integer exit code