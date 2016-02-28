module RNGLRReadBackParserSimpleTest

open Yard.Generators.RNGLR.ReadBack.Parser
open NUnit.Framework
open LexCommon

type TestExpectedResult =
    | TER_Success
    | TER_Error

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens

let dir = @"../../../Tests/RNGLR.EBNF/"
let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg


[<TestFixture>]
type ``RNGLRReadBack parser tests with simple lexer`` () =

//    let translateAndCheck toDot (expected:List<_>) (ast:Tree<_>) file errors = 
//        ast.PrintAst()
//        toDot ast (file + ".dot")
//        let res = translate RNGLR.ParseOmit.translate ast errors
//        printfn "Result: %A" res
//        Assert.AreEqual(expected, res)

    let runTest parser file expected = 
        let path = dir + file
        match run path parser, expected with
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
        let parser = RNGLR.ReadBackParser.One.buildAst
        let file = "One.txt"
        runTest parser file TER_Success

    [<Test>]
    member test.``1.1 One wrong1``() =
        let parser = RNGLR.ReadBackParser.One.buildAst
        let file = "SeqOfTwoWrong1.txt"
        runTest parser file TER_Error

    [<Test>]
    member test.``2.0 SeqOfTwo``() =
        let parser = RNGLR.ReadBackParser.SeqOfTwo.buildAst
        let file = "SeqOfTwo.txt"
        runTest parser file TER_Success

    [<Test>]
    member test.``2.1 SeqOfTwo wrong1``() =
        let parser = RNGLR.ReadBackParser.SeqOfTwo.buildAst
        let file = "SeqOfTwoWrong1.txt"
        runTest parser file TER_Error

    [<Test>]
    member test.``2.2 SeqOfTwo wrong2``() =
        let parser = RNGLR.ReadBackParser.SeqOfTwo.buildAst
        let file = "SeqOfTwoWrong2.txt"
        runTest parser file TER_Error

    [<Test>]
    member test.``2.3 SeqOfTwo wrong3``() =
        let parser = RNGLR.ReadBackParser.SeqOfTwo.buildAst
        let file = "SeqOfTwoWrong3.txt"
        runTest parser file TER_Error
    
    
    [<Test>]
    member test.``3.0 First alternative in the middle``() =
        let parser = RNGLR.ReadBackParser.AlternativeInMiddle.buildAst
        let file = "AlternativeInMiddle1.txt"
        runTest parser file TER_Success

    [<Test>]
    member test.``3.1 Second alternative in the middle``() =
        let parser = RNGLR.ReadBackParser.AlternativeInMiddle.buildAst
        let file = "AlternativeInMiddle2.txt"
        runTest parser file TER_Success
    
    [<Test>]
    member test.``4.0 CalcEBNF`` () = 
        let parser = RNGLR.ReadBackParser.CalcEBNF.buildAst
        let file = "CalcEBNF.txt"
        runTest parser file TER_Success

    (*[<Test>]
    member test.``Choice`` () = 
        let parser = RNGLR.ReadBackParser.Choice.buildAst
        let file = "Choice.txt"
        runTest parser file

    [<Test>]
    member test.``ComplexRightNull`` () = 
        let parser = RNGLR.ReadBackParser.ComplexRightNull.buildAst
        let file = "ComplexRightNull.txt"
        runTest parser file

    [<Test>]
    member test.``ComplexRightNull2`` () = 
        let parser = RNGLR.ReadBackParser.ComplexRightNull.buildAst
        let file = "ComplexRightNull2.txt"
        runTest parser file

    [<Test>]
    member test.``ManyAndOne0`` () = 
        let parser = RNGLR.ReadBackParser.ManyAndOne.buildAst
        let file = "ManyAndOne0.txt"
        runTest parser file
    
    [<Test>]
    member test.``ManyAndOne1`` () = 
        let parser = RNGLR.ReadBackParser.ManyAndOne.buildAst
        let file = "ManyAndOne1.txt"
        runTest parser file

    [<Test>]
    member test.``ManyAndOne2`` () = 
        let parser = RNGLR.ReadBackParser.ManyAndOne.buildAst
        let file = "ManyAndOne2.txt"
        runTest parser file

    [<Test>]
    member test.``simpleOneTerm`` () = 
        let parser = RNGLR.ReadBackParser.SimpleOpt.buildAst
        let file = "simpleOneTerm.txt"
        runTest parser file

    [<Test>]
    member test.``RightNull`` () = 
        let parser = RNGLR.ReadBackParser.RightNull.buildAst
        let file = "RightNull.txt"
        runTest parser file

    [<Test>]
    member test.``SimpleEpsilon`` () = 
        let parser = RNGLR.ReadBackParser.SimpleEpsilon.buildAst
        let file = "simpleEpsilon.txt"
        runTest parser file

    [<Test>]
    member test.``SimpleRightNull`` () = 
        let parser = RNGLR.ReadBackParser.SimpleRightNull.buildAst
        let file = "SimpleRightNull.txt"
        runTest parser file

    [<Test>]
    member test.``StackingConflictWrong`` () = 
        let parser = RNGLR.ReadBackParser.StackingConflict.buildAst
        let file = "StackingConflictWrong.txt"
        runTest parser file

    [<Test>]
    member test.``TwoEpsilonMiddle`` () = 
        let parser = RNGLR.ReadBackParser.TwoEpsilonsMiddle.buildAst
        let file = "TwoEpsilonsMiddle.txt"
        runTest parser file

    [<Test>]
    member test.``TwoEpsilonMiddleWrong`` () = 
        let parser = RNGLR.ReadBackParser.TwoEpsilonsMiddle.buildAst
        let file = "TwoEpsilonsMiddleWrong.txt"
        runTest parser file*)

//    [<Test>]
//    member test.``Choice_0050000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0050000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0100000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0100000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0150000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0150000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0200000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0200000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0250000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0250000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0300000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0300000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0350000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0350000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0400000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0400000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0450000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0450000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0500000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0500000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0550000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0550000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0600000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0600000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0650000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0650000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0700000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0700000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0750000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0750000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0800000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0800000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0850000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0850000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0900000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0900000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0950000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_0950000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_1000000`` () = 
//        let parser = RNGLR.ReadBackParser.Choice.buildAst
//        let file = "Choice_1000000.txt"
//        runTest parser file

[<EntryPoint>]
let main argv = 
    (new ``RNGLRReadBack parser tests with simple lexer``()).``4.0 CalcEBNF``();
    0 // return an integer exit code