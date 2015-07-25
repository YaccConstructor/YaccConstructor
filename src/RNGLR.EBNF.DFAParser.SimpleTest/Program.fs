module RNGLREBNFDFAParserSimpleTest

open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.EBNF.DFA.Parser
open Yard.Generators.RNGLR.EBNF.DFA
open NUnit.Framework
open LexCommon

open System.Collections.Generic
open System.IO


let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens

let dir = @"../../../Tests/RNGLR.EBNF.DFA/"
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

    let runTest parser file = 
//        let pathToLog = @".\res.txt"
//        let wr = new StreamWriter(pathToLog, true)
        let path = dir + file
//        let start = System.DateTime.Now
        match run path parser with
        | Error (num, tok, err, _) -> printErr (num, tok, err)
        | Success (tree, _) -> printfn "Success"
//        | Success (tree, _) -> wr.WriteLine(System.DateTime.Now - start)
//        wr.Close()

    [<Test>]
    member test.``CalcEBNF`` () = 
        let parser = RNGLR.EBNF.DFAParserCalcEBNF.buildAst
        let file = "CalcEBNF.txt"
        runTest parser file

    [<Test>]
    member test.``Choice`` () = 
        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
        let file = "Choice.txt"
        runTest parser file

    [<Test>]
    member test.``ComplexRightNull`` () = 
        let parser = RNGLR.EBNF.DFAParserComplexRightNull.buildAst
        let file = "ComplexRightNull.txt"
        runTest parser file

    [<Test>]
    member test.``ComplexRightNull2`` () = 
        let parser = RNGLR.EBNF.DFAParserComplexRightNull.buildAst
        let file = "ComplexRightNull2.txt"
        runTest parser file

    [<Test>]
    member test.``ManyAndOne0`` () = 
        let parser = RNGLR.EBNF.DFAParserManyAndOne.buildAst
        let file = "ManyAndOne0.txt"
        runTest parser file
    
    [<Test>]
    member test.``ManyAndOne1`` () = 
        let parser = RNGLR.EBNF.DFAParserManyAndOne.buildAst
        let file = "ManyAndOne1.txt"
        runTest parser file

    [<Test>]
    member test.``ManyAndOne2`` () = 
        let parser = RNGLR.EBNF.DFAParserManyAndOne.buildAst
        let file = "ManyAndOne2.txt"
        runTest parser file

    [<Test>]
    member test.``simpleOneTerm`` () = 
        let parser = RNGLR.EBNF.DFAParserSimpleOpt.buildAst
        let file = "simpleOneTerm.txt"
        runTest parser file

    [<Test>]
    member test.``RightNull`` () = 
        let parser = RNGLR.EBNF.DFAParserRightNull.buildAst
        let file = "RightNull.txt"
        runTest parser file

    [<Test>]
    member test.``SimpleEpsilon`` () = 
        let parser = RNGLR.EBNF.DFAParserSimpleEpsilon.buildAst
        let file = "simpleEpsilon.txt"
        runTest parser file

    [<Test>]
    member test.``SimpleRightNull`` () = 
        let parser = RNGLR.EBNF.DFAParserSimpleRightNull.buildAst
        let file = "SimpleRightNull.txt"
        runTest parser file

    [<Test>]
    member test.``StackingConflictWrong`` () = 
        let parser = RNGLR.EBNF.DFAParserStackingConflict.buildAst
        let file = "StackingConflictWrong.txt"
        runTest parser file

    [<Test>]
    member test.``TwoEpsilonMiddle`` () = 
        let parser = RNGLR.EBNF.DFAParserTwoEpsilonsMiddle.buildAst
        let file = "TwoEpsilonsMiddle.txt"
        runTest parser file

    [<Test>]
    member test.``TwoEpsilonMiddleWrong`` () = 
        let parser = RNGLR.EBNF.DFAParserTwoEpsilonsMiddle.buildAst
        let file = "TwoEpsilonsMiddleWrong.txt"
        runTest parser file

//    [<Test>]
//    member test.``Choice_0050000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0050000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0100000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0100000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0150000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0150000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0200000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0200000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0250000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0250000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0300000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0300000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0350000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0350000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0400000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0400000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0450000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0450000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0500000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0500000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0550000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0550000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0600000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0600000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0650000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0650000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0700000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0700000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0750000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0750000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0800000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0800000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0850000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0850000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0900000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0900000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_0950000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_0950000.txt"
//        runTest parser file
//
//    [<Test>]
//    member test.``Choice_1000000`` () = 
//        let parser = RNGLR.EBNF.DFAParserChoice.buildAst
//        let file = "Choice_1000000.txt"
//        runTest parser file

[<EntryPoint>]
let main argv = 
    let parser = RNGLR.EBNF.DFAParserChoice.buildAst
    let path = dir + "Choice.txt"
    let start = System.DateTime.Now
    match run path parser with
        | Error (num, tok, err, _) -> printErr (num, tok, err)
        | Success (tree, _) -> printfn "%A" (System.DateTime.Now - start)
    0 // return an integer exit code