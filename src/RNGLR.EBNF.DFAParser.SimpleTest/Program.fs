module RNGLREBNFDFAParserSimpleTest

open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.EBNF.DFA.Parser
open Yard.Generators.RNGLR.EBNF.DFA
open NUnit.Framework
open LexCommon


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
        let path = dir + file
        match run path parser with
        | Error (num, tok, err, _) -> printErr (num, tok, err)
        | Success (tree, _) -> printfn "Success"

    [<Test>]
    member test.``CalcEBNF`` () = 
        let parser = RNGLR.ParseCalcEBNF.buildAst
        let file = "CalcEBNF.txt"
        runTest parser file

    [<Test>]
    member test.``ComplexRightNull`` () = 
        let parser = RNGLR.ParseComplexRightNull.buildAst
        let file = "ComplexRightNull.txt"
        runTest parser file

    [<Test>]
    member test.``ComplexRightNull2`` () = 
        let parser = RNGLR.ParseComplexRightNull.buildAst
        let file = "ComplexRightNull2.txt"
        runTest parser file

    [<Test>]
    member test.``ManyAndOne0`` () = 
        let parser = RNGLR.ParseManyAndOne.buildAst
        let file = "ManyAndOne0.txt"
        runTest parser file
    
    [<Test>]
    member test.``ManyAndOne1`` () = 
        let parser = RNGLR.ParseManyAndOne.buildAst
        let file = "ManyAndOne1.txt"
        runTest parser file

    [<Test>]
    member test.``ManyAndOne2`` () = 
        let parser = RNGLR.ParseManyAndOne.buildAst
        let file = "ManyAndOne2.txt"
        runTest parser file

    [<Test>]
    member test.``simpleOneTerm`` () = 
        let parser = RNGLR.ParseSimpleOpt.buildAst
        let file = "simpleOneTerm.txt"
        runTest parser file

    [<Test>]
    member test.``RightNull`` () = 
        let parser = RNGLR.ParseRightNull.buildAst
        let file = "RightNull.txt"
        runTest parser file

    [<Test>]
    member test.``SimpleEpsilon`` () = 
        let parser = RNGLR.ParseSimpleEpsilon.buildAst
        let file = "simpleEpsilon.txt"
        runTest parser file

    [<Test>]
    member test.``SimpleRightNull`` () = 
        let parser = RNGLR.ParseSimpleRightNull.buildAst
        let file = "SimpleRightNull.txt"
        runTest parser file

    [<Test>]
    member test.``StackingConflictWrong`` () = 
        let parser = RNGLR.ParseStackingConflict.buildAst
        let file = "StackingConflictWrong.txt"
        runTest parser file

    [<Test>]
    member test.``TwoEpsilonMiddle`` () = 
        let parser = RNGLR.ParseTwoEpsilonsMiddle.buildAst
        let file = "TwoEpsilonsMiddle.txt"
        runTest parser file

    [<Test>]
    member test.``TwoEpsilonMiddleWrong`` () = 
        let parser = RNGLR.ParseTwoEpsilonsMiddle.buildAst
        let file = "TwoEpsilonsMiddleWrong.txt"
        runTest parser file

[<EntryPoint>]
let main argv = 
    let parser = RNGLR.ParseSimpleOpt.buildAst
    let path = dir + "simpleOneTerm.txt"
    match run path parser with
        | Error (num, tok, err, _) -> printErr (num, tok, err)
        | Success (tree, _) -> 
            printfn "Success"
    0 // return an integer exit code