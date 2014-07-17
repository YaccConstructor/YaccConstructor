// Learn more about F# at http://fsharp.net

module GLLParserSimpleTest

open Yard.Generators.GLL
open Yard.Generators.GLL.Parser 
open Yard.Generators.RNGLR.AST
open NUnit.Framework
open Yard.Generators
open LexCommon
open Microsoft.FSharp.Collections

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    (astBuilder()) tokens, tokens

let dir = @"../../../Tests/GLL/"
let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)


[<TestFixture>]
type ``GLL parser tests with simple lexer`` () =
    [<Test>]
    member test.``SIMPLE ambigious``() =
        let parser () = GLL.SimpleAmb.buildAst
        let path = dir + "ADB.txt"
        let r = run path parser
        match r with
        | Parser.Error str, _ -> Assert.Fail("")
        | Parser.Success tree, tokens ->
            GLL.SimpleAmb.defaultAstToDot tree "../../../src/GLLParser.SimpleTest/SimpleAmb.dot"
            
    
    [<Test>]
    member test.``SIMPLE right recursion``() =
        let parser () = GLL.SimpleRightRecursion.buildAst
        let path = dir + "BBB.txt"
        match run path parser with
        | Parser.Error str, _ -> Assert.Fail("")
        | Parser.Success tree, tokens ->
            GLL.SimpleRightRecursion.defaultAstToDot tree "../../../src/GLLParser.SimpleTest/SimpleRightRecursion.dot"

    [<Test>]
    member test.``SIMPLE left recursion``() =
        let parser () = GLL.SimpleLeftRecursion.buildAst
        let path = dir + "BBB.txt"

        match run path parser with
        | Parser.Error str, _ -> Assert.Fail("")
        | Parser.Success tree, tokens ->
            GLL.SimpleLeftRecursion.defaultAstToDot tree "../../../src/GLLParser.SimpleTest/SimpleLeftRecursion.dot"

    [<Test>]
    member test.``BAD left recursion``() =
        let getParser () = GLL.BadLeftRecursion.buildAst
        let path = dir + "BBB.txt"

        match run path getParser with
        | Parser.Error str, _ -> Assert.Fail("")
        | Parser.Success tree, tokems ->
            GLL.BadLeftRecursion.defaultAstToDot tree "../../../src/GLLParser.SimpleTest/BadLeftRecursion.dot"
[<EntryPoint>]
(new ``GLL parser tests with simple lexer``()).``SIMPLE ambigious``()