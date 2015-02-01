module YC.FST.AbstractLexing.Tests.LiteralsTests

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open AbstractAnalysis.Common
open YC.FST.GraphBasedFst
open YC.FST.AbstractLexing.Interpreter
open YC.FST.AbstractLexing.Tests.CommonTestChecker

let baseInputGraphsPath = "../../../Tests/AbstractLexing/DOT"
  
let literalsTokenizationTest path eCount vCount pathPrint =
    let graphAppr = loadDotToQG baseInputGraphsPath path
    let res = YC.FST.AbstractLexing.LiteralsLexer.tokenize eof graphAppr
    match res with
    | Success res -> 
        checkGraph res eCount vCount  
        ToDot res pathPrint printBref 
    | Error e -> Assert.Fail(sprintf "Tokenization problem in test %s: %A" path e)

[<TestFixture>]
type ``Lexer Literals Fst Tests`` () =   
    [<Test>]  
    member this.``Literals. Simple.`` () =
        literalsTokenizationTest "literals_simple.dot" 2 3 @"testParserLiterals0.dot"

    [<Test>] 
    member this.``Literals. Inner branch.`` () =
        literalsTokenizationTest "literals_inner_branch.dot" 2 3 @"testParserLiterals0.dot"

    [<Test>]
    member this.``Literals. Outer branch.`` () =
        literalsTokenizationTest "literals_outer_branch.dot" 2 3 @"testParserLiterals0.dot"

    [<Test>]
    member this.``Literals. Splitted.`` () =
        literalsTokenizationTest "literals_splitted.dot" 2 3 @"testParserLiterals0.dot"

//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer Literals Fst Tests`` () 
//      let a = t.``Literals. Splitted.``()
//      //printfn "%A" a      
//      1
