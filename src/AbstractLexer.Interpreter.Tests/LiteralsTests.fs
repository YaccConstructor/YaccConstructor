module YC.FST.AbstractLexing.Tests.LiteralsTests

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open AbstractAnalysis.Common
open YC.FST.GraphBasedFst
open YC.FST.AbstractLexing.Interpreter
open YC.FST.AbstractLexing.Tests.CommonTestChecker

let baseInputGraphsPath = "../../../../Tests/AbstractLexing/DOT"
  
let literalsTokenizationTest path eCount vCount =
    let graphAppr = loadDotToQG baseInputGraphsPath path
    let res = YC.FST.AbstractLexing.LiteralsLexer.tokenize eof graphAppr
    checkGraph res eCount vCount

[<TestFixture>]
type ``Lexer Literals Fst Tests`` () =   
    [<Test>]
    member this.``Literals. Simple.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "literals_simple.dot"
        let res = YC.FST.AbstractLexing.LiteralsLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserLiterals0.dot" printBref
        checkGraph res 2 3

    [<Test>] 
    member this.``Literals. Inner branch.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "literals_inner_branch.dot"
        let res = YC.FST.AbstractLexing.LiteralsLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserLiterals1.dot" printBref
        checkGraph res 2 3

    [<Test>]
    member this.``Literals. Outer branch.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "literals_outer_branch.dot"
        let res = YC.FST.AbstractLexing.LiteralsLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserLiterals2.dot" printBref
        checkGraph res 2 3

    [<Test>]
    member this.``Literals. Splitted.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "literals_splitted.dot"
        let res = YC.FST.AbstractLexing.LiteralsLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserLiterals3.dot" printBref
        checkGraph res 2 3

//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer Literals Fst Tests`` () 
//      let a = t.``Literals. Splitted.``()
//      //printfn "%A" a      
//      1
