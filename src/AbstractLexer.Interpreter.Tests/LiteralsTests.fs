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
        literalsTokenizationTest "literals_simple.dot" 2 3

    //[<Test>] DON'T WORK RIGHT!!
    member this.``Literals. Inner branch.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "literals_inner_branch.dot"
        let res = YC.FST.AbstractLexing.LiteralsLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\CalcTest.dot" printBref
        checkGraph res 3 3
        //literalsTokenizationTest "literals_inner_branch.dot" 3 3

    [<Test>]
    member this.``Literals. Outer branch.`` () =
        literalsTokenizationTest "literals_outer_branch.dot" 3 3

    //[<Test>] DON'T WORK RIGHT!!
    member this.``Literals. Splitted.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "literals_splitted.dot"
        let res = YC.FST.AbstractLexing.LiteralsLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\CalcTest.dot" printBref
        checkGraph res 3 3
        //literalsTokenizationTest "literals_splitted.dot" 3 3

//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer Literals Fst Tests`` () 
//      let a = t.``Literals. Splitted.``()
//      //printfn "%A" a      
//      1

