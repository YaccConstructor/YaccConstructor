module YC.FST.AbstractLexing.Tests.LiteralsTests

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open YC.Utils.StructClass
open QuickGraph.FST.GraphBasedFst
open YC.FST.AbstractLexing.Interpreter
open YC.FST.AbstractLexing.Tests.CommonTestChecker
open QuickGraph.FSA.GraphBasedFsa
open System

let baseInputGraphsPath = "../../../tests/data/AbstractLexing/DOT"

let transform x = (x, match x with |Smbl(y:char, _) when y <> (char 65535) -> Smbl(int y) |Smbl(y:char, _) when y = (char 65535) -> Smbl 65535 |_ -> Eps)
let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)
  
let literalsTokenizationTest file eCount vCount pathPrint =
    let graph = loadDotToQG (path baseInputGraphsPath file)
    let graphFsa = approximateQG(graph)
    let graphFst = FST<_,_>.FSAtoFST(graphFsa, transform, smblEOF)
    let res = YC.FST.AbstractLexing.LiteralsLexer.tokenize eof graphFst
    match res with
    | Success res -> checkGraph res eCount vCount  
    | Error e -> Assert.Fail(sprintf "Tokenization problem in test %s: %A" file e)

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
