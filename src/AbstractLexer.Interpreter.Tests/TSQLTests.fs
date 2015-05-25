module YC.FST.AbstractLexing.Tests.TSQLTests

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open AbstractAnalysis.Common
open YC.FST.GraphBasedFst
open YC.FST.AbstractLexing.Interpreter
open YC.FST.AbstractLexing.Tests.CommonTestChecker
open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation

let baseInputGraphsPath = "../../../Tests/AbstractLexing/DOT"

let transform x = (x, match x with |Smbl(y, _) -> Smbl y |_ -> Eps)
let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)

let equalSmbl x y = (fst x) = (fst y)

let getChar x = 
    match x with
    | Smbl(y, _) -> y
    | _ -> failwith "Unexpected symb in alphabet of FSA!"

let newSmb x =  Smbl(x, Unchecked.defaultof<_>)
  
let TSQLTokenizationTest path eCount vCount =
    let graphAppr = loadDotToQG baseInputGraphsPath path
    let graphFsa = graphAppr.ApprToFSA()
    let graphFst = FST<_,_>.FSAtoFST(graphFsa, transform, smblEOF)
    let res = YC.TSQLLexer.tokenize (TSQLParserToken.RNGLR_EOF(new FSA<_>())) graphFst
    match res with
    | Success res -> 
        //ToDot res @"../../../src/AbstractLexer.Interpreter.Tests/Tests/TestInterpretParser1.dot" (printBrefTSQL printSmbString)
        checkGraph res eCount vCount  
    | Error e -> Assert.Fail(sprintf "Tokenization problem in test %s: %A" path e)

[<TestFixture>]
type ``Lexer TSQL Fst Tests`` () =   
    [<Test>]  
    member this.``TSQL. Simple.`` () =
        TSQLTokenizationTest "tsql_test_1.dot" 15 15

    [<Test>]  
    member this.``TSQL. Example.`` () =
        TSQLTokenizationTest "tsql_test_2.dot" 5 6

    [<Test>]  
    member this.``TSQL. Replace Example.`` () =
        TSQLTokenizationTest "tsql_test_3.dot" 5 6
        let graphAppr = loadDotToQG baseInputGraphsPath "tsql_test_3.dot"
        let graphAppr_1 = loadDotToQG baseInputGraphsPath "tsql_test_3_1.dot"
        let graphAppr_2 = loadDotToQG baseInputGraphsPath "tsql_test_3_2.dot"
        let graphFsa = graphAppr.ApprToFSA()
        let graphFsa_1 = graphAppr_1.ApprToFSA()
        let graphFsa_2 = graphAppr_2.ApprToFSA()
        let resReplace = FSA<_>.Replace(graphFsa, graphFsa_1, graphFsa_2, '~', '^', getChar, newSmb, equalSmbl)
        let graphFst = FST<_,_>.FSAtoFST(resReplace, transform, smblEOF)
        let res = YC.TSQLLexer.tokenize (TSQLParserToken.RNGLR_EOF(new FSA<_>())) graphFst
        match res with
            | Success res -> 
                //ToDot res @"../../../src/AbstractLexer.Interpreter.Tests/Tests/ReplaceEx.dot" (printBrefTSQL printSmbString)
                checkGraph res 4 5 
            | Error e -> Assert.Fail(sprintf "Tokenization problem in test %s: %A" "tsql_test_3.dot" e)

    [<Test>]  
    member this.``TSQL. While example.`` () =
        TSQLTokenizationTest "tsql_test_4.dot" 12 11

    [<Test>]  
    member this.``TSQL. Token with cycle.`` () =
        TSQLTokenizationTest "tsql_test_5.dot" 5 6


//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer TSQL Fst Tests`` () 
//      let a = t.``TSQL. Token with cycle.``()
//      //printfn "%A" a      
//      1



