module YC.FST.AbstractLexing.Tests.Interpreter

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open AbstractAnalysis.Common
open YC.FST.GraphBasedFst
open YC.FST.FstApproximation
open YC.FST.AbstractLexing.Interpreter
open AbstractParser.Tokens
open YC.FST.AbstractLexing.Tests.CommonTestChecker
     
let TokenizationTest graphAppr eCount vCount  =
    let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
    match res with
    | Success res -> checkGraph res eCount vCount   
    | Error e -> Assert.Fail(sprintf "Tokenization problem %A:" e)
 
[<TestFixture>]
type ``Lexer FST Tests`` () =            
    [<Test>]
    member this.``Lexer FST Tests. Check composition with lexer.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 3
        let transitions = new ResizeArray<_>()
        transitions.Add(0, Smb("+", "+"), 1)
        transitions.Add(1, Smb("a", "a"), 2)
        transitions.Add(2, Smb("*", "*"), 1)
        transitions.Add(1, Smb("*", "*"), 3)
        let appr = new Appr<_>(startState, finishState, transitions)
        TokenizationTest appr 7 6 

    [<Test>]
    member this.``Lexer FST Tests. Check work of interpreter.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 4
        let transitions = new ResizeArray<_>()
        transitions.Add(0, Smb("1", "1"), 1)
        transitions.Add(0, Smb("2", "2"), 1)
        transitions.Add(0, Smb("3", "3"), 1)
        transitions.Add(1, Smb("4", "4"), 2)
        transitions.Add(1, Smb("5", "5"), 2)
        transitions.Add(1, Smb("-", "-"), 3)
        transitions.Add(2, Smb("+", "+"), 3)
        transitions.Add(3, Smb("6", "6"), 4)
        let appr = new Appr<_>(startState, finishState, transitions)
        TokenizationTest appr 7 7

//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer FST Tests`` () 
//      let a = t.``Lexer FST Tests. Check work of interpreter.``()
//      printfn "%A" a      
//      1