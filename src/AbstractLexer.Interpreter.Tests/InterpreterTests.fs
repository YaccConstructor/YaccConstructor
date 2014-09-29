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
      
 
[<TestFixture>]
type ``Lexer FST Tests`` () =            
    [<Test>]
    member this.``Lexer FST Tests. Check composition with lexer.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 3
        let transitions = new ResizeArray<_>()
        transitions.Add(0, Smb("+", "+"), 1)
        transitions.Add(1, Smb("*", "*"), 2)
        transitions.Add(2, Smb("*", "*"), 1)
        transitions.Add(1, Smb("*", "*"), 3)
        let appr = new Appr<_>(startState, finishState, transitions)
        let fstInputLexer = appr.ToFST()
        let resFST = FST<_,_>.Compos(fstInputLexer, YC.FST.AbstractLexing.CalcLexer.fstLexer())
        resFST.PrintToDOT(@"..\..\Tests\test1.dot", printSmbString)
        let parserInputGraph = YC.FST.AbstractLexing.CalcLexer.tokenize eof appr
        ToDot parserInputGraph @"..\..\Tests\testParser1.dot" printBref
        checkGraph resFST 7 6

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
        let fstInputLexer = appr.ToFST()
        let resFST = FST<_,_>.Compos(fstInputLexer, YC.FST.AbstractLexing.CalcLexer.fstLexer())
        resFST.PrintToDOT(@"..\..\Tests\test2.dot", printSmbString)
        let parserInputGraph = YC.FST.AbstractLexing.CalcLexer.tokenize eof appr
        ToDot parserInputGraph @"..\..\Tests\testParser2.dot" printBref
        checkGraph parserInputGraph 7 7

//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer FST Tests`` () 
//      let a = t.``Lexer FST Tests. Check work of interpreter.``()
//      printfn "%A" a      
//      1