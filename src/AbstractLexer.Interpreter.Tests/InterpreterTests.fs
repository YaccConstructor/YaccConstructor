module YC.FST.AbstractLexing.Tests.Interpreter

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open AbstractAnalysis.Common
open YC.FST.GraphBasedFst
open YC.FST.FstApproximation
open YC.FST.AbstractLexing.Interpreter
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
        transitions.Add(2, Smb("+", "+"), 3)
        let appr = new Appr<_>(startState, finishState, transitions)
        let fstInputLexer = appr.ToFST()
        let resFST = FST<_,_>.Compos(fstInputLexer, YC.FST.AbstractLexing.FstLexer.fstLexer())
        resFST.PrintToDOT @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\test1.dot"
        checkGraph resFST 6 6

    [<Test>]
    member this.``Lexer FST Tests. Check work of interpreter.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 3
        let transitions = new ResizeArray<_>()
        transitions.Add(0, Smb("+", "+1"), 1)
        transitions.Add(1, Smb("+", "+2"), 2)
        transitions.Add(2, Smb("*", "*1"), 1)
        transitions.Add(1, Smb("*", "*2"), 3)
        let appr = new Appr<_>(startState, finishState, transitions)
        //let fstInputLexer = appr.ToFST()
        //let resFST = FST<_,_>.Compos(fstInputLexer, fstLexer())
        //resFST.PrintToDOT @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\test2.dot"                      
        //Interpret resFST (actions()) 
        let parserInputGraph = YC.FST.AbstractLexing.FstLexer.tokenize eof appr
        ToDot parserInputGraph  @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\test2.dot" printBref
        checkGraph parserInputGraph 8 7

    [<Test>]
    member this.``Lexer FST Tests. Test with back_ref 1.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 1
        let transitions = new ResizeArray<_>()
        transitions.Add(0, Smb("++", "+1"), 1)
        let appr = new Appr<_>(startState, finishState, transitions)
        let parserInputGraph = YC.FST.AbstractLexing.FstLexer.tokenize eof appr
        ToDot parserInputGraph  @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\test3.dot" printBref
        checkGraph parserInputGraph 3 4
        let startPos = positions parserInputGraph (fun p -> p.start_offset)
        let endPos = positions parserInputGraph (fun p -> p.end_offset)
        let backref = positions parserInputGraph (fun p -> p.back_ref)
        checkArr startPos [|0; 1|]
        checkArr endPos [|1; 2|]
        checkArr backref [|"+1"; "+1"|]

    [<Test>]
    member this.``Lexer FST Tests. Test with back_ref 2.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 2
        let transitions = new ResizeArray<_>()
        transitions.Add(0, Smb("+*", "+1"), 1)
        transitions.Add(1, Smb("**", "2"), 2)
        let appr = new Appr<_>(startState, finishState, transitions)
        let parserInputGraph = YC.FST.AbstractLexing.FstLexer.tokenize eof appr
        ToDot parserInputGraph  @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\test4.dot" printBref
        checkGraph parserInputGraph 4 5
        let startPos = positions parserInputGraph (fun p -> p.start_offset)
        let endPos = positions parserInputGraph (fun p -> p.end_offset)
        let backref = positions parserInputGraph (fun p -> p.back_ref)
        checkArr startPos [|0; 1; 0; 1|]
        checkArr endPos [|1; 2; 1; 2|]
        checkArr backref [|"+1"; "+1"; "2"; "2"|]

//    [<Test>]
    member this.``Lexer FST Tests. Test with cycle.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 1
        let transitions = new ResizeArray<_>()
        transitions.Add(0, new EdgeLbl<_,_>(Smbl '+', Eps), 0)
        transitions.Add(0, new EdgeLbl<_,_>(Eps, Eps), 1)
        transitions.Add(1, new EdgeLbl<_,_>(Eps, Smbl '*'), 1)
        let fst = new FST<_,_>(startState, finishState, transitions)
        let resFST = FST<_,_>.Compos(fst, YC.FST.AbstractLexing.FstLexer.fstLexer())
        resFST.PrintToDOT @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\testCompose.dot"   
        //let parserInputGraph = Interpret resFST (actions()) eof
        //ToDot parserInputGraph  @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\testComposeGraph.dot" printBref   

//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer FST Tests`` () 
//      let a = t.``Lexer FST Tests. Calc Lexer.``()
//      printfn "%A" a      
//      1