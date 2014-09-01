module YC.FST.AbstractLexing.Tests.Interpreter

open NUnit.Framework
open YC.FST.GraphBasedFst
open Microsoft.FSharp.Collections 
 
open YC.FST.AbstractLexing.FstLexer
open YC.FST.FstApproximation
open YC.FST.AbstractLexing.Interpreter

[<TestFixture>]
type ``Lexer FST Tests`` () =            
    [<Test>]
    member this.``Lexer FST Tests. Simple test.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 3
        let transitions = new ResizeArray<_>()
        transitions.Add(0, Smb("+", "+"), 1)
        transitions.Add(1, Smb("*", "*"), 2)
        transitions.Add(2, Smb("*", "*"), 1)
        transitions.Add(2, Smb("+", "+"), 3)
        let appr = new Appr<_>(startState, finishState, transitions)
        let fstInputLexer = appr.ToFST()
        let resFST = FST<_,_>.Compos(fstInputLexer, fstLexer())
        resFST.PrintToDOT @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\test1.dot"

    [<Test>]
    member this.``Lexer FST Tests. Test 1.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 3
        let transitions = new ResizeArray<_>()
        transitions.Add(0, Smb("+", "+1"), 1)
        transitions.Add(1, Smb("+", "+2"), 2)
        transitions.Add(2, Smb("*", "*1"), 1)
        transitions.Add(1, Smb("*", "*2"), 3)
        let appr = new Appr<_>(startState, finishState, transitions)
        let fstInputLexer = appr.ToFST()
        let resFST = FST<_,_>.Compos(fstInputLexer, fstLexer())
        resFST.PrintToDOT @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\test2.dot"                      
        Interpret resFST (actions())
[<EntryPoint>]
let f x =
      let t = new ``Lexer FST Tests`` () 
      let a = t.``Lexer FST Tests. Test 1.``()
      printfn "%A" a      
      1