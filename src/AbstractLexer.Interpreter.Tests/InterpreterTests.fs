module YC.FST.AbstractLexing.Tests.Interpreter

open NUnit.Framework
open YC.FST.GraphBasedFst
open Microsoft.FSharp.Collections 
 
open YC.FST.AbstractLexing.FstLexer
open YC.FST.FstApproximation
open YC.FST.AbstractLexing.Interpreter
open AbstractAnalysis.Common

let eof = RNGLR_EOF("", [||])

let printTok =
     fun x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0])

let printBref =
    let printBrs brs =
        "["
        + (brs |> Array.map (fun (pos:Position<_>) -> pos.back_ref ) |> String.concat "; ")
        + "]"        
        + (brs |> Array.map (fun (pos:Position<_>) -> "(" + pos.start_offset.ToString() + ", " + pos.end_offset.ToString() + ")") |> String.concat "; ")         

    fun x ->
        match x with
            | PLUS(v,br) -> "+: " + printBrs br
            | MULT(v,br) -> "*: " + printBrs br
            | POW(v,br)  -> "**: " + printBrs br
            | x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0]) 
   
     
[<TestFixture>]
type ``Lexer FST Tests`` () =            
    [<Test>]
    member this.``Lexer FST Tests. Simple test.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 3
        let transitions = new ResizeArray<_>()
        transitions.Add(0, Smb("+", "+1"), 1)
        transitions.Add(1, Smb("*", "*1"), 2)
        transitions.Add(2, Smb("*", "*2"), 1)
        transitions.Add(2, Smb("+", "+2"), 3)
        let appr = new Appr<_>(startState, finishState, transitions)
        //let fstInputLexer = appr.ToFST()
        //let resFST = FST<_,_>.Compos(fstInputLexer, fstLexer())
        //resFST.PrintToDOT @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\test1.dot"
        let parserInputGraph = tokenize eof appr
        ToDot parserInputGraph  @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\testDOT1.dot" printBref

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
        //let fstInputLexer = appr.ToFST()
        //let resFST = FST<_,_>.Compos(fstInputLexer, fstLexer())
        //resFST.PrintToDOT @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\test2.dot"                      
        //Interpret resFST (actions()) 
        let parserInputGraph = tokenize eof appr
        ToDot parserInputGraph  @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\testDOT2.dot" printBref

    [<Test>]
    member this.``Lexer FST Tests. Test with back_ref 1.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 1
        let transitions = new ResizeArray<_>()
        transitions.Add(0, Smb("++", "+1"), 1)
        let appr = new Appr<_>(startState, finishState, transitions)
        let parserInputGraph = tokenize eof appr
        ToDot parserInputGraph  @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\test3.dot" printBref

    [<Test>]
    member this.``Lexer FST Tests. Test with back_ref 2.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 2
        let transitions = new ResizeArray<_>()
        transitions.Add(0, Smb("+*", "+1"), 1)
        transitions.Add(1, Smb("**", "2"), 2)
        let appr = new Appr<_>(startState, finishState, transitions)
        let parserInputGraph = tokenize eof appr
        ToDot parserInputGraph  @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\test4.dot" printBref


//    [<Test>]
//    member this.``Lexer FST Tests. Test with cycle.`` () = 
//        let startState = ResizeArray.singleton 0
//        let finishState = ResizeArray.singleton 1
//        let transitions = new ResizeArray<_>()
//        transitions.Add(0, new EdgeLbl<_,_>(Smbl '+', Eps), 0)
//        transitions.Add(0, new EdgeLbl<_,_>(Eps, Eps), 1)
//        transitions.Add(1, new EdgeLbl<_,_>(Eps, Smbl '*'), 1)
//        let fst = new FST<_,_>(startState, finishState, transitions)
//        let resFST = FST<_,_>.Compos(fst, fstLexer())
//        resFST.PrintToDOT @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\testCompose.dot"   
//        //let parserInputGraph = Interpret resFST (actions()) eof
//        //ToDot parserInputGraph  @"C:\recursive-ascent\src\AbstractLexer.Interpreter.Tests\Tests\testComposeGraph.dot" printBref   

[<EntryPoint>]
let f x =
      let t = new ``Lexer FST Tests`` () 
      let a = t.``Lexer FST Tests. Simple test.``()
      printfn "%A" a      
      1