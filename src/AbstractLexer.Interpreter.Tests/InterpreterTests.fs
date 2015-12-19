module YC.FST.AbstractLexing.Tests.Interpreter

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open AbstractAnalysis.Common
open QuickGraph.FST.GraphBasedFst
open YC.FST.AbstractLexing.Interpreter
open AbstractParser.Tokens
open YC.FST.AbstractLexing.Tests.CommonTestChecker
open QuickGraph.FSA.FsaApproximation
open QuickGraph.FSA.GraphBasedFsa
open System

let transform x = (x, match x with |Smbl(y:char, _) when y <> (char 65535) -> Smbl(int y) |Smbl(y:char, _) when y = (char 65535) -> Smbl 65535 |_ -> Eps)
let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)
     
let TokenizationTest (graphAppr:Appr<_>) eCount vCount  =
    let graphFsa = graphAppr.ApprToFSA()
    let graphFst = FST<_,_>.FSAtoFST(graphFsa, transform, smblEOF)
    let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphFst       
    match res with
    | Success res -> 
                //ToDot res @"../../../src/AbstractLexer.Interpreter.Tests/Tests/TestInterpretParser.dot" (printBref printSmbString)
                checkGraph res eCount vCount   
    | Error e -> Assert.Fail(sprintf "Tokenization problem %A:" e)
 
[<TestFixture>]
type ``Lexer FST Tests`` () =            
    [<Test>]
    member this.``Lexer FST Tests. Check composition with lexer.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 3
        let transitions = new ResizeArray<_>()
        transitions.Add(0, ("+", "+"), 1)
        transitions.Add(1, ("*", "*"), 2)
        transitions.Add(2, ("*", "*"), 1)
        transitions.Add(1, ("*", "*"), 3)
        let appr = new Appr<_>(startState, finishState, transitions)
        TokenizationTest appr 6 5 

    [<Test>]
    member this.``Lexer FST Tests. Check work of interpreter.`` () = 
        let startState = ResizeArray.singleton 0
        let finishState = ResizeArray.singleton 4
        let transitions = new ResizeArray<_>()
        transitions.Add(0, ("1", "1"), 1)
        transitions.Add(0, ("2", "2"), 1)
        transitions.Add(0, ("3", "3"), 1)
        transitions.Add(1, ("4", "4"), 2)
        transitions.Add(1, ("5", "5"), 2)
        transitions.Add(1, ("-", "-"), 3)
        transitions.Add(2, ("+", "+"), 3)
        transitions.Add(3, ("6", "6"), 4)
        let appr = new Appr<_>(startState, finishState, transitions)
        TokenizationTest appr 7 7

//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer FST Tests`` () 
//      let a = t.``Lexer FST Tests. Check composition with lexer.``()
//      printfn "%A" a      
//      1