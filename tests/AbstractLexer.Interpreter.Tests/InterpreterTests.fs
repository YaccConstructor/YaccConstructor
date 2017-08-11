module YC.FST.AbstractLexing.Tests.Interpreter

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open YC.Utils.StructClass
open QuickGraph.FST.GraphBasedFst
open YC.FST.AbstractLexing.Interpreter
open AbstractParser.Tokens
open YC.FST.AbstractLexing.Tests.CommonTestChecker
open QuickGraph.FSA.GraphBasedFsa
open System

let transform x = (x, match x with |Smbl(y:char, _) when y <> (char 65535) -> Smbl(int y) |Smbl(y:char, _) when y = (char 65535) -> Smbl 65535 |_ -> Eps)
let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)
     
let TokenizationTest graph eCount vCount tagToToken =
    let graphFsa = approximateQG(graph)
    let graphFst = FST<_,_>.FSAtoFST(graphFsa, transform, smblEOF)
    let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphFst tagToToken
    match res with
    | Success res -> 
                //ToDot res @"../../../src/AbstractLexer.Interpreter.Tests/Tests/TestInterpretParser.dot" (printBref printSmbString)
                checkGraph res eCount vCount   
    | Error e -> Assert.Fail(sprintf "Tokenization problem %A:" e)
 
[<TestFixture>]
type ``Lexer FST Tests`` () =            
    [<Test>]
    member this.``Lexer FST Tests. Check composition with lexer.`` () = 
        let graph = new BidirectionalGraph<_,_>()
        graph.AddVerticesAndEdge(new TaggedEdge<_,_>(0, 1, ("+", "+"))) |> ignore
        graph.AddVerticesAndEdge(new TaggedEdge<_,_>(1, 2, ("*", "*"))) |> ignore
        graph.AddVerticesAndEdge(new TaggedEdge<_,_>(2, 1, ("*", "*"))) |> ignore
        graph.AddVerticesAndEdge(new TaggedEdge<_,_>(1, 3, ("*", "*"))) |> ignore
        TokenizationTest graph 6 5 

    [<Test>]
    member this.``Lexer FST Tests. Check work of interpreter.`` () = 
        let graph = new BidirectionalGraph<_,_>()
        graph.AddVerticesAndEdge(new TaggedEdge<_,_>(0, 1, ("1", "1"))) |> ignore
        graph.AddVerticesAndEdge(new TaggedEdge<_,_>(0, 1, ("2", "2"))) |> ignore
        graph.AddVerticesAndEdge(new TaggedEdge<_,_>(0, 1, ("3", "3"))) |> ignore
        graph.AddVerticesAndEdge(new TaggedEdge<_,_>(1, 2, ("4", "4"))) |> ignore
        graph.AddVerticesAndEdge(new TaggedEdge<_,_>(1, 2, ("5", "5"))) |> ignore
        graph.AddVerticesAndEdge(new TaggedEdge<_,_>(1, 3, ("-", "-"))) |> ignore
        graph.AddVerticesAndEdge(new TaggedEdge<_,_>(2, 3, ("+", "+"))) |> ignore
        graph.AddVerticesAndEdge(new TaggedEdge<_,_>(3, 4, ("6", "6"))) |> ignore
        TokenizationTest graph 7 7

//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer FST Tests`` () 
//      let a = t.``Lexer FST Tests. Check composition with lexer.``()
//      printfn "%A" a      
//      1