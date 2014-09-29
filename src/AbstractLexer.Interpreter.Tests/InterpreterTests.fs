module YC.FST.AbstractLexing.Tests.Interpreter

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open AbstractAnalysis.Common
open YC.FST.GraphBasedFst
open YC.FST.FstApproximation
open YC.FST.AbstractLexing.Interpreter
open AbstractParser.Tokens

let checkGraph (graph:AdjacencyGraph<_,_>) countE countV  =
    Assert.AreEqual(graph.EdgeCount, countE, "Count of edges not equal expected number. ")
    Assert.AreEqual(graph.VertexCount, countV, "Count of vertices not equal expected number. ")

let printSmbString (x:char*Position<_>) = 
        (fst x).ToString() + "_br: " + (snd x).back_ref + "(" + (snd x).start_offset.ToString() + "," + (snd x).end_offset.ToString() + ")"

let eof = RNGLR_EOF(new GraphTokenValue<_>())    

let printBref =       
    let printGr (gr:GraphTokenValue<_>) = 
        printf "%A\n" gr.EdgeCount
        let strs = ref ""
        for edge in gr.Edges do
            strs := !strs + "[" + edge.Source.ToString() + ", " + 
                                  "{" + edge.Label.ToString() + "_br: " + edge.BackRef + "(" + edge.StartPos.ToString() + ", " + edge.EndPos.ToString() + ")"+ "}" +
                                  edge.Target.ToString() + "] ;"
        !strs        
             
    fun x ->
        match x with
            | NUMBER(gr) -> "NUM: " + printGr gr
            | MINUS(gr) -> "MINUS: " + printGr gr
            | LBRACE(gr) -> "LBRACE: " + printGr gr
            | RBRACE(gr) -> "RBRACE: " + printGr gr
            | DIV(gr) -> "DIV: " + printGr gr
            | PLUS(gr) -> "PLUS: "  + printGr gr
            | POW(gr)  -> "POW: "  + printGr gr
            | MULT(gr) -> "MULT: " + printGr gr
            | LITERAL(gr) -> "LITERAL: " + printGr gr
            | x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0])        
 
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