module YC.FST.AbstractLexing.Tests.Calc

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open AbstractAnalysis.Common
open Graphviz4Net.Dot.AntlrParser
open Graphviz4Net.Dot
open YC.FST.GraphBasedFst
open YC.FST.FstApproximation
open YC.FST.AbstractLexing.Interpreter
open YC.FST.AbstractLexing.Tests.CommonTestChecker
open System.IO

  
let baseInputGraphsPath = "../../../../Tests/AbstractLexing/DOT" 
let path baseInputGraphsPath name = System.IO.Path.Combine(baseInputGraphsPath,name)
  
let loadGraphFromDOT filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let loadDotToQG baseInputGraphsPath gFile =
    let qGraph = loadGraphFromDOT(path baseInputGraphsPath gFile) 
    let graphAppr = new Appr<_>()
    graphAppr.InitState <- ResizeArray.singleton 0

    for e in qGraph.Edges do 
        let edg = e :?> DotEdge<string>
        new TaggedEdge<_,_>(int edg.Source.Id, int edg.Destination.Id, (Smb(edg.Label, edg.Label))) |> graphAppr.AddVerticesAndEdge |> ignore   

    graphAppr.FinalState <- ResizeArray.singleton (Seq.max graphAppr.Vertices)
    graphAppr

                     
[<TestFixture>]
type ``Lexer Calc Fst Tests`` () =            
    [<Test>]
    member this.``Load graph test from DOT`` () =
        let g = loadDotToQG baseInputGraphsPath "test_00.dot"
        checkGraph g 4 4

    [<Test>]
    member this.``Calc. Simple number.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_0.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 2 3        

    [<Test>]
    member this.``Test with position.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_0.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 2 3
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0|]
        checkArr endPos [|2|]
        checkArr backref [|"12"|]

    [<Test>]
    member this.``Test with position. Ident on two edgs`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_1.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 2 3 
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0|]
        checkArr endPos [|2; 1|]
        checkArr backref [|"12"; "3"|]       

    [<Test>]
    member this.``Test with position. Ident on edgs with branch`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_2.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 3 3
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0|]
        checkArr endPos [|2; 1|]
        checkArr backref [|"12"; "3"|]  

[<EntryPoint>]
let f x =
      let t = new ``Lexer Calc Fst Tests`` () 
      let a = t.``Test with position. Ident on edgs with branch``()
      //printfn "%A" a      
      1