
module JSONParser.Test

open Graphviz4Net.Dot.AntlrParser
open Graphviz4Net.Dot
open QuickGraph
open NUnit.Framework
open AbstractLexer.Common
open AbstractLexer.Core
open QuickGraph.Algorithms
//open AbstractLexer.Test.Calc.Parser
open QuickGraph.Algorithms
open QuickGraph.Graphviz
open JSON.Parser
open System.IO
//open QuickGraph.Algorithms


let loadGraphFromDOT filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let baseInputGraphsPath = "../jsonParser.Test"

[<TestFixture>]
type ``Abstract lexer tests`` () =    

    
    let path name = System.IO.Path.Combine(baseInputGraphsPath,name)

    let loadDotToQG gFile =
        let g = loadGraphFromDOT(path gFile)
        let qGraph = new AdjacencyGraph<int, TaggedEdge<_,string>>()
        g.Edges 
        |> Seq.iter(
            fun e -> 
                let edg = e :?> DotEdge<string>
                qGraph.AddVertex(int edg.Source.Id) |> ignore
                qGraph.AddVertex(int edg.Destination.Id) |> ignore
                qGraph.AddEdge(new TaggedEdge<_,_>(int edg.Source.Id,int edg.Destination.Id,edg.Label)) |> ignore)
        qGraph

    let loadLexerInputGraph gFile =
        let qGraph = loadDotToQG gFile
        let lexerInputG = new LexerInputGraph<_>()
        lexerInputG.StartVertex <- 0
        for e in qGraph.Edges do 
            lexerInputG.AddEdgeForsed (new LexerEdge<_,_>(e.Source,e.Target,Some(e.Tag.Replace('%','"'), Unchecked.defaultof<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)))
        lexerInputG

    [<Test>]
    member this.``Load graph test from DOT`` () =
        let g = loadLexerInputGraph(path "JSON_test_1.dot")
        let tRes = YC.ReSharper.AbstractAnalysis.Languages.JSON.tokenize g
        let pRes = YC.ReSharper.AbstractAnalysis.Languages.JSON.parse tRes
        ()


[<EntryPoint>]
let f x =
      let t = new ``Abstract lexer tests`` () 
      t.``Load graph test from DOT``()
      //let t = Literals.Lexer222.token <| Lexing.LexBuffer<_>.FromString ( "+1+")
      //printfn "%A" t
      1