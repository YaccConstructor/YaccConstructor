module AbstractFsLex.Test

open Graphviz4Net.Dot.AntlrParser
open System.IO
open Graphviz4Net.Dot
open QuickGraph
open NUnit.Framework
open AbstractLexer.Common
open AbstractLexer.Core
open QuickGraph.Algorithms
open AbstractLexer.Test.Calc.Parser
open QuickGraph.Algorithms
open QuickGraph.Graphviz

let loadGraphFromDOT filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let baseInputGraphsPath = "../../../../Tests/AbstractLexing/DOT"

[<TestFixture>]
type ``Abstract lexer tests`` () =    

    let printG res (fName:string) =
        let f = GraphvizAlgorithm(res)
        let printEdg (e:AbstractParsing.Common.ParserEdge<_>) =
            let printBrs brs =
                "["
                + (brs |> Array.map (function Some x -> string x | None -> "None") |> String.concat "; ")
                + "]"
            match e.Tag with
            | NUMBER(v,br) -> "NUM: " + v + "; br= " + printBrs br
            | PLUS(v,br)   -> "+: " + v  + printBrs br
            | MULT(v,br)   ->  "*: " + v  + printBrs br
            | e -> string e 
        f.FormatEdge.Add(fun e -> (e.EdgeFormatter.Label.Value <- printEdg e.Edge))
        let str = f.Generate()
        let c = System.IO.Path.GetInvalidFileNameChars()
        let fName1 = c |> Array.fold (
                                       fun (name:string) ch -> name.Replace(ch,'_')) fName
        System.IO.File.WriteAllText(@"../../" + fName1 + ".dot" ,str)

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
        for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new AEdge<_,_>(e.Source,e.Target,(Some e.Tag, Some (e.Tag+"|"))))
        lexerInputG

    let check_brs = 
       Seq.iter
        (fun (e:AbstractParsing.Common.ParserEdge<_>) -> 
                match e.Tag with
                | NUMBER (n,brs) 
                | PLUS (n,brs)->
                //n.EndsWith("5") 
                    Assert.AreEqual(brs.Length,n.Length)
                    Assert.IsTrue(brs |> Array.forall Option.isSome)
                    Assert.IsTrue(brs |> Array.map (fun i -> i.Value)|>Array.forall((=) (n + "|")))
                | t -> Assert.Fail(sprintf "Unexpected token: %A" t) 
                )

    [<Test>]
    member this.``Load graph test from DOT`` () =
        let g = loadGraphFromDOT(path "test_00.dot")
        Assert.AreEqual(g.Edges |> Seq.length, 4)
        Assert.AreEqual(g.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Load graph test from DOT to QuickGraph`` () =
        let qGraph = loadDotToQG "test_00.dot"
        Assert.AreEqual(qGraph.Edges |> Seq.length, 4)
        Assert.AreEqual(qGraph.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Load graph test from DOT to lexer input graph`` () =
        let qGraph = loadDotToQG "test_00.dot"
        let lexerInputG = new LexerInputGraph<_>()
        lexerInputG.StartVertex <- 0
        for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new AEdge<_,_>(e.Source,e.Target,(Some e.Tag, Some e.Tag)))
        Assert.AreEqual(lexerInputG.Edges |> Seq.length, 4)
        Assert.AreEqual(lexerInputG.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Load graph test from DOT to lexer inner graph`` () =
        let lexerInnerGraph = new LexerInnerGraph<_>(loadLexerInputGraph "test_00.dot")
        Assert.AreEqual(lexerInnerGraph.StartVertex, 0)
        Assert.AreEqual(lexerInnerGraph.Edges |> Seq.length, 6)
        Assert.AreEqual(lexerInnerGraph.Vertices |> Seq.length, 6)

    [<Test>]
    member this.``Calc. Simple number.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_0.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 1)
        Assert.AreEqual(res.Vertices |> Seq.length, 2)

    [<Test>]
    member this.``Calc. Simple sum.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_1.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc. Simple sum. Check back refs.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_1.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)
        check_brs res.Edges

    [<Test>]
    member this.``Calc. Start from PLUS.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_2.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)

    [<Test>]
    member this.``Calc. Two-digit numbers sum.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_3.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc. Two-digit numbers sum. Check back refs.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_3.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)
        check_brs res.Edges

    [<Test>]
    member this.``Multi-digit with branch.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_14.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 2)
        res.Edges 
          |> Seq.iter
              (fun e -> 
                match e.Tag with
                | NUMBER (n,brs) 
                | PLUS (n,brs)->
                //n.EndsWith("5") 
                    Assert.AreEqual(brs.Length,n.Length)
                    Assert.IsTrue(brs |> Array.forall Option.isSome)
                    let brs' = brs |> Array.map (fun i -> i.Value)
                    Assert.IsTrue(brs'.[0] = "12|")
                    Assert.IsTrue(brs'.[1] = "12|")
                    Assert.IsTrue(brs'.[2] = string n.[2] + "|")
                | t -> Assert.Fail(sprintf "Unexpected token: %A" t)) 
               
    [<Test>]
    member this.``Print info on edges.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_15.dot"
        let ig = LexerInnerGraph lexerInputGraph
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        let prSeq = seq { for v in ig.TopologicalSort() do
                              for e in ig.OutEdges v do
                                  yield e.Label
                        }
        Assert.AreEqual(res.Edges |> Seq.length, 6)
        Assert.AreEqual(res.Vertices |> Seq.length, 2)
        //printfn "%A" prSeq
        for x in prSeq do
            printfn "%A" x

    [<Test>]
    member this.``Calc. Branched multy-digit numbers.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_4_1.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 2)

    [<Test>]
    member this.``Calc. Branched multy-digit numbers with Binop.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_4_2.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)

    [<Test>]
    member this.``Calc. Branched multy-digit numbers sum 1.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_4_3.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc. Branched multy-digit numbers sum 2.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_4_4.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 5)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc. Branched binop.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_5.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc. Branched binop or negation.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_6.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc. Complex branched 1.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_7.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, printG)
//        let f = GraphvizAlgorithm(res)
//        let printEdg (e:AEdge<_,_>) =
//            let printBrs brs =
//                "["
//                + (brs |> Array.map (function Some x -> string x | None -> "None") |> String.concat "; ")
//                + "]"
//            match e.Label.Value with
//            | NUMBER(v,br) -> "NUM: " + v + "; br= " + printBrs br
//            | PLUS(v,br)   -> "+: " + v  + printBrs br
//            | MULT(v,br)   ->  "*: " + v  + printBrs br
//            | e -> string e 
//        f.FormatEdge.Add(fun e -> (e.EdgeFormatter.Label.Value <- printEdg e.Edge))
//        let str = f.Generate()
        printG res @"../../res.dot"
        
        Assert.AreEqual(res.Edges |> Seq.length, 7)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc. Complex branched 2.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_8.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc. Complex branched 3.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_9.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 6)
        Assert.AreEqual(res.Vertices |> Seq.length, 6)

    [<Test>]
    member this.``Calc. Complex 0`` () =
        let lexerInputGraph = loadLexerInputGraph "test_12.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 5)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Calc. test 100`` () =
        let lexerInputGraph = loadLexerInputGraph "test_100.dot"
        let start = System.DateTime.Now
        for i in 1..1000 do Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph) |> ignore
        printfn "Time = %A" ((System.DateTime.Now - start).TotalMilliseconds / 1000.0)
        Assert.True(true)

    [<Test>]
    member this.``Calc. Epsilon edge 1.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_10_1.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 1)
        Assert.AreEqual(res.Vertices |> Seq.length, 2)

    [<Test>]
    member this.``Calc. Epsilon edge 2.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_10_2.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 2)

    [<Test>]
    member this.``Literals. Simple.`` () =
        let lexerInputGraph = loadLexerInputGraph "literals_simple.dot"
        let res = Literals.Lexer._fslex_tables.Tokenize(Literals.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 1)
        Assert.AreEqual(res.Vertices |> Seq.length, 2)

    [<Test>]
    member this.``Literals. Inner branch.`` () =
        let lexerInputGraph = loadLexerInputGraph "literals_inner_branch.dot"
        let res = Literals.Lexer._fslex_tables.Tokenize(Literals.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 2)

    [<Test>]
    member this.``Literals. Outer branch.`` () =
        let lexerInputGraph = loadLexerInputGraph "literals_outer_branch.dot"
        let res = Literals.Lexer._fslex_tables.Tokenize(Literals.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 2)

    [<Test>]
    member this.``Literals. Splitted.`` () =
        let lexerInputGraph = loadLexerInputGraph "literals_splitted.dot"
        let res = Literals.Lexer._fslex_tables.Tokenize(Literals.Lexer.fslex_actions_token, lexerInputGraph)
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 2)

//[<EntryPoint>]
//let f x =
//    
//    let t = new ``Abstract lexer tests`` () 
//    t.``Literals. Simple.``()
//    let t = Literals.Lexer222.token <| Lexing.LexBuffer<_>.FromString ( "+1+")
//    printfn "%A" t
//    1

[<EntryPoint>]
let f x =
      let t = new ``Abstract lexer tests`` () 
      t.``Calc. Complex branched 1.``()
      //let t = Literals.Lexer222.token <| Lexing.LexBuffer<_>.FromString ( "+1+")
     // printfn "%A" t
      1