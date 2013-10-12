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

let eofToken = AbstractLexer.Test.Calc.Parser.RNGLR_EOF ("",[||])
let literalEofToken = AbstractLexer.Test.Literals.Parser.RNGLR_EOF

let baseInputGraphsPath = "../../../../Tests/AbstractLexing/DOT"

[<TestFixture>]
type ``Abstract lexer tests`` () =    

//    let printG res (fName:string) =
//        let f = GraphvizAlgorithm(res)
//        let printEdg (e:AbstractParsing.Common.ParserEdge<_>) =
//            let printBrs brs =
//                "["
//                + (brs |> Array.map (fun (pos:Position<_>) -> pos.back_ref ) |> String.concat "; ")
//                + "]"
//            match e.Tag with
//            | Some (NUMBER(v,br)) -> "NUM: " + v + "; br= " + printBrs br
//            | Some (PLUS(v,br))   -> "+: " + v  + printBrs br
//            | Some (MULT(v,br))   ->  "*: " + v  + printBrs br
//            | Some (DIV(v,br))   ->  "/: " + v  + printBrs br
//            | Some (LBRACE(v,br))   ->  "(: " + v  + printBrs br
//            | Some (RBRACE(v,br))   ->  "): " + v  + printBrs br
//            | None -> "None"
//            | e -> string e 
//        f.FormatEdge.Add(fun e -> (e.EdgeFormatter.Label.Value <- printEdg e.Edge))
//        let str = f.Generate()
//        let c = System.IO.Path.GetInvalidFileNameChars()
//        let fName1 = c |> Array.fold (
//                                       fun (name:string) ch -> name.Replace(ch,'_')) fName
//        System.IO.File.WriteAllText(@"../../" + fName1 + ".dot" ,str)

    let printG res (fName:string) =
        let f = GraphvizAlgorithm(res)
        let printEdg (e:AbstractParsing.Common.ParserEdge<_>) =
            let printBrs brs =
                "["
                + (brs |> Array.map (fun (pos:Position<_>) -> pos.back_ref ) |> String.concat "; ")
                + "]"
            match e.Tag with
            | NUMBER(v,br) -> "NUM: " + v + "; br= " + printBrs br
            | PLUS(v,br)   -> "+: " + v  + printBrs br
            | MULT(v,br)   ->  "*: " + v  + printBrs br
            | DIV(v,br)   ->  "/: " + v  + printBrs br
            | LBRACE(v,br)   ->  "(: " + v  + printBrs br
            | RBRACE(v,br)   ->  "): " + v  + printBrs br
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
        for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new LexerEdge<_,_>(e.Source,e.Target,Some(e.Tag, e.Tag+"|")))
        lexerInputG

    let check_brs = 
       Seq.iter
        (fun (e:AbstractParsing.Common.ParserEdge<_>) -> 
                match e.Tag with
                | NUMBER (n,brs) 
                | PLUS (n,brs) ->
                    Assert.AreEqual(brs.Length, n.Length)
                    Assert.IsTrue(brs |> Array.map (fun i -> i.back_ref)|>Array.forall((=) (n + "|")))
                | RNGLR_EOF _ -> () 
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
        for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new LexerEdge<_,_>(e.Source,e.Target,Some(e.Tag, e.Tag)))
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
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)

    [<Test>]
    member this.``Test with position.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_with_pos_0.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)
        res.Edges 
          |> Seq.iter
              (fun e -> 
                match e.Tag with
                | NUMBER (n,brs)->
                    Assert.AreEqual(brs.Length,n.Length)
                    //Assert.IsTrue(brs |> Array.forall (fun x -> x.back_ref |> Option.isSome))
                    let brs' = brs |> Array.map (fun i -> i.back_ref)
                    Assert.IsTrue(brs'.[0] = "12|")
                    Assert.IsTrue(brs'.[1] = "12|")
                    let pos' = brs |> Array.map (fun i -> i.pos_cnum)
                    Assert.IsTrue(pos'.[0] = 0)
                    Assert.IsTrue(pos'.[1] = 1)
                | RNGLR_EOF _ -> ()
                | t -> Assert.Fail(sprintf "Unexpected token: %A" t)) 

    [<Test>]
    member this.``Test with position. Ident on two edgs`` () =
        let lexerInputGraph = loadLexerInputGraph "test_with_pos_1.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)
        res.Edges 
          |> Seq.iter
              (fun e -> 
                match e.Tag with
                | NUMBER (n,brs)->
                    Assert.AreEqual(brs.Length,n.Length)
                    //Assert.IsTrue(brs |> Array.forall (fun x -> x.back_ref |> Option.isSome))
                    let brs' = brs |> Array.map (fun i -> i.back_ref)
                    Assert.IsTrue(brs'.[0] = "12|")
                    Assert.IsTrue(brs'.[1] = "12|")
                    Assert.IsTrue(brs'.[2] = "3|")
                    let pos' = brs |> Array.map (fun i -> i.pos_cnum)
                    Assert.IsTrue(pos'.[0] = 0)
                    Assert.IsTrue(pos'.[1] = 1)
                    Assert.IsTrue(pos'.[2] = 0)
                | RNGLR_EOF _ -> ()
                | t -> Assert.Fail(sprintf "Unexpected token: %A" t)) 

    [<Test>]
    member this.``Test with position. Ident on edgs with branch`` () =
        let lexerInputGraph = loadLexerInputGraph "test_with_pos_2.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)
        res.Edges 
          |> Seq.iter
              (fun e -> 
                match e.Tag with
                | NUMBER (n,brs)->
                    Assert.AreEqual(brs.Length,n.Length)
                    //Assert.IsTrue(brs |> Array.forall (fun x -> x.back_ref |> Option.isSome))
                    let brs' = brs |> Array.map (fun i -> i.back_ref)
                    Assert.IsTrue(brs'.[0] = "12|")
                    Assert.IsTrue(brs'.[1] = "12|")
                    Assert.IsTrue(brs'.[2] = "3|" || brs'.[2] = "4|")
                    let pos' = brs |> Array.map (fun i -> i.pos_cnum)
                    Assert.IsTrue(pos'.[0] = 0)
                    Assert.IsTrue(pos'.[1] = 1)
                    Assert.IsTrue(pos'.[2] = 0)
                | RNGLR_EOF _ -> ()
                | t -> Assert.Fail(sprintf "Unexpected token: %A" t)) 

    [<Test>]
    member this.``Test with position. Ident and plus on edgs with branch`` () =
        let lexerInputGraph = loadLexerInputGraph "test_with_pos_3.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)
        printG res "test_with_pos_3_res"
        let positons =
            res.Edges 
              |> Seq.collect
                  (fun e -> 
                    match e.Tag with
                    | NUMBER (n,brs)
                    | PLUS (n,brs)->
                        brs |> Array.map (fun p -> p.pos_cnum)
                    | RNGLR_EOF _ -> [||]
                    | t -> failwith (sprintf "Unexpected token: %A" t))
            |> Array.ofSeq
        Assert.AreEqual(positons.Length,6)
        Assert.IsTrue(positons.[0] = 0)
        Assert.IsTrue(positons.[1] = 1)
        Assert.IsTrue(positons.[2] = 0)
        Assert.IsTrue(positons.[3] = 0)
        Assert.IsTrue(positons.[4] = 1)
        Assert.IsTrue(positons.[5] = 0)  

    [<Test>]
    member this.``Test with position. Ident on edgs with branch in begin.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_with_pos_4.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)
        res.Edges 
          |> Seq.iter
              (fun e -> 
                match e.Tag with
                | NUMBER (n,brs)->
                    Assert.AreEqual(brs.Length,n.Length)
                    let pos' = brs |> Array.map (fun i -> i.pos_cnum)
                    Assert.IsTrue(pos'.[0] = 0)
                    Assert.IsTrue(pos'.[1] = 0)
                | RNGLR_EOF _ -> ()
                | t -> Assert.Fail(sprintf "Unexpected token: %A" t)) 

    [<Test>]
    member this.``Test with position. Ident on edgs with branch in begin_1.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_with_pos_5.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)
        let positons =
            res.Edges 
              |> Seq.collect
                  (fun e -> 
                    match e.Tag with
                    | NUMBER (n,brs)->
                        brs |> Array.map (fun p -> p.pos_cnum)
                    | RNGLR_EOF _ -> [||]
                    | t -> failwith (sprintf "Unexpected token: %A" t))
            |> Array.ofSeq
        Assert.AreEqual(positons.Length,5)
        Assert.IsTrue(positons.[0] = 0)
        Assert.IsTrue(positons.[1] = 0)
        Assert.IsTrue(positons.[2] = 0)
        Assert.IsTrue(positons.[3] = 0)
        Assert.IsTrue(positons.[4] = 0)

    //[<Test>]
    member this.``Positions. Simple binop.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_with_pos_6.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        printfn "1"
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        printfn "2"
        Assert.AreEqual(res.Vertices |> Seq.length, 5)
        printfn "3"
        let positons =
            res.Edges 
              |> Seq.collect
                  (fun e -> 
                    match e.Tag with
                    | NUMBER (n,brs)
                    | MULT (n,brs) ->
                        brs |> Array.map (fun p -> p.pos_cnum)
                    | RNGLR_EOF _ -> [||]
                    | t -> failwith (sprintf "Unexpected token: %A" t))
            |> Array.ofSeq
        positons |> printfn "pos=%A"
        Assert.AreEqual(positons.Length,3)
        Assert.AreEqual(0, positons.[0])
        Assert.AreEqual(1, positons.[1])
        Assert.AreEqual(2, positons.[2])


    [<Test>]
    member this.``Calc. Simple sum.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_1.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Calc. Simple sum. Check back refs.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_1.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)
        check_brs res.Edges

    [<Test>]
    member this.``Calc. Start from PLUS.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_2.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc. Two-digit numbers sum.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_3.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Calc. Two-digit numbers sum. Check back refs.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_3.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)
        check_brs res.Edges

    [<Test>]
    member this.``Multi-digit with branch.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_14.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)
        res.Edges 
          |> Seq.iter
              (fun e -> 
                match e.Tag with
                | NUMBER (n,brs)
                | PLUS (n,brs)->
                //n.EndsWith("5") 
                    Assert.AreEqual(brs.Length,n.Length)
                    //Assert.IsTrue(brs |> Array.forall (fun x -> x.back_ref |> Option.isSome))
                    let brs' = brs |> Array.map (fun i -> i.back_ref)
                    Assert.IsTrue(brs'.[0] = "12|")
                    Assert.IsTrue(brs'.[1] = "12|")
                    Assert.IsTrue(brs'.[2] = string n.[2] + "|")
                | RNGLR_EOF _ -> ()
                | t -> Assert.Fail(sprintf "Unexpected token: %A" t)) 
               
    [<Test>]
    member this.``Print info on edges.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_15.dot"
        let ig = LexerInnerGraph lexerInputGraph
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        let prSeq = seq { for v in ig.TopologicalSort() do
                              for e in ig.OutEdges v do
                                  yield e.Label
                        }
        Assert.AreEqual(res.Edges |> Seq.length, 7)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)
        //printfn "%A" prSeq
        for x in prSeq do
            printfn "%A" x

    [<Test>]
    member this.``Calc. Branched multy-digit numbers.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_4_1.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)

    [<Test>]
    member this.``Calc. Branched multy-digit numbers with Binop.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_4_2.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc. Branched multy-digit numbers sum 1.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_4_3.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 5)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Calc. Branched multy-digit numbers sum 2.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_4_4.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 6)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Calc. Branched binop.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_5.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 5)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Calc. Branched binop or negation.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_6.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 5)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Calc. Complex branched 1.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_7.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken, printG)
//        let f = GraphvizAlgorithm(res)
//        let printEdg (e:LexerEdge<_,_>) =
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
        
        Assert.AreEqual(res.Edges |> Seq.length, 8)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Calc. Complex branched 2.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_8.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 5)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Calc. Complex branched 3.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_9.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 7)
        Assert.AreEqual(res.Vertices |> Seq.length, 7)

    [<Test>]
    member this.``Calc. Complex 0`` () =
        let lexerInputGraph = loadLexerInputGraph "test_12.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        printG res "../../out.dot"
        Assert.AreEqual(res.Edges |> Seq.length, 6, "Wrong edges count")
        Assert.AreEqual(res.Vertices |> Seq.length, 6, "Wrong verticies count")

    [<Test>]
    member this.``Calc. test 100`` () =
        let lexerInputGraph = loadLexerInputGraph "test_100.dot"
        let start = System.DateTime.Now
        for i in 1..1000 do Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken) |> ignore
        printfn "Time = %A" ((System.DateTime.Now - start).TotalMilliseconds / 1000.0)
        Assert.True(true)

    [<Test>]
    member this.``Calc. Epsilon edge 1.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_10_1.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc. Epsilon edge 2.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_10_2.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Literals. Simple.`` () =
        let lexerInputGraph = loadLexerInputGraph "literals_simple.dot"
        let res = Literals.Lexer._fslex_tables.Tokenize(Literals.Lexer.fslex_actions_token, lexerInputGraph, literalEofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)

    [<Test>]
    member this.``Literals. Inner branch.`` () =
        let lexerInputGraph = loadLexerInputGraph "literals_inner_branch.dot"
        let res = Literals.Lexer._fslex_tables.Tokenize(Literals.Lexer.fslex_actions_token, lexerInputGraph, literalEofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)

    [<Test>]
    member this.``Literals. Outer branch.`` () =
        let lexerInputGraph = loadLexerInputGraph "literals_outer_branch.dot"
        let res = Literals.Lexer._fslex_tables.Tokenize(Literals.Lexer.fslex_actions_token, lexerInputGraph, literalEofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)

    [<Test>]
    member this.``Literals. Splitted.`` () =
        let lexerInputGraph = loadLexerInputGraph "literals_splitted.dot"
        let res = Literals.Lexer._fslex_tables.Tokenize(Literals.Lexer.fslex_actions_token, lexerInputGraph, literalEofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)

    [<Test>]
    member this.``Test with space and idents on edg.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_with_space_0.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 3)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Test with space with branch.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_with_space_1.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)
        printG res "test_with_space_1"

    //[<Test>]
    member this.``Test with space at the end of previous tokens at the end of branch.`` () =
        let lexerInputGraph = loadLexerInputGraph "test_with_space_at_end_of_prev_token.dot."
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        printG res "test_with_space_at_end_of_prev_token"
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Calc with braces.`` () =
        let lexerInputGraph = loadLexerInputGraph "calc_1.dot."
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        printG res "calc_1_res"
        Assert.AreEqual(res.Edges |> Seq.length, 10)
        Assert.AreEqual(res.Vertices |> Seq.length, 10)

    [<Test>]
    member this.``Calc with braces 2.`` () =
        let lexerInputGraph = loadLexerInputGraph "calc_0.dot."
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        printG res "calc_0_res"
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Example with eps.`` () =
        let lexerInputGraph = loadLexerInputGraph "example_eps.dot."
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        printG res "example_eps"
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)
        
    [<Test>]
    member this.``Eps_closure_1.`` () =
        let lexerInputGraph = loadLexerInputGraph "eps_closure_1.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        printG res "eps_closure_1"
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Eps_closure_2.`` () =
        let lexerInputGraph = loadLexerInputGraph "eps_closure_2.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        printG res "eps_res_closure_2"
        Assert.AreEqual(res.Edges |> Seq.length, 5)
        Assert.AreEqual(res.Vertices |> Seq.length, 5)

    [<Test>]
    member this.``Eps_closure_3.`` () =
        let lexerInputGraph = loadLexerInputGraph "eps_closure_3.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        //printG res "eps_closure_3"
        //let eps_res = EpsClosure.NfaToDfa res
        //printGEps eps_res "eps_res_closure_3"
        //Assert.AreEqual(res.Edges |> Seq.length, 2)
        //Assert.AreEqual(res.Vertices |> Seq.length, 2)
        //Assert.AreEqual(eps_res.Edges |> Seq.length, 1)
        //Assert.AreEqual(eps_res.Vertices |> Seq.length, 2)
        printG res "eps_res_closure_3"
        Assert.AreEqual(res.Edges |> Seq.length, 2)
        Assert.AreEqual(res.Vertices |> Seq.length, 3)

    [<Test>]
    member this.``Eps_closure_4.`` () =
        let lexerInputGraph = loadLexerInputGraph "eps_closure_4.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        printG res "eps_res_closure_4"
        Assert.AreEqual(res.Edges |> Seq.length, 4)
        Assert.AreEqual(res.Vertices |> Seq.length, 4)

    [<Test>]
    member this.``Eps_closure_5.`` () =
        let lexerInputGraph = loadLexerInputGraph "eps_closure_5.dot"
        let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eofToken)
        printG res "eps_res_closure_5"
        Assert.AreEqual(res.Edges |> Seq.length, 1)
        Assert.AreEqual(res.Vertices |> Seq.length, 2)
        Assert.AreEqual(eofToken, (res.Edges |> Seq.nth 0).Tag)

//[<EntryPoint>]
//let f x =
//    
//    let t = new ``Abstract lexer tests`` () 
//    t.``Literals. Simple.``()
//    let t = Literals.Lexer222.token <| Lexing.LexBuffer<_>.FromString ( "+1+")
//    printfn "%A" t
//    1

//[<EntryPoint>]
//let f x =
//      let t = new ``Abstract lexer tests`` () 
//      t.``Positions. Simple binop.``()
//      //``Test with space at the end of previous tokens at the end of branch.``()
//      //let t = Literals.Lexer222.token <| Lexing.LexBuffer<_>.FromString ( "+1+")
//     // printfn "%A" t
//      1