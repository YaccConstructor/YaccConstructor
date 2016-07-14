module YC.TSQL.Test

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open AbstractAnalysis.Common
open Yard.Utils.StructClass
open QuickGraph.FST.GraphBasedFst
open YC.FST.AbstractLexing.Interpreter
open YC.FST.AbstractLexing.Tests.CommonTestChecker
open QuickGraph.FSA.GraphBasedFsa
open Yard.Examples.MSParserAbstract
open Graphviz4Net.Dot.AntlrParser
open Graphviz4Net.Dot
open Yard.Generators.RNGLR.AbstractParser
open Yard.Generators.ARNGLR.Parser
open RNGLRAbstractParserTests
open System
open System.Collections.Generic
open System.IO

let baseInputGraphsPath = "../../../src/TSQL.Test/DotTSQL"

let transform x = (x, match x with |Smbl(y:char, _) when y <> (char 65535) -> Smbl(int <| Convert.ToUInt32(y)) |Smbl(y:char, _) when y = (char 65535) -> Smbl 65535 |_ -> Eps)
let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)
let equalSmbl x y = (fst x) = (fst y)
let newSmb x =  Smbl(x, Unchecked.defaultof<_>)
let path baseInputGraphsPath name = System.IO.Path.Combine(baseInputGraphsPath,name)
let br = Unchecked.defaultof<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>

let getChar x =
    match x with
    | Smbl(y, _) -> y
    | _ -> failwith "Unexpected symb in alphabet of FSA!"

let printBref =
    fun x ->
        match x with
            | Yard.Examples.MSParserAbstract.DEC_NUMBER(gr) -> "NUM"
            | Yard.Examples.MSParserAbstract.IDENT(gr) -> "IDENT"
            | Yard.Examples.MSParserAbstract.L_from(gr) -> "FROM"
            | Yard.Examples.MSParserAbstract.L_select(gr) -> "SELECT"
            | Yard.Examples.MSParserAbstract.L_insert(gr) -> "INSERT"
            | Yard.Examples.MSParserAbstract.L_values(gr) -> "VALUES"
            | Yard.Examples.MSParserAbstract.L_left_bracket_(gr) -> "LEFT_BRACKET"
            | Yard.Examples.MSParserAbstract.L_right_bracket_(gr) -> "RIGHT_BRACKET"
            | Yard.Examples.MSParserAbstract.L_plus_(gr) -> "PLUS"
            | Yard.Examples.MSParserAbstract.L_more_(gr) -> "MORE"
            | Yard.Examples.MSParserAbstract.L_where(gr) -> "WHERE"
            | Yard.Examples.MSParserAbstract.L_comma_(gr) -> "COMMA"
            | Yard.Examples.MSParserAbstract.L_minus_(gr) -> "MINUS"
            | x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0]) 

let loadGraphToQGResharper path =
    let dot = File.ReadAllText(path)
    let vertexFunc = fun v attrs -> int v
    let edgeFunc = fun v1 v2 (attrs: IDictionary<string, string>) -> new TaggedEdge<_,_>(v1, v2, (attrs.Item("label"), br))
     
    BidirectionalGraph<_,_>.LoadDot(dot, new Func<_,_,_>(vertexFunc), new Func<_,_,_,_>(edgeFunc))
      
let TSQLTokenizationTest file eCount vCount =
    let graph = loadGraphToQGResharper (path baseInputGraphsPath file)
    let graphFsa = approximateQG(graph)
    let graphFst = FST<_,_>.FSAtoFST(graphFsa, transform, smblEOF)
    let res = YC.TSQLLexer.tokenize (Yard.Examples.MSParserAbstract.RNGLR_EOF(new FSA<_>())) graphFst
    match res with
    | QuickGraph.FST.GraphBasedFst.Test.Success res -> 
        checkGraph res eCount vCount  
    | QuickGraph.FST.GraphBasedFst.Test.Error e -> Assert.Fail(sprintf "Tokenization problem in test %s: %A" file e)

let test buildAstAbstract qGraph nodesCount edgesCount epsilonsCount termsCount ambiguityCount = 
    let r = (new Parser<_>()).Parse  buildAstAbstract qGraph
    //printfn "%A" r
    match r with
    | Yard.Generators.ARNGLR.Parser.Error (num, tok, message) ->
        printfn "Error in position %d on Token %A: %s" num tok message
        Assert.Fail "!!!!!!"
    | Yard.Generators.ARNGLR.Parser.Success(tree) ->
        //tree.PrintAst()
        let n, e, eps, t, amb = tree.CountCounters()
        //printfn "%A %A %A %A %A" n e eps t amb
        (*Assert.AreEqual(nodesCount, n, "Nodes count mismatch")
        Assert.AreEqual(edgesCount, e, "Edges count mismatch")
        Assert.AreEqual(epsilonsCount, eps, "Epsilons count mismatch")
        Assert.AreEqual(termsCount, t, "Terms count mismatch")
        Assert.AreEqual(ambiguityCount, amb, "Ambiguities count mismatch")*)
        Assert.IsTrue(n > 0, "0 nodes")
        Assert.IsTrue(e > 0, "0 edges")
        Assert.Pass()

[<TestFixture>]
type ``Lexer and Parser TSQL Tests`` () =   
    [<Test>]  
    member this.``TSQL. Lexer test 1.`` () =
        TSQLTokenizationTest "test_tsql_1.dot" 9 10

    [<Test>]  
    member this.``TSQL. Lexer test 2.`` () =
        TSQLTokenizationTest "test_tsql_2.dot" 18 19 

    [<Test>]  
    member this.``TSQL. Lexer and Parser test.`` () =
        let graph = loadGraphToQGResharper (path baseInputGraphsPath "test_tsql_1.dot")
        let graphFsa = approximateQG(graph)
        let graphFst = FST<_,_>.FSAtoFST(graphFsa, transform, smblEOF)
        let res = YC.TSQLLexer.tokenize (Yard.Examples.MSParserAbstract.RNGLR_EOF(new FSA<_>())) graphFst
        match res with
        | QuickGraph.FST.GraphBasedFst.Test.Success res -> 
            //ToDot res @"../../../src/TSQL.Test/DotTSQL/teeeest.dot" printBref
            checkGraph res 9 10
            test  Yard.Examples.MSParserAbstract.buildAstAbstract res 156 159 5 8 4
        | QuickGraph.FST.GraphBasedFst.Test.Error e -> Assert.Fail(sprintf "Tokenization problem in test %s: %A" "test_tsql_1.dot" e)

    [<Test>]
    member this.``TSQL. Parser test 1.`` () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [
             edg 0 1 (Yard.Examples.MSParserAbstract.L_select(new FSA<_>()))
             edg 1 2 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 2 3 (Yard.Examples.MSParserAbstract.L_from(new FSA<_>()))
             edg 3 4 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 4 5 (Yard.Examples.MSParserAbstract.L_where(new FSA<_>()))
             edg 5 6 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 6 7 (Yard.Examples.MSParserAbstract.L_more_(new FSA<_>()))
             edg 7 8 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 8 9 (Yard.Examples.MSParserAbstract.RNGLR_EOF(new FSA<_>()))
             ] |> ignore
        
        test Yard.Examples.MSParserAbstract.buildAstAbstract qGraph 156 159 5 8 4

    [<Test>]
    member this.``TSQL. Parser test 2.`` () =
        let qGraph = new ParserInputGraph<_>(0, 18)
        qGraph.AddVerticesAndEdgeRange
            [
             edg 0 1 (Yard.Examples.MSParserAbstract.L_insert(new FSA<_>()))
             edg 1 2 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 1 2 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 2 3 (Yard.Examples.MSParserAbstract.L_left_bracket_(new FSA<_>()))
             edg 3 4 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 4 5 (Yard.Examples.MSParserAbstract.L_comma_(new FSA<_>()))
             edg 5 6 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 6 7 (Yard.Examples.MSParserAbstract.L_right_bracket_(new FSA<_>()))
             edg 7 8 (Yard.Examples.MSParserAbstract.L_values(new FSA<_>()))
             edg 8 9 (Yard.Examples.MSParserAbstract.L_left_bracket_(new FSA<_>()))
             edg 9 10 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 10 11 (Yard.Examples.MSParserAbstract.L_plus_(new FSA<_>()))
             edg 11 12 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 12 13 (Yard.Examples.MSParserAbstract.L_comma_(new FSA<_>()))
             edg 13 14 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 14 15 (Yard.Examples.MSParserAbstract.L_minus_(new FSA<_>()))
             edg 15 16 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 16 17 (Yard.Examples.MSParserAbstract.L_right_bracket_(new FSA<_>()))          
             edg 17 18 (Yard.Examples.MSParserAbstract.RNGLR_EOF(new FSA<_>()))
             ] |> ignore
        
        test Yard.Examples.MSParserAbstract.buildAstAbstract qGraph 207 210 4 18 5

    [<Test>]
    member this.``TSQL. Parser test 3.`` () =
        let qGraph = new ParserInputGraph<_>(0, 15)
        qGraph.AddVerticesAndEdgeRange
            [
             edg 0 1 (Yard.Examples.MSParserAbstract.L_insert(new FSA<_>()))
             edg 1 2 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))             
             edg 2 3 (Yard.Examples.MSParserAbstract.L_left_bracket_(new FSA<_>()))
             edg 3 4 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 4 5 (Yard.Examples.MSParserAbstract.L_comma_(new FSA<_>()))
             edg 5 6 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 6 7 (Yard.Examples.MSParserAbstract.L_right_bracket_(new FSA<_>()))
             edg 7 8 (Yard.Examples.MSParserAbstract.L_values(new FSA<_>()))
             edg 8 9 (Yard.Examples.MSParserAbstract.L_left_bracket_(new FSA<_>()))
             edg 9 10 (Yard.Examples.MSParserAbstract.L_select(new FSA<_>()))
             edg 10 11 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 11 12 (Yard.Examples.MSParserAbstract.L_from(new FSA<_>()))
             edg 12 13 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 9 13 (Yard.Examples.MSParserAbstract.IDENT(new FSA<_>()))
             edg 13 14 (Yard.Examples.MSParserAbstract.L_right_bracket_(new FSA<_>()))
             edg 14 15 (Yard.Examples.MSParserAbstract.RNGLR_EOF(new FSA<_>()))
             ] |> ignore
        
        test Yard.Examples.MSParserAbstract.buildAstAbstract qGraph 201 203 8 15 3

(*[<EntryPoint>]
let f x =
      let t = new ``Lexer and Parser TSQL Tests`` () 
      t.``TSQL. Lexer and Parser test.``()
      1*)
