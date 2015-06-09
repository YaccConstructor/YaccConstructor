module YC.TSQL.Test

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open AbstractAnalysis.Common
open YC.FST.GraphBasedFst
open YC.FST.AbstractLexing.Interpreter
open YC.FST.AbstractLexing.Tests.CommonTestChecker
open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation
open Yard.Examples.MSParser
open Graphviz4Net.Dot.AntlrParser
open Graphviz4Net.Dot

let baseInputGraphsPath = "../../../src/TSQL.Test/DotTSQL"

let transform x = (x, match x with |Smbl(y, _) -> Smbl y |_ -> Eps)
let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)

let equalSmbl x y = (fst x) = (fst y)

let getChar x = 
    match x with
    | Smbl(y, _) -> y
    | _ -> failwith "Unexpected symb in alphabet of FSA!"

let newSmb x =  Smbl(x, Unchecked.defaultof<_>)

let printBref =       
    let printGr (gr:FSA<_>) = 
        let strs = ref ""
        for edge in gr.Edges do
            strs := !strs + "[" + (match edge.Tag with |Smbl (x:char*Position<_>)  -> (fst x).ToString() |_ -> "") + "];"
        !strs       
             
    fun x ->
        match x with
            | Yard.Examples.MSParser.DEC_NUMBER(gr) -> "NUM: " + printGr gr
            | Yard.Examples.MSParser.IDENT(gr) -> "IDENT: " + printGr gr
            | Yard.Examples.MSParser.L_from(gr) -> "FROM: " + printGr gr
            | Yard.Examples.MSParser.L_select(gr) -> "SELECT: " + printGr gr
            | x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0]) 

let loadDotToQGReSharper baseInputGraphsPath gFile =
    let qGraph = loadGraphFromDOT(path baseInputGraphsPath gFile)
    let graphAppr = new Appr<_>()
    graphAppr.InitState <- ResizeArray.singleton 0

    for e in qGraph.Edges do
        let edg = e :?> DotEdge<string>
        new TaggedEdge<_,_>(int edg.Source.Id, int edg.Destination.Id, (edg.Label, Unchecked.defaultof<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)) |> graphAppr.AddVerticesAndEdge |> ignore

    graphAppr.FinalState <- ResizeArray.singleton (Seq.max graphAppr.Vertices)
    graphAppr
      
let TSQLTokenizationTest path eCount vCount =
    let graphAppr = loadDotToQGReSharper baseInputGraphsPath path    
    let graphFsa = graphAppr.ApprToFSA()
    //graphFsa.PrintToDOT <| @"../../../src/TSQL.Test/DotTSQL/test2.dot"
    let graphFst = FST<_,_>.FSAtoFST(graphFsa, transform, smblEOF)
    let res = YC.TSQLLexer.tokenize (Yard.Examples.MSParser.RNGLR_EOF(new FSA<_>())) graphFst
    match res with
    | Success res -> 
        ToDot res @"../../../src/TSQL.Test/DotTSQL/test1.dot" printBref
        checkGraph res eCount vCount  
    | Error e -> Assert.Fail(sprintf "Tokenization problem in test %s: %A" path e)

[<TestFixture>]
type ``Lexer and Parser TSQL Tests`` () =   
    [<Test>]  
    member this.``TSQL. Simple.`` () =
        TSQLTokenizationTest "test_tsql_1.dot" 5 6 

//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer and Parser TSQL Tests`` () 
//      t.``TSQL. Simple.``()
//      1