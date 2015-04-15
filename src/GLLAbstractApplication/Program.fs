module GLLAbstractApplication
open System.IO
open System
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
open Graphviz4Net.Dot.AntlrParser
open System.IO
open Graphviz4Net.Dot
open QuickGraph
open NUnit.Framework
open AbstractAnalysis.Common
open YC.Tests.Helper
open Yard.Generators.GLL
open Yard.Generators.GLL.AbstractParser 
open Yard.Frontends.YardFrontend
open Yard.Generators.GLL
open GLL.AbstractParse.SimpleAmb

let run () =
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let gen = new Yard.Generators.GLL.GLL()
    let il = ref <| fe.ParseGrammar(@"C:\Users\User\recursive-ascent\src\GLLAbstractApplication\SimpleAmb.yrd")
    for constr in gen.Constraints do
        let grammar = il.Value.grammar
        if not <| constr.Check grammar then
            eprintfn "Constraint %s: applying %s..." constr.Name constr.Conversion.Name
            il := {!il with grammar = constr.Fix grammar}

    gen.Generate(!il,"-pos int -token int -abstract true -o SimpleAmb.yrd.fs")

run () |> printfn "%A"

let baseInputGraphsPath = "../../../Tests/AbstractRNGLR/DOT"

let path name = path baseInputGraphsPath name

let lbl tokenId = tokenId
let edg f t l = new ParserEdge<_>(f,t,lbl l)
let loadLexerInputGraph gFile =
    let qGraph = loadDotToQG baseInputGraphsPath gFile
    let lexerInputG = new LexerInputGraph<_>()
    lexerInputG.StartVertex <- 0
    for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new LexerEdge<_,_>(e.Source,e.Target,Some (e.Tag, e.Tag)))
    lexerInputG

let parser = GLL.AbstractParse.SimpleAmb.buildAbstractAst
let run = 
printfn "%A" r
match r with
| Error (num, tok, message) ->
    printfn "Error in position %d on Token %A: %s" num tok message
    Assert.Fail "!!!!!!"
| Success(tree) ->
    tree.PrintAst()
    
let qGraph = new ParserInputGraph<_>(0, 4)
    qGraph.AddVerticesAndEdgeRange
        [edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
            edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
            edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
            edg 3 4 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
            ] |> ignore

RNGLR.PrettySimpleCalc.buildAstAbstract qGraph 13 12 0 3 0

//let run1 astBuilder =
//    let lb = LexBuffer<_>.FromString "1 - 2"
//    let tokens = seq{while not lb.IsPastEndOfStream do yield Calc.Lexer.token lb}
//    astBuilder tokens, tokens


//let parser1 = GLL.Parse.SimpleAmb.buildAst
//
////for i in [1..10] do
////    let str = String.init (i * 5) (fun i -> "B ")
////    let start = System.DateTime.Now
////    let r = run1 (str.Trim()) parser1
////    let t = System.DateTime.Now - start
////    printfn "%A" t.TotalSeconds
//
////////////////////////////////////////
//let run2 path astBuilder =
//    let tokens = Lexer2.tokens1(path)
//    astBuilder tokens, tokens
////
////let parser2 = GLL.Parse.SimpleAmb.buildAst
////let str = String.init 1 (fun i -> "B ") + "B"
//let str = "A B"
//let r = run2 str parser1
////printfn "simple amb"
//
//match r with
//    | Parser.Error str, _ ->
//        printfn "%s" "dddd" //str 
//    | Parser.Success tree, tokens ->
//        printfn "%s" "sss"
//        //GLL.SimpleLeftRecursion.defaultAstToDot tree "ast.dot"
//////
////printfn "ff"
