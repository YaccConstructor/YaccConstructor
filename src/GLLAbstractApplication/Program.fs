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
open Yard.Generators.GLL.AbstractParser

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
let inputGraph = loadLexerInputGraph "input.dot"

let parser = GLL.AbstractParse.SimpleAmb.buildAbstractAst
let run2 astBuilder = astBuilder inputGraph
let r = run2 parser

match r with
| AbstractParser.Error _ ->
    printfn "Error"     
| AbstractParser.Success tree->
    printfn "%s" "sss"
    

