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
open GLL.SimpleAmb
open Yard.Generators.GLL.AbstractParser

let outDir = @"../../../src/GLLAbstractApplication/"

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

let lbl tokenId = tokenId
let edg f t l = new ParserEdge<_>(f,t,lbl l)

let inputGraph =
    let qGraph = new ParserInputGraph<_>([|0|], [|3; 1|])
    qGraph.AddVerticesAndEdgeRange
        [edg 0 1 (GLL.SimpleAmb.NUM 1)
         edg 1 2 (GLL.SimpleAmb.PLUS 2)
         edg 2 3 (GLL.SimpleAmb.NUM 3)
         ] |> ignore
    qGraph


let parser = GLL.SimpleAmb.buildAbstractAst
  
let r = parser inputGraph

match r with
| AbstractParser.Error _ ->
    printfn "Error"     
| AbstractParser.Success tree->
    GLL.SimpleAmb.defaultAstToDot tree "ast334.dot"
    printfn "%s" "sss"
    

