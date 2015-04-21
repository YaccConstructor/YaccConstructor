//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.


module GLLAbstractParserTests

open System.IO
open QuickGraph
open NUnit.Framework
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open YC.Tests.Helper
open Yard.Generators.Common.AST3
open GLL.AbstractParse.SimpleRightRecursion
open GLL.AbstractParse.BadLeftRecursion
open GLL.AbstractParse.SimpleAmb
open GLL.AbstractParse.SimpleRightNull
open GLL.AbstractParse.SimpleleftRecursion

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

let test buildAstAbstract qGraph = 
    let r = buildAstAbstract qGraph
    printfn "%A" r
    match r with
        | Error str, _ ->
            printfn "Error"
        | Success tree, tokens ->
            //tree.PrintAst()
//            let n, e, eps, t, amb = tree.CountCounters()
//            Assert.AreEqual(nodesCount, n, "Nodes count mismatch")
//            Assert.AreEqual(edgesCount, e, "Edges count mismatch")
//            Assert.AreEqual(epsilonsCount, eps, "Epsilons count mismatch")
//            Assert.AreEqual(termsCount, t, "Terms count mismatch")
//            Assert.AreEqual(ambiguityCount, amb, "Ambiguities count mismatch")
//            Assert.Pass()


[<TestFixture>]
type ``GLL abstract parser tests`` () =

    [<Test>]
    member this.SimpleRightRecursion () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.AbstractParse.SimpleRightRecursion.B 1)
             edg 1 2 (GLL.AbstractParse.SimpleRightRecursion.A 2)
             edg 2 3 (GLL.AbstractParse.SimpleRightRecursion.A 3)
             edg 3 4 (GLL.AbstractParse.SimpleRightRecursion.B 4)
             edg 4 5 (GLL.AbstractParse.SimpleRightRecursion.RNGLR_EOF 0)
             ] |> ignore

        test GLL.AbstractParse.SimpleRightRecursion qGraph 

    [<Test>]
    member this.BadLeftRecursion () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.AbstractParse.BadLeftRecursion.B 1)
             edg 1 2 (GLL.AbstractParse.BadLeftRecursion.B 2)
             edg 2 3 (GLL.AbstractParse.BadLeftRecursion.B 3)
             edg 3 4 (GLL.AbstractParse.BadLeftRecursion.RNGLR_EOF 0)
             ] |> ignore

        test GLL.AbstractParse.BadLeftRecursion.buildAstAbstract qGraph

    [<Test>]
    member this.SimpleAmb () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.AbstractParse.SimpleAmb.A 1)
             edg 1 2 (GLL.AbstractParse.SimpleAmb.D 2)
             edg 2 3 (GLL.AbstractParse.SimpleAmb.B 3)
             edg 3 4 (GLL.AbstractParse.SimpleAmb.RNGLR_EOF 0)
             ] |> ignore

        test GLL.AbstractParse.SimpleAmb.buildAstAbstract qGraph
    
    [<Test>]
    member this.SimpleRightNull () =
        let qGraph = new ParserInputGraph<_>(0, 1)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.AbstractParse.SimpleRightNull.A 1)
             edg 1 2 (GLL.AbstractParse.SimpleRightNull.RNGLR_EOF 0)
             ] |> ignore

        test GLL.AbstractParse.SimpleRightNull.buildAstAbstract qGraph

    [<Test>]
    member this.SimpleLeftRecursion () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.AbstractParse.SimpleleftRecursion.B 1)
             edg 1 2 (GLL.AbstractParse.SimpleleftRecursion.B 2)
             edg 2 3 (GLL.AbstractParse.SimpleleftRecursion.B 3)
             edg 3 4 (GLL.AbstractParse.SimpleleftRecursion.RNGLR_EOF 0)
             ] |> ignore

        test GLL.AbstractParse.SimpleleftRecursion.buildAstAbstract qGraph