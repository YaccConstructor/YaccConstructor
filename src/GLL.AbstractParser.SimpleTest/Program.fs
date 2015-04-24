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
open Yard.Generators.Common.ASTGLL
open GLL.AbstractParse.SimpleRightRecursion
open GLL.AbstractParse.BadLeftRecursion
open GLL.AbstractParse.SimpleAmb
open GLL.AbstractParse.SimpleRightNull
open GLL.AbstractParse.SimpleLeftRecursion
open GLL.AbstractParse.SimpleBranch

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

let test buildAstAbstract qGraph (intToString : int -> string) (fileName : string) = 
    let r = buildAstAbstract qGraph
    printfn "%A" r
    match r with
        | Error str ->
            printfn "Error"
            Assert.Fail("")
        | Success tree ->
            tree.AstToDot intToString fileName
            Assert.Pass("UIII")
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
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.AbstractParse.SimpleRightRecursion.B 1)
             edg 1 2 (GLL.AbstractParse.SimpleRightRecursion.A 2)
             edg 2 3 (GLL.AbstractParse.SimpleRightRecursion.A 3)
             edg 3 4 (GLL.AbstractParse.SimpleRightRecursion.B 4)
             edg 4 5 (GLL.AbstractParse.SimpleRightRecursion.RNGLR_EOF 0)
             ] |> ignore

        test GLL.AbstractParse.SimpleRightRecursion.buildAbstractAst qGraph GLL.AbstractParse.SimpleRightRecursion.numToString "SimpleRightRecursion.dot"

    [<Test>]
    member this.BadLeftRecursion () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.AbstractParse.BadLeftRecursion.B 1)
             edg 1 2 (GLL.AbstractParse.BadLeftRecursion.B 2)
             edg 2 3 (GLL.AbstractParse.BadLeftRecursion.B 3)
             edg 3 4 (GLL.AbstractParse.BadLeftRecursion.RNGLR_EOF 0)
             ] |> ignore

        test GLL.AbstractParse.BadLeftRecursion.buildAbstractAst qGraph GLL.AbstractParse.BadLeftRecursion.numToString "BadLeftRecursion.dot"

    [<Test>]
    member this.SimpleAmb () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.AbstractParse.SimpleAmb.A 1)
             edg 1 2 (GLL.AbstractParse.SimpleAmb.D 2)
             edg 2 3 (GLL.AbstractParse.SimpleAmb.B 3)
             edg 3 4 (GLL.AbstractParse.SimpleAmb.RNGLR_EOF 0)
             ] |> ignore

        test GLL.AbstractParse.SimpleAmb.buildAbstractAst qGraph GLL.AbstractParse.SimpleAmb.numToString "SimpleAmb.dot"
    
    [<Test>]
    member this.SimpleRightNull () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.AbstractParse.SimpleRightNull.A 1)
             edg 1 2 (GLL.AbstractParse.SimpleRightNull.RNGLR_EOF 0)
             ] |> ignore

        test GLL.AbstractParse.SimpleRightNull.buildAbstractAst qGraph GLL.AbstractParse.SimpleRightNull.numToString "SimpleRightNull.dot"

    [<Test>]
    member this.SimpleLeftRecursion () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.AbstractParse.SimpleLeftRecursion.B 1)
             edg 1 2 (GLL.AbstractParse.SimpleLeftRecursion.B 2)
             edg 2 3 (GLL.AbstractParse.SimpleLeftRecursion.B 3)
             edg 3 4 (GLL.AbstractParse.SimpleLeftRecursion.RNGLR_EOF 0)
             ] |> ignore

        test GLL.AbstractParse.SimpleLeftRecursion.buildAbstractAst qGraph GLL.AbstractParse.SimpleLeftRecursion.numToString "SimpleLeftRecursion.dot"

    [<Test>]
    member this.SimpleBranch () =
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.AbstractParse.SimpleBranch.A 1)
             edg 1 2 (GLL.AbstractParse.SimpleBranch.C 2)
             edg 1 2 (GLL.AbstractParse.SimpleBranch.B 3)
             edg 2 3 (GLL.AbstractParse.SimpleBranch.RNGLR_EOF 0)
             ] |> ignore

        test GLL.AbstractParse.SimpleBranch.buildAbstractAst qGraph GLL.AbstractParse.SimpleBranch.numToString "SimpleBranch.dot"