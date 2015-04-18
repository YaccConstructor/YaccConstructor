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

let test buildAstAbstract qGraph nodesCount edgesCount epsilonsCount termsCount ambiguityCount = 
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
    member this._01_PrettySimpleCalc_SequenceInput () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
             edg 3 4 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph 13 12 0 3 0

    [<Test>]
    member this._02_PrettySimpleCalcSimple_BranchedInput () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (RNGLR.PrettySimpleCalc.NUM 1)
             edg 1 2 (RNGLR.PrettySimpleCalc.PLUS 2)
             edg 2 3 (RNGLR.PrettySimpleCalc.NUM 3)
             edg 0 3 (RNGLR.PrettySimpleCalc.NUM 4)
             edg 3 4 (RNGLR.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test RNGLR.PrettySimpleCalc.buildAstAbstract qGraph 15 14 0 4 1
