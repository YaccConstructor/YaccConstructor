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

open GLL.SimpleRightRecursion
open GLL.BadLeftRecursion
open GLL.SimpleAmb

open GLL.SimpleRightNull
open GLL.SimpleLeftRecursion
open GLL.ParseSimpleBranch

open GLL.Brackets
open GLL.CroppedBrackets
open GLL.Eps

open GLL.FirstEps
open GLL.List
open GLL.NotAmbigousSimpleCalc

open GLL.NotAmbigousSimpleCalcWith2Ops
open GLL.ParseCalc
open GLL.ParseSimpleCalc

open GLL.PrettySimpleCalc
open GLL.SimpleCalcWithNTerm
open GLL.SimpleCalcWithNTerms_2

open GLL.SimpleCalcWithNTerms_3
open GLL.SimpleCalcWithNTerms_4
open GLL.Stars

open GLL.StrangeBrackets
open GLL.Stars2

let outputDir = @"../../../src/GLL.AbstractParser.SimpleTest/"

let lbl tokenId = tokenId
let edg f t l = new ParserEdge<_>(f,t,lbl l)


let test buildAbstractAst qGraph (intToString : int -> string) (fileName : string) = 
    let r = buildAbstractAst qGraph
    printfn "%A" r
    match r with
        | Error str ->
            printfn "Error"
            Assert.Fail("")
        | Success tree ->
            tree.AstToDot intToString (outputDir + fileName)
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
            [edg 0 1 (GLL.SimpleRightRecursion.B 1)
             edg 1 2 (GLL.SimpleRightRecursion.A 2)
             edg 2 3 (GLL.SimpleRightRecursion.A 3)
             edg 3 4 (GLL.SimpleRightRecursion.B 4)
             edg 4 5 (GLL.SimpleRightRecursion.RNGLR_EOF 0)
             ] |> ignore

        test GLL.SimpleRightRecursion.buildAbstractAst qGraph GLL.SimpleRightRecursion.numToString "SimpleRightRecursion.dot"

    [<Test>]
    member this.BadLeftRecursion () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.BadLeftRecursion.B 1)
             edg 1 2 (GLL.BadLeftRecursion.B 2)
             edg 2 3 (GLL.BadLeftRecursion.B 3)
             edg 3 4 (GLL.BadLeftRecursion.RNGLR_EOF 0)
             ] |> ignore

        test GLL.BadLeftRecursion.buildAbstractAst qGraph GLL.BadLeftRecursion.numToString "BadLeftRecursion.dot"

    [<Test>]
    member this.SimpleAmb () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.SimpleAmb.A 1)
             edg 1 2 (GLL.SimpleAmb.D 2)
             edg 2 3 (GLL.SimpleAmb.B 3)
             edg 3 4 (GLL.SimpleAmb.RNGLR_EOF 0)
             ] |> ignore

        test GLL.SimpleAmb.buildAbstractAst qGraph GLL.SimpleAmb.numToString "SimpleAmb.dot"
    
    [<Test>]
    member this.SimpleRightNull () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.SimpleRightNull.A 1)
             edg 1 2 (GLL.SimpleRightNull.RNGLR_EOF 0)
             ] |> ignore

        test GLL.SimpleRightNull.buildAbstractAst qGraph GLL.SimpleRightNull.numToString "SimpleRightNull.dot"

    [<Test>]
    member this.SimpleLeftRecursion () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.SimpleLeftRecursion.B 1)
             edg 1 2 (GLL.SimpleLeftRecursion.B 2)
             edg 2 3 (GLL.SimpleLeftRecursion.B 3)
             edg 3 4 (GLL.SimpleLeftRecursion.RNGLR_EOF 0)
             ] |> ignore

        test GLL.SimpleLeftRecursion.buildAbstractAst qGraph GLL.SimpleLeftRecursion.numToString "SimpleLeftRecursion.dot"

    [<Test>]
    member this.SimpleBranch () =
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.ParseSimpleBranch.A 1)
             edg 1 2 (GLL.ParseSimpleBranch.C 2)
             edg 1 2 (GLL.ParseSimpleBranch.B 3)
             edg 2 3 (GLL.ParseSimpleBranch.RNGLR_EOF 0)
             ] |> ignore

        test GLL.ParseSimpleBranch.buildAbstractAst qGraph GLL.ParseSimpleBranch.numToString "SimpleBranch.dot"

    [<Test>]
    member this._01_PrettySimpleCalc_SequenceInput () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.PrettySimpleCalc.NUM 1)
             edg 1 2 (GLL.PrettySimpleCalc.PLUS 2)
             edg 2 3 (GLL.PrettySimpleCalc.NUM 3)
             edg 3 4 (GLL.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test GLL.PrettySimpleCalc.buildAbstractAst qGraph GLL.PrettySimpleCalc.numToString "PrettySimpleCalc.dot"

    [<Test>]
    member this._02_PrettySimpleCalcSimple_BranchedInput () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.PrettySimpleCalc.NUM 1)
             edg 1 2 (GLL.PrettySimpleCalc.PLUS 2)
             edg 2 3 (GLL.PrettySimpleCalc.NUM 3)
             edg 0 3 (GLL.PrettySimpleCalc.NUM 4)
             edg 3 4 (GLL.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test GLL.PrettySimpleCalc.buildAbstractAst qGraph GLL.PrettySimpleCalc.numToString "PrettySimpleCalc.dot"

    [<Test>]
    member this._03_PrettySimpleCalc_BranchedInput () =
        let qGraph = new ParserInputGraph<_>(2, 9)
        qGraph.AddVerticesAndEdgeRange
            [
             edg 2 3 (GLL.PrettySimpleCalc.NUM 1)
             edg 3 4 (GLL.PrettySimpleCalc.PLUS 2)
             edg 4 5 (GLL.PrettySimpleCalc.NUM 3)
             edg 3 6 (GLL.PrettySimpleCalc.PLUS 4)
             edg 6 5 (GLL.PrettySimpleCalc.NUM 5)
             edg 5 7 (GLL.PrettySimpleCalc.PLUS 6)
             edg 7 8 (GLL.PrettySimpleCalc.NUM 7)
             edg 8 9 (GLL.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test GLL.PrettySimpleCalc.buildAbstractAst qGraph GLL.PrettySimpleCalc.numToString "PrettySimpleCalc.dot"

    [<Test>]
    member this._04_PrettySimpleCalc_LotsOfVariants () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.PrettySimpleCalc.NUM 1)
             edg 1 2 (GLL.PrettySimpleCalc.PLUS 2)
             edg 2 3 (GLL.PrettySimpleCalc.NUM 3)
             edg 3 4 (GLL.PrettySimpleCalc.PLUS 4)
             edg 4 5 (GLL.PrettySimpleCalc.NUM 5)
             edg 3 6 (GLL.PrettySimpleCalc.PLUS 6)
             edg 6 5 (GLL.PrettySimpleCalc.NUM 7)
             edg 5 7 (GLL.PrettySimpleCalc.PLUS 8)
             edg 7 8 (GLL.PrettySimpleCalc.NUM 9)
             edg 8 9 (GLL.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test GLL.PrettySimpleCalc.buildAbstractAst qGraph GLL.PrettySimpleCalc.numToString "PrettySimpleCalc.dot"

    [<Test>]
    member this._05_NotAmbigousSimpleCalc_LotsOfVariants () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (GLL.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (GLL.NotAmbigousSimpleCalc.NUM 5)
             edg 3 6 (GLL.NotAmbigousSimpleCalc.PLUS 6)
             edg 6 5 (GLL.NotAmbigousSimpleCalc.NUM 7)
             edg 5 7 (GLL.NotAmbigousSimpleCalc.PLUS 8)
             edg 7 8 (GLL.NotAmbigousSimpleCalc.NUM 9)
             edg 8 9 (GLL.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test GLL.NotAmbigousSimpleCalc.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalc.numToString "NotAmbigousSimpleCalc1.dot"

    [<Test>]
    member this._06_NotAmbigousSimpleCalc_Loop () =
        let qGraph = new ParserInputGraph<_>(0 , 7)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (GLL.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (GLL.NotAmbigousSimpleCalc.NUM 5)
             edg 5 2 (GLL.NotAmbigousSimpleCalc.PLUS 6)
             edg 4 6 (GLL.NotAmbigousSimpleCalc.NUM 7)
             edg 6 7 (GLL.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test GLL.NotAmbigousSimpleCalc.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalc.numToString "NotAmbigousSimpleCalc2.dot"

    [<Test>]
    member this._07_NotAmbigousSimpleCalc_Loop2 () =
        let qGraph = new ParserInputGraph<_>(0, 7)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (GLL.NotAmbigousSimpleCalc.PLUS 4)
             edg 3 5 (GLL.NotAmbigousSimpleCalc.PLUS 5)
             edg 5 1 (GLL.NotAmbigousSimpleCalc.NUM 6)
             edg 4 6 (GLL.NotAmbigousSimpleCalc.NUM 7)
             edg 6 7 (GLL.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore
        
        test GLL.NotAmbigousSimpleCalc.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalc.numToString "NotAmbigousSimpleCalc3.dot"

    [<Test>]
    member this._08_NotAmbigousSimpleCalc_Loop3 () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (GLL.NotAmbigousSimpleCalc.PLUS 4)
             edg 3 5 (GLL.NotAmbigousSimpleCalc.PLUS 5)
             edg 5 7 (GLL.NotAmbigousSimpleCalc.NUM 6)
             edg 7 2 (GLL.NotAmbigousSimpleCalc.PLUS 8)
             edg 4 6 (GLL.NotAmbigousSimpleCalc.NUM 7)
             edg 6 8 (GLL.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test GLL.NotAmbigousSimpleCalc.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalc.numToString "NotAmbigousSimpleCalc4.dot"
        
    [<Test>]
    member this._09_NotAmbigousSimpleCalc_Loop4 () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (GLL.NotAmbigousSimpleCalc.PLUS 4)
             edg 3 5 (GLL.NotAmbigousSimpleCalc.PLUS 5)
             edg 5 3 (GLL.NotAmbigousSimpleCalc.NUM 6)             
             edg 4 6 (GLL.NotAmbigousSimpleCalc.NUM 7)
             edg 6 8 (GLL.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test GLL.NotAmbigousSimpleCalc.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalc.numToString "NotAmbigousSimpleCalc5.dot"

    [<Test>]
    member this._10_NotAmbigousSimpleCalc_Loop5 () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (GLL.NotAmbigousSimpleCalc.PLUS 4)
             edg 3 5 (GLL.NotAmbigousSimpleCalc.PLUS 5)
             edg 5 3 (GLL.NotAmbigousSimpleCalc.NUM 6)
             edg 3 8 (GLL.NotAmbigousSimpleCalc.PLUS 7)
             edg 8 3 (GLL.NotAmbigousSimpleCalc.NUM 8)
             edg 4 6 (GLL.NotAmbigousSimpleCalc.NUM 9)
             edg 6 9 (GLL.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore


        test GLL.NotAmbigousSimpleCalc.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalc.numToString "NotAmbigousSimpleCalc6.dot"

    [<Test>]
    member this._11_NotAmbigousSimpleCalc_Loop6 () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (GLL.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (GLL.NotAmbigousSimpleCalc.NUM 5)
             edg 5 2 (GLL.NotAmbigousSimpleCalc.PLUS 6)
             edg 5 7 (GLL.NotAmbigousSimpleCalc.PLUS 6)
             edg 7 1 (GLL.NotAmbigousSimpleCalc.NUM 8)
             edg 4 6 (GLL.NotAmbigousSimpleCalc.NUM 7)
             edg 6 8 (GLL.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore


        test GLL.NotAmbigousSimpleCalc.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalc.numToString "NotAmbigousSimpleCalc7.dot"

    [<Test>]
    member this._12_NotAmbigousSimpleCalc_Loop7 () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (GLL.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (GLL.NotAmbigousSimpleCalc.NUM 5)
             edg 7 5 (GLL.NotAmbigousSimpleCalc.NUM 6)
             edg 5 7 (GLL.NotAmbigousSimpleCalc.PLUS 7)
             edg 7 1 (GLL.NotAmbigousSimpleCalc.NUM 8)
             edg 4 6 (GLL.NotAmbigousSimpleCalc.NUM 9)
             edg 6 8 (GLL.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore


        test GLL.NotAmbigousSimpleCalc.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalc.numToString "NotAmbigousSimpleCalc8.dot"

    [<Test>]
    member this._13_NotAmbigousSimpleCalc_Loop8 () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (GLL.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (GLL.NotAmbigousSimpleCalc.NUM 5)
             edg 5 2 (GLL.NotAmbigousSimpleCalc.PLUS 6)
             edg 7 5 (GLL.NotAmbigousSimpleCalc.NUM 7)
             edg 5 7 (GLL.NotAmbigousSimpleCalc.PLUS 8)
             edg 7 1 (GLL.NotAmbigousSimpleCalc.NUM 9)
             edg 4 6 (GLL.NotAmbigousSimpleCalc.NUM 10)
             edg 6 8 (GLL.NotAmbigousSimpleCalc.RNGLR_EOF 0)
             ] |> ignore


        test GLL.NotAmbigousSimpleCalc.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalc.numToString "NotAmbigousSimpleCalc9.dot"

    [<Test>]
    member this._14_NotAmbigousSimpleCalcWith2Ops_Loop () =
        let qGraph = new ParserInputGraph<_>(0, 7)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 3)
             edg 3 4 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 4)
             edg 4 5 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 5)
             edg 5 2 (GLL.NotAmbigousSimpleCalcWith2Ops.MULT 6)
             edg 4 6 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 7)
             edg 6 7 (GLL.NotAmbigousSimpleCalcWith2Ops.RNGLR_EOF 0)
             ] |> ignore
        
        test GLL.NotAmbigousSimpleCalcWith2Ops.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalcWith2Ops.numToString "NotAmbigousSimpleCalcWith2Ops.dot"

    [<Test>]
    member this._15_NotAmbigousSimpleCalcWith2Ops_Loops () =
        let qGraph = new ParserInputGraph<_>(0, 8)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 3)
             edg 2 4 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 4)
             edg 3 4 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 5)
             edg 4 5 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 6)
             edg 5 2 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 7)
             edg 4 6 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 8)
             edg 6 7 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 9)
             edg 7 8 (GLL.NotAmbigousSimpleCalcWith2Ops.RNGLR_EOF 0)
             ] |> ignore
        
        test GLL.NotAmbigousSimpleCalcWith2Ops.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalcWith2Ops.numToString "NotAmbigousSimpleCalcWith2Ops2.dot"

    [<Test>]
    member this._16_Stars_Loop () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (GLL.Stars.STAR 1)
             edg 0 1 (GLL.Stars.SEMI 2)
             edg 1 2 (GLL.Stars.RNGLR_EOF 0)
             ] |> ignore
        
        test GLL.Stars.buildAbstractAst qGraph GLL.Stars.numToString "Stars.dot"

    [<Test>]
    member this._17_Stars2_Loop () =
        let qGraph = new ParserInputGraph<_>(0, 1)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (GLL.Stars2.STAR 1)
             edg 0 1 (GLL.Stars2.RNGLR_EOF 0)
             ] |> ignore
        
        test GLL.Stars2.buildAbstractAst qGraph GLL.Stars2.numToString "Stars2.dot"

    [<Test>]
    member this._18_Stars2_Loop2 () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (GLL.Stars2.STAR 1)
             edg 0 1 (GLL.Stars2.STAR 2)
             edg 1 2 (GLL.Stars2.RNGLR_EOF 0)
             ] |> ignore
        
        test GLL.Stars2.buildAbstractAst qGraph GLL.Stars2.numToString "Stars2_2.dot"

    [<Test>]
    member this._19_FirstEps () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.FirstEps.Z 1)
            edg 1 3 (GLL.FirstEps.N 2)
            edg 3 4 (GLL.FirstEps.RNGLR_EOF 0)
            ] |> ignore

        test GLL.FirstEps.buildAbstractAst qGraph GLL.FirstEps.numToString "FirstEps.dot"
    
    [<Test>]
    member this._20_CroppedBrackets () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (GLL.CroppedBrackets.LBR 1)
            edg 0 1 (GLL.CroppedBrackets.NUM 2)
            edg 1 1 (GLL.CroppedBrackets.RBR 3)
            edg 1 2 (GLL.CroppedBrackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.CroppedBrackets.buildAbstractAst qGraph GLL.CroppedBrackets.numToString "CroppedBrackets.dot"

    [<Test>]
    member this._21_Brackets () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (GLL.Brackets.LBR 1)
            edg 0 1 (GLL.Brackets.NUM 2)
            edg 1 1 (GLL.Brackets.RBR 3)
            edg 1 2 (GLL.Brackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.Brackets.buildAbstractAst qGraph GLL.Brackets.numToString "Brackets.dot"

    [<Test>]
    member this._22_Brackets_BackEdge () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (GLL.Brackets.LBR 1)
            edg 0 1 (GLL.Brackets.NUM 2)
            edg 1 1 (GLL.Brackets.RBR 3)
            edg 1 0 (GLL.Brackets.NUM 4)
            edg 1 2 (GLL.Brackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.Brackets.buildAbstractAst qGraph GLL.Brackets.numToString "Brackets_backEdge.dot"

    [<Test>]
    member this._23_UnambiguousBrackets () =
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.StrangeBrackets.LBR 1)
            edg 1 1 (GLL.StrangeBrackets.LBR 2)
            edg 1 2 (GLL.StrangeBrackets.RBR 3)
            edg 2 2 (GLL.StrangeBrackets.RBR 4)
            edg 2 3 (GLL.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.StrangeBrackets.buildAbstractAst qGraph GLL.StrangeBrackets.numToString "StrangeBrackets.dot"

    [<Test>]
    member this._24_UnambiguousBrackets_Circle () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.StrangeBrackets.LBR 0)
            edg 1 0 (GLL.StrangeBrackets.RBR 1)
            edg 0 9 (GLL.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.StrangeBrackets.buildAbstractAst qGraph GLL.StrangeBrackets.numToString "StrangeBrackets2.dot"

    [<Test>]
    member this._25_UnambiguousBrackets_BiggerCircle () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.StrangeBrackets.LBR 0)
            edg 1 2 (GLL.StrangeBrackets.RBR 1)
            edg 2 3 (GLL.StrangeBrackets.LBR 2)
            edg 3 0 (GLL.StrangeBrackets.RBR 3)
            edg 0 9 (GLL.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.StrangeBrackets.buildAbstractAst qGraph GLL.StrangeBrackets.numToString "StrangeBrackets3.dot"

    [<Test>]
    member this._26_UnambiguousBrackets_Inf () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (GLL.StrangeBrackets.LBR 0)
            edg 0 0 (GLL.StrangeBrackets.RBR 1)
            edg 0 9 (GLL.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.StrangeBrackets.buildAbstractAst qGraph GLL.StrangeBrackets.numToString "StrangeBrackets4.dot"

    [<Test>]
    member this._27_UnambiguousBrackets_WithoutEmptyString () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.StrangeBrackets.LBR 0)
            edg 1 0 (GLL.StrangeBrackets.RBR 1)
            edg 1 2 (GLL.StrangeBrackets.RBR 2)
            edg 2 9 (GLL.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore
        test GLL.StrangeBrackets.buildAbstractAst qGraph GLL.StrangeBrackets.numToString "StrangeBrackets5.dot"

    [<Test>]
    member this._28_UnambiguousBrackets_DifferentPathLengths () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.StrangeBrackets.LBR 0)
            edg 1 2 (GLL.StrangeBrackets.RBR 1)
            edg 2 3 (GLL.StrangeBrackets.LBR 2)
            edg 3 4 (GLL.StrangeBrackets.RBR 3)
            edg 2 5 (GLL.StrangeBrackets.LBR 4)
            edg 5 6 (GLL.StrangeBrackets.RBR 5)
            edg 6 3 (GLL.StrangeBrackets.LBR 6)
            edg 4 9 (GLL.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.StrangeBrackets.buildAbstractAst qGraph GLL.StrangeBrackets.numToString "StrangeBrackets6.dot"