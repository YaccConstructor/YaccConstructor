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
open Yard.Generators.GLL.AbstractParserWithoutTree
open YC.Tests.Helper
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon

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

open GLL.ParseAttrs
open GLL.ParseCalc
open GLL.ParseCond
open GLL.ParseCounter
open GLL.ParseCycle
open GLL.ParseEps2
open GLL.ParseEpsilon
open GLL.ParseExpr
open GLL.ParseFirst
open GLL.ParseListEps
open GLL.ParseLolCalc
open GLL.ParseLongCycle
open GLL.ParseLongCycle_BAD
open GLL.ParseLongest
open GLL.ParseMixed
open GLL.ParseOmit
//open GLL.ParseOrder

type msg =
    | Data of int*BioParserInputGraph
    | End of bool ref
    | Die of AsyncReplyChannel<unit>

let outputDir = @"../../../src/GLL.AbstractParser.SimpleTest/"

let lbl tokenId = tokenId
let edg f t l = new ParserEdge<_>(f,t,lbl l)

let perfTest2 parse graph =    
    for i = 10 to 200 do
        let g = graph (1 + i) 2 
        let start = System.DateTime.Now
        System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.LowLatency
        let r = parse g
        System.GC.Collect()        
        let finish = System.DateTime.Now - start
        printfn "%i  : %A" (i+1) finish.TotalSeconds
        System.GC.Collect()
        match r with
        | Error _ ->
            printfn "Error"     
        | Success tree->
            ()//printfn "%s" "sss"

let test buildAbstractAst qGraph l (intToString : int -> string) (fileName : string) nodesCount edgesCount termsCount ambiguityCount tokenData tokenToNum = 

    let r = buildAbstractAst qGraph l
    printfn "%A" r
    match r with
        | Error str ->
            printfn "Error"
            Assert.Fail("")
        | Success tree ->
            tree.AstToDot intToString tokenToNum tokenData (outputDir + fileName)
            let n, e, t, amb = tree.CountCounters
            printfn "%d %d %d %d" n e t amb
            Assert.AreEqual(nodesCount, n, "Nodes count mismatch")
            Assert.AreEqual(edgesCount, e, "Edges count mismatch")
            Assert.AreEqual(termsCount, t, "Terms count mismatch") 
            Assert.AreEqual(ambiguityCount, amb, "Ambiguities count mismatch")
            Assert.Pass()
     
let f arr tokenToNumber = Array.map (fun e -> tokenToNumber e) arr
let len (edges : BioParserEdge[]) : int[] = edges |> Array.map (fun e -> e.Tokens.Length + 1) 
let filterRnaParsingResult res expectedRange lengthLimit =
    let ranges = YC.Bio.RNA.Search.filterRnaParsingResult lengthLimit res
    Assert.IsTrue(ranges |> Microsoft.FSharp.Collections.ResizeArray.exists ((=) expectedRange))

[<TestFixture>]
type ``GLL abstract parser tests`` () =
//    [<Test>]
//    member this._01_PrettySimpleCalc_SequenceInput () =
//        
//        let a1 = f [|GLL.PrettySimpleCalc.NUM 0|] GLL.PrettySimpleCalc.tokenToNumber
//        let a2 = f [|GLL.PrettySimpleCalc.PLUS 2|] GLL.PrettySimpleCalc.tokenToNumber
//        let a3 = f [|GLL.PrettySimpleCalc.NUM 3|] GLL.PrettySimpleCalc.tokenToNumber
//        let a4 = f [|GLL.PrettySimpleCalc.RNGLR_EOF 0|] GLL.PrettySimpleCalc.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1; 
//            edgB 1 2 a2; 
//            edgB 2 3 a3;
//            edgB 3 4 a4|]
//         
//        let qGraph = new BioParserInputGraph<GLL.PrettySimpleCalc.Token>([|0|], 4, len edges, edges, 5)
//        test GLL.PrettySimpleCalc.buildAbstractAst qGraph 100 GLL.PrettySimpleCalc.numToString "PrettySimpleCalcSeq.dot" 21 24 5 0 GLL.PrettySimpleCalc.tokenData GLL.PrettySimpleCalc.tokenToNumber
//
//    [<Test>]
//    member this._06_NotAmbigousSimpleCalc_Loop () =
//        let a1 = f [|GLL.NotAmbigousSimpleCalc.NUM 0|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let a2 = f [|GLL.NotAmbigousSimpleCalc.PLUS 2|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let a3 = f [|GLL.NotAmbigousSimpleCalc.NUM 3|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let a4 = f [|GLL.NotAmbigousSimpleCalc.RNGLR_EOF 0|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let a5 = f [|GLL.NotAmbigousSimpleCalc.PLUS 0|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1; 
//            edgB 1 2 a2; 
//            edgB 2 3 a3;
//            edgB 3 4 a4;
//            edgB 3 0 a5|]
//        let qGraph = new BioParserInputGraph<GLL.NotAmbigousSimpleCalc.Token>([|0|], 4, len edges, edges, 5)
//        
//        test GLL.NotAmbigousSimpleCalc.buildAbstractAst qGraph 100 GLL.NotAmbigousSimpleCalc.numToString "NotAmbigousSimpleCalc2.dot" 25 30 6 1 GLL.NotAmbigousSimpleCalc.tokenData GLL.NotAmbigousSimpleCalc.tokenToNumber
//
//    [<Test>]
//    member this._07_NotAmbigousSimpleCalc_LoopInLoop () =
//        let a1 = f [|GLL.NotAmbigousSimpleCalc.NUM 0|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let a2 = f [|GLL.NotAmbigousSimpleCalc.PLUS 2|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let a3 = f [|GLL.NotAmbigousSimpleCalc.NUM 3|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let a4 = f [|GLL.NotAmbigousSimpleCalc.PLUS 2|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let a5 = f [|GLL.NotAmbigousSimpleCalc.NUM 3|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let a6 = f [|GLL.NotAmbigousSimpleCalc.PLUS 2|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let a7 = f [|GLL.NotAmbigousSimpleCalc.STAR 3|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let a8 = f [|GLL.NotAmbigousSimpleCalc.RNGLR_EOF 0|] GLL.NotAmbigousSimpleCalc.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1; 
//            edgB 1 2 a2; 
//            edgB 2 3 a3;
//            edgB 3 4 a4;
//            edgB 4 5 a5;
//            edgB 5 0 a6;
//            edgB 5 2 a7;
//            edgB 5 6 a8|]
//        let qGraph = new BioParserInputGraph<GLL.NotAmbigousSimpleCalc.Token>([|0|], 6, len edges, edges, 7)
//        test  
//            GLL.NotAmbigousSimpleCalc.buildAbstractAst 
//            qGraph 
//            100
//            GLL.NotAmbigousSimpleCalc.numToString 
//            "NotAmbigousSimpleCalcLoopLoop.dot" 
//            39 48 9 2 
//            GLL.NotAmbigousSimpleCalc.tokenData
//            GLL.NotAmbigousSimpleCalc.tokenToNumber
//
////
////    [<Test>]
////    member this._14_NotAmbigousSimpleCalcWith2Ops_Loop () =
////        let qGraph = new ParserInputGraph<_>(0, 7)
////        qGraph.AddVerticesAndEdgeRange
////            [edg 0 1 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM  1)
////             edg 1 2 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 2)
////             edg 2 3 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 3)
////             edg 3 4 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 4)
////             edg 4 5 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 5)
////             edg 5 2 (GLL.NotAmbigousSimpleCalcWith2Ops.MULT 6)
////             edg 4 6 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 7)
////             edg 6 7 (GLL.NotAmbigousSimpleCalcWith2Ops.RNGLR_EOF 0)
////             ] |> ignore
////        
////        test GLL.NotAmbigousSimpleCalcWith2Ops.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalcWith2Ops.numToString "NotAmbigousSimpleCalcWith2Ops.dot" 0 0 0 0
////
////    [<Test>]
////    member this._15_NotAmbigousSimpleCalcWith2Ops_Loops () =
////        let qGraph = new ParserInputGraph<_>(0, 8)
////        qGraph.AddVerticesAndEdgeRange
////            [edg 0 1 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM  1)
////             edg 1 2 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 2)
////             edg 2 3 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 3)
////             edg 2 4 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 4)
////             edg 3 4 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 5)
////             edg 4 5 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 6)
////             edg 5 2 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 7)
////             edg 4 6 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 8)
////             edg 6 7 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 9)
////             edg 7 8 (GLL.NotAmbigousSimpleCalcWith2Ops.RNGLR_EOF 0)
////             ] |> ignore
////        
////        test GLL.NotAmbigousSimpleCalcWith2Ops.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalcWith2Ops.numToString "NotAmbigousSimpleCalcWith2Ops2.dot" 0 0 0 0
////
//    [<Test>]
//    member this._16_Stars_Loop () =
//        let a1 = f [|GLL.Stars.STAR 0|] GLL.Stars.tokenToNumber
//        let a2 = f [|GLL.Stars.SEMI 2|] GLL.Stars.tokenToNumber
//        let a3 = f [|GLL.Stars.RNGLR_EOF 3|] GLL.Stars.tokenToNumber
//        let edges = [|
//            edgB 0 0 a1; 
//            edgB 0 1 a2; 
//            edgB 1 2 a3|]
//        let qGraph = new BioParserInputGraph<GLL.Stars.Token>([|0|], 2, len edges, edges, 3)
//        test
//            GLL.Stars.buildAbstractAst 
//            qGraph 
//            100
//            GLL.Stars.numToString 
//            "Stars_Loop.dot" 
//            19 24 4 1 
//            GLL.Stars.tokenData
//            GLL.Stars.tokenToNumber
//
//    [<Test>]
//    member this._17_Stars2_Loop () =
//        let a1 = f [|GLL.Stars2.STAR 0|] GLL.Stars2.tokenToNumber
//        let a2 = f [|GLL.Stars2.RNGLR_EOF 3|] GLL.Stars2.tokenToNumber
//        let edges = [|
//            edgB 0 0 a1; 
//            edgB 0 1 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 1, len edges, edges, 2)
//        
//        test 
//            GLL.Stars2.buildAbstractAst 
//            qGraph 
//            100
//            GLL.Stars2.numToString 
//            "Stars2.dot" 
//            23 33 3 2
//            GLL.Stars2.tokenData
//            GLL.Stars2.tokenToNumber
//
//    [<Test>]
//    member this._19_FirstEps () =
//        let a1 = f [|GLL.FirstEps.Z 1|] GLL.FirstEps.tokenToNumber
//        let a2 = f [|GLL.FirstEps.N 2|] GLL.FirstEps.tokenToNumber
//        let a3 = f [|GLL.FirstEps.RNGLR_EOF 3|] GLL.FirstEps.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2; 
//            edgB 2 3 a3|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 3, len edges, edges, 4)
//        
//        test 
//            GLL.FirstEps.buildAbstractAst
//            qGraph
//            100
//            GLL.FirstEps.numToString 
//            "FirstEps.dot" 
//            26 30 6 0
//            GLL.FirstEps.tokenData
//            GLL.FirstEps.tokenToNumber
//    
//    [<Test>]
//    member this._20_CroppedBrackets () =
//        let a1 = f [|GLL.CroppedBrackets.LBR 1|] GLL.CroppedBrackets.tokenToNumber
//        let a2 = f [|GLL.CroppedBrackets.NUM 2|] GLL.CroppedBrackets.tokenToNumber
//        let a3 = f [|GLL.CroppedBrackets.RBR 3|] GLL.CroppedBrackets.tokenToNumber
//        let a4 = f [|GLL.CroppedBrackets.RNGLR_EOF 4|] GLL.CroppedBrackets.tokenToNumber
//        let edges = [|
//            edgB 0 0 a1;
//            edgB 0 1 a2;
//            edgB 1 1 a3; 
//            edgB 1 2 a4;|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.CroppedBrackets.buildAbstractAst qGraph 100 GLL.CroppedBrackets.numToString "CroppedBrackets.dot" 14 15 5 1 GLL.CroppedBrackets.tokenData GLL.CroppedBrackets.tokenToNumber
//
//    [<Test>]
//    member this._21_Brackets () =
//        let a1 = f [|GLL.Brackets.LBR 1|] GLL.Brackets.tokenToNumber
//        let a2 = f [|GLL.Brackets.NUM 2|] GLL.Brackets.tokenToNumber
//        let a3 = f [|GLL.Brackets.RBR 3|] GLL.Brackets.tokenToNumber
//        let a4 = f [|GLL.Brackets.RNGLR_EOF 4|] GLL.Brackets.tokenToNumber
//        let edges = [|
//            edgB 0 0 a1;
//            edgB 0 1 a2;
//            edgB 1 1 a3; 
//            edgB 1 2 a4;|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.Brackets.buildAbstractAst qGraph 100 GLL.Brackets.numToString "Brackets.dot" 14 15 5 1 GLL.Brackets.tokenData GLL.Brackets.tokenToNumber
//
//    [<Test>]
//    member this._22_Brackets_BackEdge () =
//        let a1 = f [|GLL.Brackets.LBR 1|] GLL.Brackets.tokenToNumber
//        let a2 = f [|GLL.Brackets.NUM 2|] GLL.Brackets.tokenToNumber
//        let a3 = f [|GLL.Brackets.RBR 3|] GLL.Brackets.tokenToNumber
//        let a4 = f [|GLL.Brackets.NUM 2|] GLL.Brackets.tokenToNumber
//        let a5 = f [|GLL.Brackets.RNGLR_EOF 4|] GLL.Brackets.tokenToNumber
//        let edges = [|
//            edgB 0 0 a1;
//            edgB 0 1 a2;
//            edgB 1 1 a3; 
//            edgB 1 0 a4;
//            edgB 1 2 a5;|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.Brackets.buildAbstractAst qGraph 100 GLL.Brackets.numToString "Brackets_backEdge.dot" 35 54 6 4 GLL.Brackets.tokenData GLL.Brackets.tokenToNumber
//
//    [<Test>]
//    member this._24_UnambiguousBrackets_Circle () =
//        let a1 = f [|GLL.StrangeBrackets.LBR 1|] GLL.StrangeBrackets.tokenToNumber
//        let a2 = f [|GLL.StrangeBrackets.RBR 3|] GLL.StrangeBrackets.tokenToNumber
//        let a3 = f [|GLL.StrangeBrackets.RNGLR_EOF 4|] GLL.StrangeBrackets.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 0 a2;
//            edgB 0 2 a3;|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.StrangeBrackets.buildAbstractAst qGraph 100 GLL.StrangeBrackets.numToString "StrangeBrackets2.dot" 19 21 6 1 GLL.StrangeBrackets.tokenData GLL.StrangeBrackets.tokenToNumber
//
//    [<Test>]
//    member this._25_UnambiguousBrackets_BiggerCircle () =
//        let a1 = f [|GLL.StrangeBrackets.LBR 1|] GLL.StrangeBrackets.tokenToNumber
//        let a2 = f [|GLL.StrangeBrackets.RBR 3|] GLL.StrangeBrackets.tokenToNumber
//        let a3 = f [|GLL.StrangeBrackets.LBR 1|] GLL.StrangeBrackets.tokenToNumber
//        let a4 = f [|GLL.StrangeBrackets.RBR 3|] GLL.StrangeBrackets.tokenToNumber
//        let a5 = f [|GLL.StrangeBrackets.RNGLR_EOF 4|] GLL.StrangeBrackets.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2;
//            edgB 2 3 a3;
//            edgB 3 0 a4;
//            edgB 0 4 a5;|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 4, len edges, edges, 5)
//        
//        test GLL.StrangeBrackets.buildAbstractAst qGraph 100 GLL.StrangeBrackets.numToString "StrangeBrackets3.dot" 30 33 9 1 GLL.StrangeBrackets.tokenData GLL.StrangeBrackets.tokenToNumber
//
//    [<Test>]
//    member this._26_UnambiguousBrackets_Inf () =
//        let a1 = f [|GLL.StrangeBrackets.LBR 1|] GLL.StrangeBrackets.tokenToNumber
//        let a2 = f [|GLL.StrangeBrackets.RBR 3|] GLL.StrangeBrackets.tokenToNumber
//        let a3 = f [|GLL.StrangeBrackets.RNGLR_EOF 4|] GLL.StrangeBrackets.tokenToNumber
//        let edges = [|
//            edgB 0 0 a1;
//            edgB 0 0 a2;
//            edgB 0 1 a3;|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 1, len edges, edges, 2)
//
//        test GLL.StrangeBrackets.buildAbstractAst qGraph 100 GLL.StrangeBrackets.numToString "StrangeBrackets4.dot" 16 18 5 1 GLL.StrangeBrackets.tokenData GLL.StrangeBrackets.tokenToNumber
//
//    
//    [<Test>]
//    member this._29_Attrs () =
//        let a1 = f [|GLL.ParseAttrs.A 1; GLL.ParseAttrs.A 1; GLL.ParseAttrs.A 1; GLL.ParseAttrs.A 1; GLL.ParseAttrs.A 1|] GLL.ParseAttrs.tokenToNumber
//        let a2 = f [|GLL.ParseAttrs.RNGLR_EOF 2|] GLL.ParseAttrs.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.ParseAttrs.buildAbstractAst qGraph 100 GLL.ParseAttrs.numToString "Attrs.dot" 29 33 7 0 GLL.ParseAttrs.tokenData GLL.ParseAttrs.tokenToNumber
//
//    [<Test>]
//    member this._30_Condition () =
//        let a1 = f [|GLL.ParseCond.IF 1; GLL.ParseCond.IF 1; GLL.ParseCond.A 3; GLL.ParseCond.ELSE 4; GLL.ParseCond.A 5|] GLL.ParseCond.tokenToNumber
//        let a2 = f [|GLL.ParseCond.RNGLR_EOF 2|] GLL.ParseCond.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//
//        test GLL.ParseCond.buildAbstractAst qGraph 100 GLL.ParseCond.numToString "Cond.dot" 44 57 7 1 GLL.ParseCond.tokenData GLL.ParseCond.tokenToNumber
//
//    [<Test>]
//    member this._31_Counter () =
//        let a1 = f [|GLL.ParseCounter.A 1; GLL.ParseCounter.A 1; GLL.ParseCounter.A 3; GLL.ParseCounter.A 4; GLL.ParseCounter.A 5|] GLL.ParseCounter.tokenToNumber
//        let a2 = f [|GLL.ParseCounter.RNGLR_EOF 2|] GLL.ParseCounter.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//
//        test GLL.ParseCounter.buildAbstractAst qGraph 100 GLL.ParseCounter.numToString "Counter.dot" 21 21 7 0 GLL.ParseCounter.tokenData GLL.ParseCounter.tokenToNumber
//    
//    [<Test>]
//    member this._32_Cycle () =
//        let a1 = f [|GLL.ParseCycle.A 1; GLL.ParseCycle.B 1|] GLL.ParseCycle.tokenToNumber
//        let a2 = f [|GLL.ParseCycle.RNGLR_EOF 2|] GLL.ParseCycle.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//
//        test GLL.ParseCycle.buildAbstractAst qGraph 100 GLL.ParseCycle.numToString "Cycle.dot" 15 18 4 1 GLL.ParseCycle.tokenData GLL.ParseCycle.tokenToNumber
//         
//    [<Test>]
//    member this._33_Epsilon2_with_eps2_yrd () =
//        let a1 = f [|GLL.ParseEps2.Z 1; GLL.ParseEps2.N 1|] GLL.ParseEps2.tokenToNumber
//        let a2 = f [|GLL.ParseEps2.RNGLR_EOF 2|] GLL.ParseEps2.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.ParseEps2.buildAbstractAst qGraph 100 GLL.ParseEps2.numToString "Eps2.dot" 26 30 6 0 GLL.ParseEps2.tokenData GLL.ParseEps2.tokenToNumber
//
//    [<Test>]
//    member this._34_Epsilon () =
//        let a1 = f [|GLL.ParseEpsilon.RNGLR_EOF 2|] GLL.ParseEpsilon.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 1, len edges, edges, 2)
//        
//        test GLL.ParseEpsilon.buildAbstractAst qGraph 100 GLL.ParseEpsilon.numToString "Epsilon.dot" 21 24 5 0 GLL.ParseEpsilon.tokenData GLL.ParseEpsilon.tokenToNumber
//        
//    [<Test>]
//    member this._35_Expression () =
//        let a1 = f [|GLL.ParseExpr.N 1; GLL.ParseExpr.P 1; GLL.ParseExpr.N 3; GLL.ParseExpr.P 4; GLL.ParseExpr.N 5|] GLL.ParseExpr.tokenToNumber
//        let a2 = f [|GLL.ParseExpr.RNGLR_EOF 2|] GLL.ParseExpr.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.ParseExpr.buildAbstractAst qGraph 100 GLL.ParseExpr.numToString "Expr.dot" 36 45 7 1 GLL.ParseExpr.tokenData GLL.ParseExpr.tokenToNumber
//
//    [<Test>]
//    member this._36_First () =
//        let a1 = f [|GLL.ParseFirst.A 1; GLL.ParseFirst.A 1; GLL.ParseFirst.A 3; GLL.ParseFirst.A 4; GLL.ParseFirst.B 5|] GLL.ParseFirst.tokenToNumber
//        let a2 = f [|GLL.ParseFirst.RNGLR_EOF 2|] GLL.ParseFirst.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//       
//        test GLL.ParseFirst.buildAbstractAst qGraph 100 GLL.ParseFirst.numToString "First.dot" 21 21 7 0 GLL.ParseFirst.tokenData GLL.ParseFirst.tokenToNumber
//
//    [<Test>]
//    member this._37_ListEps () =
//        let a1 = f [|GLL.ParseListEps.NUM 1; GLL.ParseListEps.NUM 1|] GLL.ParseListEps.tokenToNumber
//        let a2 = f [|GLL.ParseListEps.RNGLR_EOF 2|] GLL.ParseListEps.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//
//        test GLL.ParseListEps.buildAbstractAst qGraph 100 GLL.ParseListEps.numToString "ListEps.dot" 72 93 13 3 GLL.ParseListEps.tokenData GLL.ParseListEps.tokenToNumber
////    
////    [<Test>]
////    member this._38_LolCalc () =
////        let qGraph = new ParserInputGraph<_>(0, 12)
////        qGraph.AddVerticesAndEdgeRange
////           [edg 0 1 (GLL.ParseLolCalc.A 1)
////            edg 1 2 (GLL.ParseLolCalc.MUL 2)
////            edg 2 3 (GLL.ParseLolCalc.B 3)
////            edg 3 4 (GLL.ParseLolCalc.ADD 4)
////            edg 4 5 (GLL.ParseLolCalc.A 5)
////            edg 5 6 (GLL.ParseLolCalc.MUL 6)
////            edg 6 7 (GLL.ParseLolCalc.B 7)
////            edg 7 8 (GLL.ParseLolCalc.ADD 8)
////            edg 8 9 (GLL.ParseLolCalc.B 9)
////            edg 9 10 (GLL.ParseLolCalc.MUL 10)
////            edg 10 11 (GLL.ParseLolCalc.A 11)
////            edg 11 12 (GLL.ParseLolCalc.RNGLR_EOF 0)
////            ] |> ignore
////
////        test GLL.ParseLolCalc.buildAbstractAst qGraph GLL.ParseLolCalc.numToString "LolCalc.dot" 0 0 0 0
////    
//    [<Test>]
//    member this._39_LongCycle () =
//        let a1 = f [|GLL.ParseLongCycle.A 1|] GLL.ParseLongCycle.tokenToNumber
//        let a2 = f [|GLL.ParseLongCycle.RNGLR_EOF 2|] GLL.ParseLongCycle.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.ParseLongCycle.buildAbstractAst qGraph 100 GLL.ParseLongCycle.numToString "LongCycle.dot" 14 18 3 1 GLL.ParseLongCycle.tokenData GLL.ParseLongCycle.tokenToNumber
//    
//    [<Test>]
//    member this._41_Longest () =
//        let a1 = f [|GLL.ParseLongest.A 1; GLL.ParseLongest.A 1; GLL.ParseLongest.A 3; GLL.ParseLongest.A 4; GLL.ParseLongest.A 5|] GLL.ParseLongest.tokenToNumber
//        let a2 = f [|GLL.ParseLongest.RNGLR_EOF 2|] GLL.ParseLongest.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//       
//        test GLL.ParseLongest.buildAbstractAst qGraph 100 GLL.ParseLongest.numToString "Longest.dot" 91 123 14 1 GLL.ParseLongest.tokenData GLL.ParseLongest.tokenToNumber
//         
//    [<Test>]
//    member this._42_Mixed () =
//        let a1 = f [|GLL.ParseMixed.B 1; GLL.ParseMixed.A 1; GLL.ParseMixed.B 3; GLL.ParseMixed.A 4|] GLL.ParseMixed.tokenToNumber
//        let a2 = f [|GLL.ParseMixed.RNGLR_EOF 2|] GLL.ParseMixed.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//
//        test GLL.ParseMixed.buildAbstractAst qGraph 100 GLL.ParseMixed.numToString "Mixed.dot" 24 27 6 0 GLL.ParseMixed.tokenData GLL.ParseMixed.tokenToNumber
//    
//    [<Test>]
//    member this._43_Omit () =
//        let a1 = f [|GLL.ParseOmit.A 1; GLL.ParseOmit.B 1; GLL.ParseOmit.A 3|] GLL.ParseOmit.tokenToNumber
//        let a2 = f [|GLL.ParseOmit.RNGLR_EOF 2|] GLL.ParseOmit.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.ParseOmit.buildAbstractAst qGraph 100 GLL.ParseOmit.numToString "Omit.dot" 26 30 6 0 GLL.ParseOmit.tokenData GLL.ParseOmit.tokenToNumber
//    
////    [<Test>]
////    member this._44_Order () =
////        let qGraph = new ParserInputGraph<_>(0, 9)
////        qGraph.AddVerticesAndEdgeRange
////           [edg 0 1 (GLL.ParseOrder.A 1)
////            edg 1 2 (GLL.ParseOrder.A 2)
////            edg 2 3 (GLL.ParseOrder.A 3)
////            edg 3 4 (GLL.ParseOrder.A 4)
////            edg 4 5 (GLL.ParseOrder.A 5)
////            edg 5 6 (GLL.ParseOrder.A 6)
////            edg 6 7 (GLL.ParseOrder.A 7)
////            edg 7 8 (GLL.ParseOrder.A 8)
////            edg 8 9 (GLL.ParseOrder.RNGLR_EOF 0)
////            ] |> ignore
////
////        test GLL.ParseOrder.buildAbstractAst qGraph GLL.ParseOrder.numToString "Order.dot"
//
//    [<Test>]
//    member this._45_SimpleRightRecursion () =
//        let a1 = f [|GLL.SimpleRightRecursion.B 1; GLL.SimpleRightRecursion.B 1; GLL.SimpleRightRecursion.B 3|] GLL.SimpleRightRecursion.tokenToNumber
//        let a2 = f [|GLL.SimpleRightRecursion.RNGLR_EOF 2|] GLL.SimpleRightRecursion.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.SimpleRightRecursion.buildAbstractAst qGraph 100 GLL.SimpleRightRecursion.numToString "SimpleRightRecursion.dot" 15 15 5 0 GLL.SimpleRightRecursion.tokenData GLL.SimpleRightRecursion.tokenToNumber
//
//    [<Test>]
//    member this._46_BadLeftRecursion () =
//        let a1 = f [|GLL.BadLeftRecursion.B 1; GLL.BadLeftRecursion.B 1; GLL.BadLeftRecursion.B 3|] GLL.BadLeftRecursion.tokenToNumber
//        let a2 = f [|GLL.BadLeftRecursion.RNGLR_EOF 2|] GLL.BadLeftRecursion.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.BadLeftRecursion.buildAbstractAst qGraph 100 GLL.BadLeftRecursion.numToString "BadLeftRecursion.dot" 33 45 5 1 GLL.BadLeftRecursion.tokenData GLL.BadLeftRecursion.tokenToNumber
//
//    [<Test>]
//    member this._47_SimpleAmb () =
//        let a1 = f [|GLL.SimpleAmb.A 1; GLL.SimpleAmb.D 1; GLL.SimpleAmb.B 3|] GLL.SimpleAmb.tokenToNumber
//        let a2 = f [|GLL.SimpleAmb.RNGLR_EOF 2|] GLL.SimpleAmb.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.SimpleAmb.buildAbstractAst qGraph 100 GLL.SimpleAmb.numToString "SimpleAmb.dot" 18 21 5 1 GLL.SimpleAmb.tokenData GLL.SimpleAmb.tokenToNumber
//    
//    [<Test>]
//    member this._48_SimpleRightNull () =
//        let a1 = f [|GLL.SimpleRightNull.A 1; GLL.SimpleRightNull.A 1|] GLL.SimpleRightNull.tokenToNumber
//        let a2 = f [|GLL.SimpleRightNull.RNGLR_EOF 2|] GLL.SimpleRightNull.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.SimpleRightNull.buildAbstractAst qGraph 100 GLL.SimpleRightNull.numToString "SimpleRightNull.dot" 22 24 6 0 GLL.SimpleRightNull.tokenData GLL.SimpleRightNull.tokenToNumber
//
//    [<Test>]
//    member this._49_SimpleLeftRecursion () =
//        let a1 = f [|GLL.SimpleLeftRecursion.B 1; GLL.SimpleLeftRecursion.B 1; GLL.SimpleLeftRecursion.B 3|] GLL.SimpleLeftRecursion.tokenToNumber
//        let a2 = f [|GLL.SimpleLeftRecursion.RNGLR_EOF 2|] GLL.SimpleLeftRecursion.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 2, len edges, edges, 3)
//        
//        test GLL.SimpleLeftRecursion.buildAbstractAst qGraph 100 GLL.SimpleLeftRecursion.numToString "SimpleLeftRecursion.dot" 19 21 5 0 GLL.SimpleLeftRecursion.tokenData GLL.SimpleLeftRecursion.tokenToNumber
//
//    [<Test>]
//    member this._50_SimpleBranch () =
//        let a1 = f [|GLL.ParseSimpleBranch.A 1|] GLL.ParseSimpleBranch.tokenToNumber
//        let a2 = f [|GLL.ParseSimpleBranch.C 1|] GLL.ParseSimpleBranch.tokenToNumber
//        let a3 = f [|GLL.ParseSimpleBranch.B 1|] GLL.ParseSimpleBranch.tokenToNumber
//        let a4 = f [|GLL.ParseSimpleBranch.RNGLR_EOF 2|] GLL.ParseSimpleBranch.tokenToNumber
//        let edges = [|
//            edgB 0 1 a1;
//            edgB 1 2 a2;
//            edgB 1 2 a3;
//            edgB 2 3 a4|] 
//        let qGraph = new BioParserInputGraph<_>([|0|], 3, len edges, edges, 4)
//        
//
//        test GLL.ParseSimpleBranch.buildAbstractAst qGraph 100 GLL.ParseSimpleBranch.numToString "SimpleBranch.dot" 14 15 5 1 GLL.ParseSimpleBranch.tokenData GLL.ParseSimpleBranch.tokenToNumber

//    [<Test>]
//    member this.``TSQL performance test for GLL`` () =  
//        let graphGenerator numberOfBlocks numberOfPath =
//            let final = 100
//            let qGraph = new ParserInputGraph<_>(0, final)
//            let mutable b = 1
//            let mutable e = 2
//            let mutable curB = 1
//            let mutable curE = 3
//            let chains = Array.zeroCreate 5
//            let ra1 = new ResizeArray<_>()
//            ra1.Add(GLL.MsSqlParser.DEC_NUMBER (0))
//            ra1.Add(GLL.MsSqlParser.L_plus_ (1))
//            ra1.Add(GLL.MsSqlParser.IDENT (2))
//            let ra2 = new ResizeArray<_>()
//            ra2.Add(GLL.MsSqlParser.IDENT (3))
//            ra2.Add(GLL.MsSqlParser.L_plus_ (4))
//            ra2.Add(GLL.MsSqlParser.IDENT (5))
//            let ra3 = new ResizeArray<_>()
//            ra3.Add(GLL.MsSqlParser.L_left_bracket_ (6))
//            ra3.Add(GLL.MsSqlParser.IDENT (7))
//            ra3.Add(GLL.MsSqlParser.L_plus_ (8))
//            ra3.Add(GLL.MsSqlParser.IDENT (9))
//            ra3.Add(GLL.MsSqlParser.L_right_bracket_ (10))
//            let ra4 = new ResizeArray<_>()
//            ra4.Add(GLL.MsSqlParser.L_null (11))
//            ra4.Add(GLL.MsSqlParser.L_null (12))
//            let ra5 = new ResizeArray<_>()
//            ra5.Add(GLL.MsSqlParser.STRING_CONST (13))
//            ra5.Add(GLL.MsSqlParser.L_plus_ (14))
//            ra5.Add(GLL.MsSqlParser.IDENT (15))
//            chains.[0] <- ra1
//            chains.[1] <- ra2
//            chains.[2] <- ra3
//            chains.[3] <- ra4
//            chains.[4] <- ra5    
//            (qGraph.AddVerticesAndEdge <| edg 0 1 (GLL.MsSqlParser.L_select (16))) |> ignore
//            for blocks = 0 to numberOfBlocks - 1 do
//                for i = 0 to numberOfPath - 1 do
//                    let curChain = chains.[i]
//                    for k = 0 to curChain.Count - 1 do
//                        if k <> curChain.Count - 1 then
//                            qGraph.AddVerticesAndEdge <| edg curB curE (curChain.[k]) |> ignore  
//                            curB <- curE
//                            curE <- curE + 1
//                        else
//                            qGraph.AddVerticesAndEdge <| edg curB e (curChain.[k]) |> ignore
//                            if i <> numberOfPath - 1 then
//                                curE <- curE
//                                curB <- b
//                if blocks <> numberOfBlocks - 1 then
//                    b <- e
//                    e <- curE               
//                    qGraph.AddVerticesAndEdge <| edg b e (GLL.MsSqlParser.L_comma_ (17)) |> ignore
//                    b <- e
//                    e <- e + 1
//                    curB <- b
//                    curE <- e + 1
//            b <- e
//            e <- curE               
//            qGraph.AddVerticesAndEdge <| edg b e (GLL.MsSqlParser.L_from (18)) |> ignore
//            b <- e
//            e <- e + 1
//            qGraph.AddVerticesAndEdge <| edg b e (GLL.MsSqlParser.IDENT (19)) |> ignore
//            b <- e
//            e <- e + 1
//            qGraph.AddVerticesAndEdge <| edg b e (GLL.MsSqlParser.RNGLR_EOF (20)) |> ignore
//            qGraph.FinalStates <- [|e|]
//            //qGraph.PrintToDot "input.dot" (GLL.MsSqlParser.tokenToNumber >> GLL.MsSqlParser.numToString)
//            qGraph
//
//        let parse = GLL.MsSqlParser.buildAbstractAst
//        perfTest2 parse graphGenerator
//  
    member this.``1000: trna`` file lengthLimit expectedRange =
        let start = System.DateTime.Now
        let processRes res = 
            filterRnaParsingResult res expectedRange 60        

        let getSmb =
            let cnt = ref 0
            fun ch ->
                let i = incr cnt; !cnt 
                match ch with
                | 'A' -> GLL.tRNA.A i                
                | 'U' -> GLL.tRNA.U i
                | 'T' -> GLL.tRNA.U i
                | 'C' -> GLL.tRNA.C i
                | 'G' -> GLL.tRNA.G i                
                | x ->   failwithf "Strange symbol in input: %A" x
                |> GLL.tRNA.tokenToNumber                
        let basePath = "../../../Tests/bio/"
        let path = Path.Combine(basePath, file)
        let graphs,longEdges = YC.BIO.BioGraphLoader.loadGraphFormFileToBioParserInputGraph path lengthLimit getSmb (GLL.tRNA.RNGLR_EOF 0)

        graphs        
        |> Array.ofSeq        
        |> Array.mapi 
            (fun i graph -> 
                GLL.tRNA.buildAbstract graph 5
            )
        |> Array.iter processRes
        printfn "Time = %A" (System.DateTime.Now - start)

    [<Test>]
    member this.``1000: trna in 860-930`` () =
        this.``1000: trna`` """simple_tRNA1\g""" 1001 ((0,863),(0,926))

    [<Test>]
    member this.``1000: trna in 629-699`` () =
        this.``1000: trna`` """simple_tRNA2\g""" 1001 ((0,596),(0,697))

    [<Test>]
    member this.``1000: trna in 133-204`` () =
        this.``1000: trna`` """simple_tRNA3\g""" 1001 ((0,127),(0,205))

    [<Test>]
    member this.``1000 as graph 139 + 861: trna in 133-204`` () =
        this.``1000: trna`` """simple_tRNA4\g""" 1001 ((0,133),(0,204))

    [<Test>]
    member this.``1000 as graph 49 + 5: trna in 133-204`` () =
        this.``1000: trna`` """simple_tRNA6\g""" 1001 ((0,133),(0,204))

    //[<Test>]
    member this.``Problem with shift. Big`` () =
        this.``1000: trna`` """simple_tRNA5\g""" 1001 ((0,34),(0,99))


    member this.SmallTests file expectedRange =        
        let lengthLimit = 1001        
        let getSmb =
            let cnt = ref 0
            fun ch ->
                let i = incr cnt; !cnt 
                match ch with
                | 'A' -> GLL.shiftProblem.A i                
                | 'C' -> GLL.shiftProblem.C i
                | 'G' -> GLL.shiftProblem.G i                
                | x ->   failwithf "Strange symbol in input: %A" x
                |> GLL.shiftProblem.tokenToNumber                
        let basePath = "../../../Tests/bio/"
        let path = Path.Combine(basePath, file)
        let graphs,longEdges = YC.BIO.BioGraphLoader.loadGraphFormFileToBioParserInputGraph path lengthLimit getSmb (GLL.shiftProblem.RNGLR_EOF 0)

        graphs
        |> Array.ofSeq
        |> Array.mapi 
            (fun i graph -> 
                printfn "%A" i
                GLL.shiftProblem.buildAbstract graph 1
            )
        |> Array.iter (fun res -> filterRnaParsingResult res expectedRange 5)        

    [<Test>]
    member this.``Very small`` () =        
        this.SmallTests  """very_small\g"""  ((0,1),(0,6))

    [<Test>]
    member this.``Very small with 2 edges`` () =        
        this.SmallTests  """very_small_multy_1\g"""  ((0,1),(1,3))
    
    [<Test>]
    member this.``shift_small`` () =        
        this.SmallTests  """problem_with_shift_2\g"""  ((0,1),(1,3))
    
    
    member this.``Very very small tests`` file expectedRange =
        
        let getSmb =
            let cnt = ref 0
            fun ch ->
                let i = incr cnt; !cnt 
                match ch with
                | 'A' -> GLL.VeryVerySmall.A i                
                | 'C' -> GLL.VeryVerySmall.C i
                | 'G' -> GLL.VeryVerySmall.G i                
                | x ->   failwithf "Strange symbol in input: %A" x
                |> GLL.VeryVerySmall.tokenToNumber                
        let basePath = "../../../Tests/bio/"
        let path = Path.Combine(basePath, file)
        let graphs,longEdges = YC.BIO.BioGraphLoader.loadGraphFormFileToBioParserInputGraph path 5 getSmb (GLL.VeryVerySmall.RNGLR_EOF 0)

        graphs
        |> Array.ofSeq
        |> Array.mapi 
            (fun i graph -> 
                printfn "%A" i
                GLL.VeryVerySmall.buildAbstract graph 3
            )
        |> Array.iter (fun res -> filterRnaParsingResult res expectedRange 3) 

    [<Test>]
    member this.``Very very small with 2 edges`` () =
        this.``Very very small tests`` """v_very_small_multy\g""" ((0,0),(1,2))        

    [<Test>]
    member this.``Very very small`` () =
        this.``Very very small tests`` """v_very_small\g""" ((0,0),(0,3))        


    [<Test>]
    member this.``Problem with multiple edges. Small`` () =
        let file = """problem_with_multiple_edges\g""" 
        let lengthLimit = 1001 
        let expectedRange = ((0,0),(0,4))
        let getSmb =
            let cnt = ref 0
            fun ch ->
                let i = incr cnt; !cnt 
                match ch with
                | 'A' -> GLL.multipleEdgesProblem.A i                
                | 'C' -> GLL.multipleEdgesProblem.C i            
                | x ->   failwithf "Strange symbol in input: %A" x
                |> GLL.multipleEdgesProblem.tokenToNumber                
        let basePath = "../../../Tests/bio/"
        let path = Path.Combine(basePath, file)
        let graphs,longEdges = YC.BIO.BioGraphLoader.loadGraphFormFileToBioParserInputGraph path lengthLimit getSmb (GLL.multipleEdgesProblem.RNGLR_EOF 0)

        graphs
        |> Array.ofSeq
        |> Array.mapi 
            (fun i graph -> 
                printfn "%A" i
                GLL.multipleEdgesProblem.buildAbstract graph 1
            )
        |> Array.iter (fun res -> filterRnaParsingResult res expectedRange 5)        

    

   // [<Test>]
    member this.``Big for tRNA 1`` () =
        this.``1000: trna`` """mix_1\late_pair_info_count""" 120 ((0,860),(0,930))

    member this.``Big for tRNA 2`` () =
        this.``1000: trna`` """synth_1\graph""" 120 ((0,860),(0,930))

        
[<EntryPoint>]
let fs x =
    //System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.LowLatency
    let t = new ``GLL abstract parser tests``()
    //let f () = t.``TSQL performance test for GLL`` ()
              //_35_Expression() //
    //let th = new System.Threading.Thread(f, 10000000)
    //th.Start()
    //t.bio2_5()
    //t.``1000: trna in 860-930``()
    //t.``1000: trna in 629-699``()
    //t.``1000: trna in 133-204``()
    //t.``1000 as graph 139 + 861: trna in 133-204`` ()
    //t.``Very small``()
    //t.``Very small with 2 edges``()
    //t.``Very very small with 2 edges``()
    //t.``Very very small``()
    //t.``Problem with shift. Big`` ()
    t.``shift_small`` ()
    
    //t.``Problem with shift. Small 2``()
    
    //t.``Problem with multiple edges. Small``()
    //t.``Big for tRNA 2``()
    //t.``Problem with shift. Small``()
    0