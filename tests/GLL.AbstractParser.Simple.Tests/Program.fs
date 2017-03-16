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
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon
open YaccConstructor.API

open System.Collections.Generic
open System.Linq
//open GLL.ParseOrder

let outputDir = ""//@"../../../src/GLL.AbstractParser.SimpleTest/"

//let dataDir = @"../../../data/AbstractGLL/"
//let grammarsDir = @"../../"

let dataDir = @"C:/Code/YaccConstructor/tests/data/AbstractGLL/"
let grammarsDir = @"C:/Code/YaccConstructor/tests/GLL.AbstractParser.Simple.Tests/"

let lbl tokenId = tokenId
let edg f t l = new ParserEdge<_>(f,t,lbl l)

let rnd = new System.Random()

let getInputGraph tokenizer inputFile =    
    let edges = 
        File.ReadAllLines (dataDir + inputFile)
        |> Array.map (fun s -> let x = s.Split([|' '|])
                               (int x.[0]), (int x.[1]), x.[2])
    let edg (f : int) (t : string) (l : int) = 
        new TaggedEdge<_,_>(f, l, tokenizer (t.ToUpper())) 
      
    let g = new SimpleGraphInput<_>([|0<positionInInput>|], id)
    
    [|for (first,last,tag) in edges -> edg first tag last |]
    |> g.AddVerticesAndEdgeRange
    |> ignore
    
    g 

let getParserSource grammarFile =    
    generate (grammarsDir + grammarFile)
             "YardFrontend" "GLLGenerator" 
             None
             ["ExpandMeta"]
             [] :?> ParserSourceGLL

let test grammarFile inputFile nodesCount edgesCount termsCount ambiguityCount = 
    let parser = getParserSource grammarFile
    let input  = getInputGraph parser.StringToToken inputFile
    let tree = buildAst parser input
    printfn "%A" tree
    tree.AstToDot parser.IntToString (grammarsDir + inputFile + ".dot")
    let n, e, t, amb = tree.CountCounters
    //printfn "%d %d %d %d" n e t amb
    Assert.AreEqual(nodesCount, n, sprintf "Nodes expected:%i, found:%i." nodesCount n)
    Assert.AreEqual(edgesCount, e, sprintf "Edges expected:%i, found:%i." edgesCount e)
    Assert.AreEqual(termsCount, t, sprintf "Terms expected:%i, found:%i." termsCount t) 
    Assert.AreEqual(ambiguityCount, amb, sprintf "Ambiguities expected:%i, found:%i." ambiguityCount amb)
    Assert.Pass()
      
[<TestFixture>]
type ``GLL abstract parser tests``() =
    [<Test>]  
    member this._01_PrettySimpleCalc_SequenceInput () =
        Assert.Pass()
//        test "PrettySimpleCalc.yrd" 
//             "PrettySimpleCalc.txt"
//             15 14 3 0
    
//    [<Test>]  
//    member this._02_SimpleRec_1length () =
//        test "SimpleRec.yrd" 
//             "SimpleRec1.txt"
//             15 14 3 0
//
//    [<Test>]  
//    member this._03_SimpleRec_2length () =
//        test "SimpleRec.yrd" 
//             "SimpleRec2.txt"
//             15 14 3 0
//
//    [<Test>]
//    member this._06_NotAmbigousSimpleCalc_Loop () =
//        test "NotAmbigousSimpleCalc.yrd" 
//             "NotAmbigousSimpleCalc_Loop.txt"
//             25 30 6 1
//    [<Test>]
//    member this._07_NotAmbigousSimpleCalc_LoopInLoop () =
//        test "NotAmbigousSimpleCalc.yrd" 
//             "NotAmbigousSimpleCalc_LoopInLoop.txt"
//            39 48 9 2 
//        
//
//    [<Test>]
//    member this._14_NotAmbigousSimpleCalcWith2Ops_Loop () =
//        test "NotAmbigousSimpleCalcWith2Ops.yrd" 
//             "NotAmbigousSimpleCalcWith2Ops_Loop.txt"
//             0 0 0 0
//
//    [<Test>]
//    member this._15_NotAmbigousSimpleCalcWith2Ops_Loops () =
//        test "NotAmbigousSimpleCalcWith2Ops.yrd" 
//             "NotAmbigousSimpleCalcWith2Ops_Loops.txt"
//             0 0 0 0
//
//    [<Test>]
//    member this._16_Stars_Loop () =
//        test "Stars.yrd" 
//             "Stars_Loop.txt"
//             19 24 4 1 
//        
//    [<Test>]
//    member this._17_Stars2_Loop () =
//        test "Stars2.yrd" 
//             "Stars2_Loop.txt"
//             23 33 3 2
//        
//    [<Test>]
//    member this._19_FirstEps () =
//        test "FirstEps.yrd" 
//             "FirstEps.txt"
//             26 30 6 0
//        
//    [<Test>]
//    member this._20_CroppedBrackets () =
//        test "CroppedBrackets.yrd" 
//             "CroppedBrackets.txt"
//             14 15 5 1
//
//    [<Test>]
//    member this._21_Brackets () =
//        test "Brackets.yrd" 
//             "Brackets.txt"
//             14 15 5 1  
//
//    [<Test>]
//    member this._22_Brackets_BackEdge () =
//        test "Brackets.yrd" 
//             "Brackets_BackEdge.txt"
//             35 54 6 4

//
//    [<Test>]
//    member this._24_UnambiguousBrackets_Circle () =
//        let qGraph = new ParserInputGraph<_>(0, 2)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.StrangeBrackets.LBR 0)
//            edg 1 0 (GLL.StrangeBrackets.RBR 1)
//            edg 0 2 (GLL.StrangeBrackets.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.StrangeBrackets.buildAbstractAst qGraph GLL.StrangeBrackets.numToString "StrangeBrackets2.dot" 19 21 6 1 GLL.StrangeBrackets.tokenData GLL.StrangeBrackets.tokenToNumber
//
//    [<Test>]
//    member this._25_UnambiguousBrackets_BiggerCircle () =
//        let qGraph = new ParserInputGraph<_>(0, 4)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.StrangeBrackets.LBR 0)
//            edg 1 2 (GLL.StrangeBrackets.RBR 1)
//            edg 2 3 (GLL.StrangeBrackets.LBR 2)
//            edg 3 0 (GLL.StrangeBrackets.RBR 3)
//            edg 0 4 (GLL.StrangeBrackets.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.StrangeBrackets.buildAbstractAst qGraph GLL.StrangeBrackets.numToString "StrangeBrackets3.dot" 30 33 9 1 GLL.StrangeBrackets.tokenData GLL.StrangeBrackets.tokenToNumber
//
//    [<Test>]
//    member this._26_UnambiguousBrackets_Inf () =
//        let qGraph = new ParserInputGraph<_>(0, 1)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 0 (GLL.StrangeBrackets.LBR 0)
//            edg 0 0 (GLL.StrangeBrackets.RBR 1)
//            edg 0 1 (GLL.StrangeBrackets.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.StrangeBrackets.buildAbstractAst qGraph GLL.StrangeBrackets.numToString "StrangeBrackets4.dot" 16 18 5 1 GLL.StrangeBrackets.tokenData GLL.StrangeBrackets.tokenToNumber
//
//    
//    [<Test>]
//    member this._29_Attrs () =
//        let qGraph = new ParserInputGraph<_>(0, 6)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseAttrs.A 1)
//            edg 1 2 (GLL.ParseAttrs.A 2)
//            edg 2 3 (GLL.ParseAttrs.A 3)
//            edg 3 4 (GLL.ParseAttrs.A 4)
//            edg 4 5 (GLL.ParseAttrs.A 5)
//            edg 5 6 (GLL.ParseAttrs.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseAttrs.buildAbstractAst qGraph GLL.ParseAttrs.numToString "Attrs.dot" 29 33 7 0 GLL.ParseAttrs.tokenData GLL.ParseAttrs.tokenToNumber
//
//    [<Test>]
//    member this._30_Condition () =
//        let qGraph = new ParserInputGraph<_>(0, 6)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseCond.IF 1)
//            edg 1 2 (GLL.ParseCond.IF 2)
//            edg 2 3 (GLL.ParseCond.A 3)
//            edg 3 4 (GLL.ParseCond.ELSE 4)
//            edg 4 5 (GLL.ParseCond.A 5)
//            edg 5 6 (GLL.ParseCond.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseCond.buildAbstractAst qGraph GLL.ParseCond.numToString "Cond.dot" 44 57 7 1 GLL.ParseCond.tokenData GLL.ParseCond.tokenToNumber
//
//    [<Test>]
//    member this._31_Counter () =
//        let qGraph = new ParserInputGraph<_>(0, 6)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseCounter.A 1)
//            edg 1 2 (GLL.ParseCounter.A 2)
//            edg 2 3 (GLL.ParseCounter.A 3)
//            edg 3 4 (GLL.ParseCounter.A 4)
//            edg 4 5 (GLL.ParseCounter.A 5)
//            edg 5 6 (GLL.ParseCounter.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseCounter.buildAbstractAst qGraph GLL.ParseCounter.numToString "Counter.dot" 21 21 7 0 GLL.ParseCounter.tokenData GLL.ParseCounter.tokenToNumber
//    
//    [<Test>]
//    member this._32_Cycle () =
//        let qGraph = new ParserInputGraph<_>(0, 3)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseCycle.A 1)
//            edg 1 2 (GLL.ParseCycle.B 2)
//            edg 2 3 (GLL.ParseCycle.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseCycle.buildAbstractAst qGraph GLL.ParseCycle.numToString "Cycle.dot" 15 18 4 1 GLL.ParseCycle.tokenData GLL.ParseCycle.tokenToNumber
//         
//    [<Test>]
//    member this._33_Epsilon2_with_eps2_yrd () =
//        let qGraph = new ParserInputGraph<_>(0, 3)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseEps2.Z 1)
//            edg 1 2 (GLL.ParseEps2.N 2)
//            edg 2 3 (GLL.ParseEps2.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseEps2.buildAbstractAst qGraph GLL.ParseEps2.numToString "Eps2.dot" 26 30 6 0 GLL.ParseEps2.tokenData GLL.ParseEps2.tokenToNumber
//
//    [<Test>]
//    member this._34_Epsilon () =
//        let qGraph = new ParserInputGraph<_>(0, 1)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseEpsilon.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseEpsilon.buildAbstractAst qGraph GLL.ParseEpsilon.numToString "Epsilon.dot" 21 24 5 0 GLL.ParseEpsilon.tokenData GLL.ParseEpsilon.tokenToNumber
//        
//    [<Test>]
//    member this._35_Expression () =
//        let qGraph = new ParserInputGraph<_>(0, 6)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseExpr.N 1)
//            edg 1 2 (GLL.ParseExpr.P 2)
//            edg 2 3 (GLL.ParseExpr.N 3)
//            edg 3 4 (GLL.ParseExpr.P 4)
//            edg 4 5 (GLL.ParseExpr.N 5)
//            edg 5 6 (GLL.ParseExpr.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseExpr.buildAbstractAst qGraph GLL.ParseExpr.numToString "Expr.dot" 36 45 7 1 GLL.ParseExpr.tokenData GLL.ParseExpr.tokenToNumber
//
//    [<Test>]
//    member this._36_First () =
//        let qGraph = new ParserInputGraph<_>(0, 6)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseFirst.A 1)
//            edg 1 2 (GLL.ParseFirst.A 2)
//            edg 2 3 (GLL.ParseFirst.A 3)
//            edg 3 4 (GLL.ParseFirst.A 4)
//            edg 4 5 (GLL.ParseFirst.B 5)
//            edg 5 6 (GLL.ParseFirst.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseFirst.buildAbstractAst qGraph GLL.ParseFirst.numToString "First.dot" 21 21 7 0 GLL.ParseFirst.tokenData GLL.ParseFirst.tokenToNumber
//
//    [<Test>]
//    member this._37_ListEps () =
//        let qGraph = new ParserInputGraph<_>(0, 3)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseListEps.NUM 1)
//            edg 1 2 (GLL.ParseListEps.NUM 2)
//            edg 2 3 (GLL.ParseListEps.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseListEps.buildAbstractAst qGraph GLL.ParseListEps.numToString "ListEps.dot" 72 93 13 3 GLL.ParseListEps.tokenData GLL.ParseListEps.tokenToNumber
//    
//    [<Test>]
//    member this._38_LolCalc () =
//        let qGraph = new ParserInputGraph<_>(0, 12)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseLolCalc.A 1)
//            edg 1 2 (GLL.ParseLolCalc.MUL 2)
//            edg 2 3 (GLL.ParseLolCalc.B 3)
//            edg 3 4 (GLL.ParseLolCalc.ADD 4)
//            edg 4 5 (GLL.ParseLolCalc.A 5)
//            edg 5 6 (GLL.ParseLolCalc.MUL 6)
//            edg 6 7 (GLL.ParseLolCalc.B 7)
//            edg 7 8 (GLL.ParseLolCalc.ADD 8)
//            edg 8 9 (GLL.ParseLolCalc.B 9)
//            edg 9 10 (GLL.ParseLolCalc.MUL 10)
//            edg 10 11 (GLL.ParseLolCalc.A 11)
//            edg 11 12 (GLL.ParseLolCalc.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseLolCalc.buildAbstractAst qGraph GLL.ParseLolCalc.numToString "LolCalc.dot" 0 0 0 0
//    
//    [<Test>]
//    member this._39_LongCycle () =
//        let qGraph = new ParserInputGraph<_>(0, 2)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseLongCycle.A 1)
//            edg 1 2 (GLL.ParseLongCycle.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseLongCycle.buildAbstractAst qGraph GLL.ParseLongCycle.numToString "LongCycle.dot" 14 18 3 1 GLL.ParseLongCycle.tokenData GLL.ParseLongCycle.tokenToNumber
//    
//    [<Test>]
//    member this._41_Longest () =
//        let qGraph = new ParserInputGraph<_>(0, 6)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseLongest.A 1)
//            edg 1 2 (GLL.ParseLongest.A 2)
//            edg 2 3 (GLL.ParseLongest.A 3)
//            edg 3 4 (GLL.ParseLongest.A 4)
//            edg 4 5 (GLL.ParseLongest.A 5)
//            edg 5 6 (GLL.ParseLongest.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseLongest.buildAbstractAst qGraph GLL.ParseLongest.numToString "Longest.dot" 91 123 14 1 GLL.ParseLongest.tokenData GLL.ParseLongest.tokenToNumber
//         
//    [<Test>]
//    member this._42_Mixed () =
//        let qGraph = new ParserInputGraph<_>(0, 5)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseMixed.B 1)
//            edg 1 2 (GLL.ParseMixed.A 2)
//            edg 2 3 (GLL.ParseMixed.B 3)
//            edg 3 4 (GLL.ParseMixed.A 4)
//            edg 4 5 (GLL.ParseMixed.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseMixed.buildAbstractAst qGraph GLL.ParseMixed.numToString "Mixed.dot" 24 27 6 0 GLL.ParseMixed.tokenData GLL.ParseMixed.tokenToNumber
//    
//    [<Test>]
//    member this._43_Omit () =
//        let qGraph = new ParserInputGraph<_>(0, 4)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseOmit.A 1)
//            edg 1 2 (GLL.ParseOmit.B 2)
//            edg 2 3 (GLL.ParseOmit.A 3)
//            edg 3 4 (GLL.ParseOmit.RNGLR_EOF 0)
//            ] |> ignore 
//
//        test GLL.ParseOmit.buildAbstractAst qGraph GLL.ParseOmit.numToString "Omit.dot" 26 30 6 0 GLL.ParseOmit.tokenData GLL.ParseOmit.tokenToNumber
//    
//    [<Test>]
//    member this._44_Order () =
//        let qGraph = new ParserInputGraph<_>(0, 9)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.ParseOrder.A 1)
//            edg 1 2 (GLL.ParseOrder.A 2)
//            edg 2 3 (GLL.ParseOrder.A 3)
//            edg 3 4 (GLL.ParseOrder.A 4)
//            edg 4 5 (GLL.ParseOrder.A 5)
//            edg 5 6 (GLL.ParseOrder.A 6)
//            edg 6 7 (GLL.ParseOrder.A 7)
//            edg 7 8 (GLL.ParseOrder.A 8)
//            edg 8 9 (GLL.ParseOrder.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.ParseOrder.buildAbstractAst qGraph GLL.ParseOrder.numToString "Order.dot"
//


//    [<Test>]
//    member this._45_SimpleRightRecursion () =
//        let qGraph = new ParserInputGraph<_>(0, 4)
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (GLL.SimpleRightRecursion.B 1)
//             edg 1 2 (GLL.SimpleRightRecursion.B 2)
//             edg 2 3 (GLL.SimpleRightRecursion.B 3)
//             edg 3 4 (GLL.SimpleRightRecursion.RNGLR_EOF 0)
//             ] |> ignore
//
//        test GLL.SimpleRightRecursion.buildAbstractAst qGraph GLL.SimpleRightRecursion.numToString "SimpleRightRecursion.dot" 15 15 5 0 GLL.SimpleRightRecursion.tokenData GLL.SimpleRightRecursion.tokenToNumber
//
//    [<Test>]
//    member this._46_BadLeftRecursion () =
//        let input = 
//            new LinearInput(
//                Array.map (fun x -> GLL.BadLeftRecursion.tokenToNumber.[x] * 1<token>)
//                    [|GLL.BadLeftRecursion.B 1;
//                      GLL.BadLeftRecursion.B 2;
//                      GLL.BadLeftRecursion.B 3|])
//
//        let res = isParsed GLL.BadLeftRecursion.parserSource input
//
//        shouldBeTrue res
//
//    [<Test>]
//    member this._47_SimpleAmb () =
//        let qGraph = new ParserInputGraph<_>(0, 4)
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (GLL.SimpleAmb.A 1)
//             edg 1 2 (GLL.SimpleAmb.D 2)
//             edg 2 3 (GLL.SimpleAmb.B 3)
//             edg 3 4 (GLL.SimpleAmb.RNGLR_EOF 0)
//             ] |> ignore
//
//        test GLL.SimpleAmb.buildAbstractAst qGraph GLL.SimpleAmb.numToString "SimpleAmb.dot" 18 21 5 1 GLL.SimpleAmb.tokenData GLL.SimpleAmb.tokenToNumber
//    
//    [<Test>]
//    member this._48_SimpleRightNull () =
//        let qGraph = new ParserInputGraph<_>(0, 3)
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (GLL.SimpleRightNull.A 1)
//             edg 1 2 (GLL.SimpleRightNull.A 1)
//             edg 2 3 (GLL.SimpleRightNull.RNGLR_EOF 0)
//             ] |> ignore
//
//        test GLL.SimpleRightNull.buildAbstractAst qGraph GLL.SimpleRightNull.numToString "SimpleRightNull.dot" 22 24 6 0 GLL.SimpleRightNull.tokenData GLL.SimpleRightNull.tokenToNumber
//
//    [<Test>]
//    member this._49_SimpleLeftRecursion () =
//        let qGraph = new ParserInputGraph<_>(0, 4)
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (GLL.SimpleLeftRecursion.B 1)
//             edg 1 2 (GLL.SimpleLeftRecursion.B 2)
//             edg 2 3 (GLL.SimpleLeftRecursion.B 3)
//             edg 3 4 (GLL.SimpleLeftRecursion.RNGLR_EOF 0)
//             ] |> ignore
//
//        test GLL.SimpleLeftRecursion.buildAbstractAst qGraph GLL.SimpleLeftRecursion.numToString "SimpleLeftRecursion.dot" 19 21 5 0 GLL.SimpleLeftRecursion.tokenData GLL.SimpleLeftRecursion.tokenToNumber

//    [<Test>]
//    member this._50_SimpleBranch () =
//        let input = 
//            new LinearInput(
//                Array.map (GLL.ParseSimpleBranch.tokenToNumber >> (fun x -> x * 1<token>))
//                    [|GLL.ParseSimpleBranch.Token.A 1;
//                      GLL.ParseSimpleBranch.Token.B 1|])
//
//        let res = isParsed GLL.ParseSimpleBranch.parserSource input
//
//        shouldBeTrue res
        
//[<EntryPoint>]
//let f x =
//    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.LowLatency
//    let t = new ``GLL abstract parser tests``()   
//
////         @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\1.1.ttl"
////         @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\wine.rdf"
////         @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\pizza.owl"
////         @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\foaf.rdf" 
////         @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\people_pets.rdf"
////        ] do
////
////            let f = 
////                YC.GLL.Abstarct.Tests.RDFPerformance.loadFromFile p
////                 
////            printfn "triples in %A: %A" (System.IO.Path.GetFileName p) f.Triples.Count
//    //YC.GLL.Abstarct.Tests.RDFPerformance.parse @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\foaf.rdf"
//    //YC.GLL.Abstarct.Tests.RDFPerformance.parse @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\wine.rdf"
//    YC.GLL.Abstarct.Tests.RDFPerformance.performTests()
//    0


//               t.PerformanceTestLinearBadLeftRec()
//               t.PerformanceTestLinearUnambBraces()
//               t.PerformanceTestLinearAmbBraces()

