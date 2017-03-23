﻿//   Copyright 2013, 2014 YaccConstructor Software Foundation
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

let needChangeDirectory = 
    @"C:\Users\Artem Gorokhov\AppData\Local\JetBrains\Installations\ReSharperPlatformVs14" = Directory.GetCurrentDirectory()

let outputDir = ""//@"../../../src/GLL.AbstractParser.SimpleTest/"

let dataDir = 
    if needChangeDirectory
    then @"C:/Code/YaccConstructor/tests/data/AbstractGLL/"
    else @"./data/AbstractGLL/"
let grammarsDir = 
    if needChangeDirectory
    then @"C:/Code/YaccConstructor/tests/GLL.AbstractParser.Simple.Tests/"
    else @"./GLL.AbstractParser.Simple.Tests/"


let lbl tokenId = tokenId
let edg f t l = new ParserEdge<_>(f,t,lbl l)

let rnd = new System.Random()

let getInputGraph tokenizer inputFile =    
    let edges = 
        File.ReadAllLines (dataDir + inputFile)
        |> Array.filter(fun x -> not (x = ""))
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
    //printfn "%A" needChangeDirectory
    let parser = getParserSource grammarFile
    let input  = getInputGraph parser.StringToToken inputFile
    let tree = buildAst parser input
    //printfn "%A" tree
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
        test "PrettySimpleCalc.yrd" 
             "PrettySimpleCalc.txt"
             15 14 3 0
    
    [<Test>]  
    member this._02_SimpleRec_1length () =
        test "SimpleRec.yrd" 
             "SimpleRec1.txt"
             6 7 1 1

    [<Test>]  
    member this._03_SimpleRec_2length () =
        test "SimpleRec.yrd" 
             "SimpleRec2.txt"
             9 10 2 1

    [<Test>]
    member this._06_NotAmbigousSimpleCalc_Loop () =
        test "NotAmbigousSimpleCalc.yrd" 
             "NotAmbigousSimpleCalc_Loop.txt"
             17 18 4 1
    [<Test>]
    member this._07_NotAmbigousSimpleCalc_LoopInLoop () =
        test "NotAmbigousSimpleCalc.yrd" 
             "NotAmbigousSimpleCalc_LoopInLoop.txt"
             27 29 7 2 
        
    [<Test>]
    member this._14_NotAmbigousSimpleCalcWith2Ops_Loop () =
        test "NotAmbigousSimpleCalcWith2Ops.yrd" 
             "NotAmbigousSimpleCalcWith2Ops_Loop.txt"
             28 28 7 1

    [<Test>]
    member this._15_NotAmbigousSimpleCalcWith2Ops_Loops () =
        test "NotAmbigousSimpleCalcWith2Ops.yrd" 
             "NotAmbigousSimpleCalcWith2Ops_Loops.txt"
             34 34 9 1

    [<Test>]
    member this._16_Stars_Loop () =
        test "Stars.yrd" 
             "Stars_Loop.txt"
             13 16 2 1
        
    [<Test>]
    member this._17_Stars2_Loop () =
        test "Stars2.yrd" 
             "Stars2_Loop.txt"
             17 23 1 1
        
    [<Test>]
    member this._19_FirstEps () =
        test "FirstEps.yrd" 
             "FirstEps.txt"
             10 9 2 0
        
    [<Test>]
    member this._20_CroppedBrackets () =
        test "CroppedBrackets.yrd" 
             "CroppedBrackets.txt"
             8 8 3 1

    [<Test>]
    member this._21_Brackets () =
        test "Brackets.yrd" 
             "Brackets.txt"
             8 8 3 1  

    [<Test>]
    member this._22_Brackets_BackEdge () =
        test "Brackets.yrd" 
             "Brackets_BackEdge.txt"
             25 38 4 4


    [<Test>]
    member this._24_UnambiguousBrackets_Circle () =
        test "StrangeBrackets.yrd" 
             "StrangeBrackets.txt"
             13 13 4 1

    //the tests below are not checked
    [<Test>]
    member this._25_UnambiguousBrackets_BiggerCircle () =
        test "StrangeBrackets.yrd" 
             "StrangeBrackets2.txt"
             17 15 5 0

    [<Test>]
    member this._26_UnambiguousBrackets_Inf () =
        test "StrangeBrackets.yrd" 
             "StrangeBrackets3.txt"
             10 11 3 1
    
    [<Test>]
    member this._29_Attrs () =
        test "Attrs.yrd" 
             "Attrs.txt"
             15 14 5 0

    [<Test>]
    member this._30_Condition () =
        test "Cond.yrd" 
             "Cond.txt"
             42 47 5 1

    [<Test>]
    member this._31_Counter () =
        test "Counter.yrd" 
             "Counter.txt"
             35 40 5 0
    
    [<Test>]
    member this._32_Cycle () =
        test "Cycle.yrd" 
             "Cycle.txt"
             7 7 2 1

    [<Test>]
    member this._33_Epsilon2_with_eps2_yrd () =
        test "Eps2.yrd" 
             "Eps2.txt"
             10 9 2 0

//    [<Test>]
//    member this._34_Epsilon () =
//        test "Epsilon.yrd" 
//             "Epsilon.txt"
//             30 33 9 1
//        let qGraph = new rInputGraph<_>(0, 1)
//        qGraph.AddVerticesAndEdgeRange
//           [edg 0 1 (GLL.Epsilon.RNGLR_EOF 0)
//            ] |> ignore
//
//        test GLL.Epsilon.buildAbstractAst qGraph GLL.Epsilon.numToString "Epsilon.dot" 21 24 5 0 GLL.Epsilon.tokenData GLL.Epsilon.tokenToNumber
        
    [<Test>]
    member this._35_Expression () =
        test "Expr.yrd" 
             "Expr.txt"
             24 27 5 1

    [<Test>]
    member this._36_First () =
        test "First.yrd" 
             "First.txt"
             15 14 5 0

    [<Test>]
    member this._37_ListEps () =
        test "ListEps.yrd" 
             "ListEps.txt"
             24 24 5 1

    [<Test>]
    member this._38_LolCalc () =
        test "LolCalc.yrd" 
             "LolCalc.txt"
             115 174 11 10

    [<Test>]
    member this._39_LongCycle () =
        test "LongCycle.yrd" 
             "LongCycle.txt"
             6 6 1 1

    [<Test>]
    member this._41_Longest () =
        test "Longest.yrd" 
             "Longest.txt"
             24 25 6 0
         
    [<Test>]
    member this._42_Mixed () =
        test "Mixed.yrd" 
             "Mixed.txt"
             16 16 4 0

    [<Test>]
    member this._43_Omit () =
        test "Omit.yrd" 
             "Omit.txt"
             22 20 4 0

    [<Test>]
    member this._44_Order () =
        test "Order.yrd" 
             "Order.txt"
             38 37 8 0

    [<Test>]
    member this._45_SimpleRightRecursion () =
        test "SimpleRightRecursion.yrd" 
             "SimpleRightRecursion.txt"
             15 15 3 0


    [<Test>]
    member this._46_BadLeftRecursion () =
        test "BadLeftRecursion.yrd" 
             "BadLeftRecursion.txt"
             19 24 3 1

    [<Test>]
    member this._47_SimpleAmb () =
        test "SimpleAmb.yrd" 
             "SimpleAmb.txt"
             10 11 3 1

    [<Test>]
    member this._48_SimpleRightNull () =
        test "SimpleRightNull.yrd" 
             "SimpleRightNull.txt"
             27 28 5 0

    [<Test>]
    member this._49_SimpleLeftRecursion () =
        test "SimpleLeftRecursion.yrd" 
             "SimpleLeftRecursion.txt"
             9 8 3 0

    [<Test>]
    member this._50_SimpleBranch () =
        test "SimpleBranch.yrd" 
             "SimpleBranch.txt"
             4 3 2 0
//        
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
//
//
//               t.PerformanceTestLinearBadLeftRec()
//               t.PerformanceTestLinearUnambBraces()
//               t.PerformanceTestLinearAmbBraces()

