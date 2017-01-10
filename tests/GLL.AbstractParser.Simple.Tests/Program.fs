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
open System.Collections.Generic
open System.Linq
//open GLL.ParseOrder

let outputDir = ""//@"../../../src/GLL.AbstractParser.SimpleTest/"

type elt = T of char | N of char

let parseGraphCYK (grph: ParserInputGraph<_>) =
    let grm = 
        [
            N 's', [T 'l'; N 'b']
            N 'b', [N 's'; T 'r']
            N 's', [N 's'; N 's']
            N 's', []
            N 'l', [T '(']
            N 'r', [T ')']
        ]
    let r = [for v in grph.Vertices -> (N 's', v , v)] @ [for e in grph.Edges do for n in [N 'l'; N 'r'] do yield (n, e.Source, e.Target)] |> System.Collections.Generic.HashSet<_>
    printfn "Start r size:%A" r.Count
    let _new = new System.Collections.Generic.HashSet<_>(r)
    let whileCount = ref 0
    let for1Count = ref 0
    let for2Count = ref 0
    let for3Count = ref 0
    while _new.Count <> 0 do
        incr whileCount
        let x = _new.First()
        _new.Remove x |> ignore
        let (_n,n,m) = x
        for (_m,n',n) in Seq.filter (fun (_,_,x) -> x = n) r do
            incr for1Count
            for (_n',_) in (grm |> List.filter (fun (_,r) -> r = [_m; _n])) do                
                if r.Contains((_n',n',m)) |> not
                then 
                    incr for2Count
                    _new.Add(_n',n',m)|>ignore
                    r.Add(_n',n',m)|>ignore
        for (_m,m,m') in Seq.filter (fun (_,_,x) -> x = n) r do
            for (_m',_) in (grm |> List.filter (fun (_,r) -> r = [_n; _m])) do
                if r.Contains((_m',n,m')) |> not
                then 
                    incr for3Count
                    _new.Add(_m',n,m')|>ignore
                    r.Add(_m',n,m')|>ignore


    printfn "While: %A" !whileCount
    printfn "For 1: %A" !for1Count
    printfn "For 2: %A" !for2Count
    printfn "For 3: %A" !for3Count
    r

let lbl tokenId = tokenId
let edg f t l = new ParserEdge<_>(f,t,lbl l)

let rnd = new System.Random()

//let newRandGraph (initialGraph:ParserInputGraph<_>) (tokens:array<_>) addEdges =
//    let vertices = initialGraph.VertexCount
//    for i in 0 .. addEdges - 1 do
//        let token = tokens.[rnd.Next(0,tokens.Length-1)]
//        let visited = Array.init vertices false
//        let fromV = rnd.Next(0,vertices-1)
//        let rec addEdg  =            
//            if initialGraph.OutEdges(fromV) |> Seq.exists(fun e -> e.Tag = token) |> not
//            then initialGraph.


let newRandomGraph vCount eCount (tokens:array<_>) eof =
    let result = new ParserInputGraph<_>([|0..vCount-1|], [|vCount|])
    for i in 0..vCount-1 do result.AddVerticesAndEdge(edg i vCount eof) |> ignore
    let rec addEdg added = 
        if added <> eCount
        then
            let token = tokens.[rnd.Next(0,tokens.Length) % tokens.Length]
            //printfn "T:%A" token
            let fromV = rnd.Next(0,vCount-1)
            if result.OutEdges(fromV) |> Seq.exists(fun e -> e.Tag = token) |> not
            then 
                result.AddVerticesAndEdge(edg fromV (rnd.Next(0,vCount-1)) token)
                |> ignore
                addEdg (added + 1)
            else addEdg added
    addEdg 0
    result

let newRandomGraph2 (initialGraph:ParserInputGraph<_>) eCount (tokens:array<_>) eof =
    let vCount = initialGraph.VertexCount
    let result = initialGraph    
    let rec addEdg added = 
        if added <> eCount
        then
            let token = tokens.[rnd.Next(0,tokens.Length) % tokens.Length]
            //printfn "T:%A" token
            let fromV = rnd.Next(0,vCount-1)
            if result.OutEdges(fromV) |> Seq.exists(fun e -> e.Tag = token) |> not
            then 
                result.AddVerticesAndEdge(edg fromV (rnd.Next(0,vCount-1)) token)
                |> ignore
                addEdg (added + 1)
            else addEdg added
    addEdg 0
    //printfn "V:%A E:%A" result.VertexCount result.EdgeCount
    result


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

let test buildAbstractAst qGraph (intToString : int -> string) (fileName : string) nodesCount edgesCount termsCount ambiguityCount tokenData tokenToNum = 
    let r = buildAbstractAst qGraph
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
     


[<TestFixture>]
type ``GLL abstract parser tests`` () =

    [<Test>]
    member this._01_PrettySimpleCalc_SequenceInput () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.PrettySimpleCalc.NUM 1)
             edg 1 2 (GLL.PrettySimpleCalc.PLUS 2)
             edg 2 3 (GLL.PrettySimpleCalc.NUM 3)
             edg 3 4 (GLL.PrettySimpleCalc.RNGLR_EOF 0)
             ] |> ignore

        test GLL.PrettySimpleCalc.buildAbstractAst qGraph GLL.PrettySimpleCalc.numToString "PrettySimpleCalcSeq.dot" 21 24 5 0 GLL.PrettySimpleCalc.tokenData GLL.PrettySimpleCalc.tokenToNumber

    [<Test>]
    member this._06_NotAmbigousSimpleCalc_Loop () =
        let qGraph = new ParserInputGraph<_>([|0|] , [|4|] )
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (GLL.NotAmbigousSimpleCalc.RNGLR_EOF 4)
             edg 3 0 (GLL.NotAmbigousSimpleCalc.PLUS 5)
             ] |> ignore
        
        test GLL.NotAmbigousSimpleCalc.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalc.numToString "NotAmbigousSimpleCalc2.dot" 25 30 6 1 GLL.NotAmbigousSimpleCalc.tokenData GLL.NotAmbigousSimpleCalc.tokenToNumber

    [<Test>]
    member this._07_NotAmbigousSimpleCalc_LoopInLoop () =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.NotAmbigousSimpleCalc.NUM  1)
             edg 1 2 (GLL.NotAmbigousSimpleCalc.PLUS 2)
             edg 2 3 (GLL.NotAmbigousSimpleCalc.NUM 3)
             edg 3 4 (GLL.NotAmbigousSimpleCalc.PLUS 4)
             edg 4 5 (GLL.NotAmbigousSimpleCalc.NUM 5)
             edg 5 0 (GLL.NotAmbigousSimpleCalc.PLUS 6)
             edg 5 2 (GLL.NotAmbigousSimpleCalc.STAR 7)
             edg 5 6 (GLL.NotAmbigousSimpleCalc.RNGLR_EOF 8)
             ] |> ignore
        
        test  
            GLL.NotAmbigousSimpleCalc.buildAbstractAst 
            qGraph 
            GLL.NotAmbigousSimpleCalc.numToString 
            "NotAmbigousSimpleCalcLoopLoop.dot" 
            39 48 9 2 
            GLL.NotAmbigousSimpleCalc.tokenData
            GLL.NotAmbigousSimpleCalc.tokenToNumber

//
//    [<Test>]
//    member this._14_NotAmbigousSimpleCalcWith2Ops_Loop () =
//        let qGraph = new ParserInputGraph<_>(0, 7)
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM  1)
//             edg 1 2 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 2)
//             edg 2 3 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 3)
//             edg 3 4 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 4)
//             edg 4 5 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 5)
//             edg 5 2 (GLL.NotAmbigousSimpleCalcWith2Ops.MULT 6)
//             edg 4 6 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 7)
//             edg 6 7 (GLL.NotAmbigousSimpleCalcWith2Ops.RNGLR_EOF 0)
//             ] |> ignore
//        
//        test GLL.NotAmbigousSimpleCalcWith2Ops.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalcWith2Ops.numToString "NotAmbigousSimpleCalcWith2Ops.dot" 0 0 0 0
//
//    [<Test>]
//    member this._15_NotAmbigousSimpleCalcWith2Ops_Loops () =
//        let qGraph = new ParserInputGraph<_>(0, 8)
//        qGraph.AddVerticesAndEdgeRange
//            [edg 0 1 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM  1)
//             edg 1 2 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 2)
//             edg 2 3 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 3)
//             edg 2 4 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 4)
//             edg 3 4 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 5)
//             edg 4 5 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 6)
//             edg 5 2 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 7)
//             edg 4 6 (GLL.NotAmbigousSimpleCalcWith2Ops.PLUS 8)
//             edg 6 7 (GLL.NotAmbigousSimpleCalcWith2Ops.NUM 9)
//             edg 7 8 (GLL.NotAmbigousSimpleCalcWith2Ops.RNGLR_EOF 0)
//             ] |> ignore
//        
//        test GLL.NotAmbigousSimpleCalcWith2Ops.buildAbstractAst qGraph GLL.NotAmbigousSimpleCalcWith2Ops.numToString "NotAmbigousSimpleCalcWith2Ops2.dot" 0 0 0 0
//
    [<Test>]
    member this._16_Stars_Loop () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (GLL.Stars.STAR 1)
             edg 0 1 (GLL.Stars.SEMI 2)
             edg 1 2 (GLL.Stars.RNGLR_EOF 3)
             ] |> ignore
        
        test 
            GLL.Stars.buildAbstractAst 
            qGraph 
            GLL.Stars.numToString 
            "Stars_Loop.dot" 
            19 24 4 1 
            GLL.Stars.tokenData
            GLL.Stars.tokenToNumber

    [<Test>]
    member this._17_Stars2_Loop () =
        let qGraph = new ParserInputGraph<_>(0, 1)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 0 (GLL.Stars2.STAR 1)
             edg 0 1 (GLL.Stars2.RNGLR_EOF 2)
             ] |> ignore
        
        test 
            GLL.Stars2.buildAbstractAst 
            qGraph 
            GLL.Stars2.numToString 
            "Stars2.dot" 
            23 33 3 2
            GLL.Stars2.tokenData
            GLL.Stars2.tokenToNumber

    [<Test>]
    member this._19_FirstEps () =
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.FirstEps.Z 1)
            edg 1 2 (GLL.FirstEps.N 2)
            edg 2 3 (GLL.FirstEps.RNGLR_EOF 3)
            ] |> ignore

        test 
            GLL.FirstEps.buildAbstractAst
            qGraph
            GLL.FirstEps.numToString 
            "FirstEps.dot" 
            26 30 6 0
            GLL.FirstEps.tokenData
            GLL.FirstEps.tokenToNumber
    
    [<Test>]
    member this._20_CroppedBrackets () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (GLL.CroppedBrackets.LBR 1)
            edg 0 1 (GLL.CroppedBrackets.NUM 2)
            edg 1 1 (GLL.CroppedBrackets.RBR 3)
            edg 1 2 (GLL.CroppedBrackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.CroppedBrackets.buildAbstractAst qGraph GLL.CroppedBrackets.numToString "CroppedBrackets.dot" 14 15 5 1 GLL.CroppedBrackets.tokenData GLL.CroppedBrackets.tokenToNumber

    [<Test>]
    member this._21_Brackets () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (GLL.Brackets.LBR 1)
            edg 0 1 (GLL.Brackets.NUM 2)
            edg 1 1 (GLL.Brackets.RBR 3)
            edg 1 2 (GLL.Brackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.Brackets.buildAbstractAst qGraph GLL.Brackets.numToString "Brackets.dot" 14 15 5 1 GLL.Brackets.tokenData GLL.Brackets.tokenToNumber    

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

        test GLL.Brackets.buildAbstractAst qGraph GLL.Brackets.numToString "Brackets_backEdge.dot" 35 54 6 4 GLL.Brackets.tokenData GLL.Brackets.tokenToNumber

    [<Test>]
    member this._24_UnambiguousBrackets_Circle () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.StrangeBrackets.LBR 0)
            edg 1 0 (GLL.StrangeBrackets.RBR 1)
            edg 0 2 (GLL.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.StrangeBrackets.buildAbstractAst qGraph GLL.StrangeBrackets.numToString "StrangeBrackets2.dot" 19 21 6 1 GLL.StrangeBrackets.tokenData GLL.StrangeBrackets.tokenToNumber

    [<Test>]
    member this._25_UnambiguousBrackets_BiggerCircle () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.StrangeBrackets.LBR 0)
            edg 1 2 (GLL.StrangeBrackets.RBR 1)
            edg 2 3 (GLL.StrangeBrackets.LBR 2)
            edg 3 0 (GLL.StrangeBrackets.RBR 3)
            edg 0 4 (GLL.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.StrangeBrackets.buildAbstractAst qGraph GLL.StrangeBrackets.numToString "StrangeBrackets3.dot" 30 33 9 1 GLL.StrangeBrackets.tokenData GLL.StrangeBrackets.tokenToNumber

    [<Test>]
    member this._26_UnambiguousBrackets_Inf () =
        let qGraph = new ParserInputGraph<_>(0, 1)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 0 (GLL.StrangeBrackets.LBR 0)
            edg 0 0 (GLL.StrangeBrackets.RBR 1)
            edg 0 1 (GLL.StrangeBrackets.RNGLR_EOF 0)
            ] |> ignore

        test GLL.StrangeBrackets.buildAbstractAst qGraph GLL.StrangeBrackets.numToString "StrangeBrackets4.dot" 16 18 5 1 GLL.StrangeBrackets.tokenData GLL.StrangeBrackets.tokenToNumber

    
    [<Test>]
    member this._29_Attrs () =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseAttrs.A 1)
            edg 1 2 (GLL.ParseAttrs.A 2)
            edg 2 3 (GLL.ParseAttrs.A 3)
            edg 3 4 (GLL.ParseAttrs.A 4)
            edg 4 5 (GLL.ParseAttrs.A 5)
            edg 5 6 (GLL.ParseAttrs.RNGLR_EOF 0)
            ] |> ignore

        test GLL.ParseAttrs.buildAbstractAst qGraph GLL.ParseAttrs.numToString "Attrs.dot" 29 33 7 0 GLL.ParseAttrs.tokenData GLL.ParseAttrs.tokenToNumber

    [<Test>]
    member this._30_Condition () =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseCond.IF 1)
            edg 1 2 (GLL.ParseCond.IF 2)
            edg 2 3 (GLL.ParseCond.A 3)
            edg 3 4 (GLL.ParseCond.ELSE 4)
            edg 4 5 (GLL.ParseCond.A 5)
            edg 5 6 (GLL.ParseCond.RNGLR_EOF 0)
            ] |> ignore

        test GLL.ParseCond.buildAbstractAst qGraph GLL.ParseCond.numToString "Cond.dot" 44 57 7 1 GLL.ParseCond.tokenData GLL.ParseCond.tokenToNumber

    [<Test>]
    member this._31_Counter () =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseCounter.A 1)
            edg 1 2 (GLL.ParseCounter.A 2)
            edg 2 3 (GLL.ParseCounter.A 3)
            edg 3 4 (GLL.ParseCounter.A 4)
            edg 4 5 (GLL.ParseCounter.A 5)
            edg 5 6 (GLL.ParseCounter.RNGLR_EOF 0)
            ] |> ignore

        test GLL.ParseCounter.buildAbstractAst qGraph GLL.ParseCounter.numToString "Counter.dot" 21 21 7 0 GLL.ParseCounter.tokenData GLL.ParseCounter.tokenToNumber
    
    [<Test>]
    member this._32_Cycle () =
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseCycle.A 1)
            edg 1 2 (GLL.ParseCycle.B 2)
            edg 2 3 (GLL.ParseCycle.RNGLR_EOF 0)
            ] |> ignore

        test GLL.ParseCycle.buildAbstractAst qGraph GLL.ParseCycle.numToString "Cycle.dot" 15 18 4 1 GLL.ParseCycle.tokenData GLL.ParseCycle.tokenToNumber
         
    [<Test>]
    member this._33_Epsilon2_with_eps2_yrd () =
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseEps2.Z 1)
            edg 1 2 (GLL.ParseEps2.N 2)
            edg 2 3 (GLL.ParseEps2.RNGLR_EOF 0)
            ] |> ignore

        test GLL.ParseEps2.buildAbstractAst qGraph GLL.ParseEps2.numToString "Eps2.dot" 26 30 6 0 GLL.ParseEps2.tokenData GLL.ParseEps2.tokenToNumber

    [<Test>]
    member this._34_Epsilon () =
        let qGraph = new ParserInputGraph<_>(0, 1)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseEpsilon.RNGLR_EOF 0)
            ] |> ignore

        test GLL.ParseEpsilon.buildAbstractAst qGraph GLL.ParseEpsilon.numToString "Epsilon.dot" 21 24 5 0 GLL.ParseEpsilon.tokenData GLL.ParseEpsilon.tokenToNumber
        
    [<Test>]
    member this._35_Expression () =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseExpr.N 1)
            edg 1 2 (GLL.ParseExpr.P 2)
            edg 2 3 (GLL.ParseExpr.N 3)
            edg 3 4 (GLL.ParseExpr.P 4)
            edg 4 5 (GLL.ParseExpr.N 5)
            edg 5 6 (GLL.ParseExpr.RNGLR_EOF 0)
            ] |> ignore

        test GLL.ParseExpr.buildAbstractAst qGraph GLL.ParseExpr.numToString "Expr.dot" 36 45 7 1 GLL.ParseExpr.tokenData GLL.ParseExpr.tokenToNumber

    [<Test>]
    member this._36_First () =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseFirst.A 1)
            edg 1 2 (GLL.ParseFirst.A 2)
            edg 2 3 (GLL.ParseFirst.A 3)
            edg 3 4 (GLL.ParseFirst.A 4)
            edg 4 5 (GLL.ParseFirst.B 5)
            edg 5 6 (GLL.ParseFirst.RNGLR_EOF 0)
            ] |> ignore

        test GLL.ParseFirst.buildAbstractAst qGraph GLL.ParseFirst.numToString "First.dot" 21 21 7 0 GLL.ParseFirst.tokenData GLL.ParseFirst.tokenToNumber

    [<Test>]
    member this._37_ListEps () =
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseListEps.NUM 1)
            edg 1 2 (GLL.ParseListEps.NUM 2)
            edg 2 3 (GLL.ParseListEps.RNGLR_EOF 0)
            ] |> ignore

        test GLL.ParseListEps.buildAbstractAst qGraph GLL.ParseListEps.numToString "ListEps.dot" 72 93 13 3 GLL.ParseListEps.tokenData GLL.ParseListEps.tokenToNumber
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
    [<Test>]
    member this._39_LongCycle () =
        let qGraph = new ParserInputGraph<_>(0, 2)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseLongCycle.A 1)
            edg 1 2 (GLL.ParseLongCycle.RNGLR_EOF 0)
            ] |> ignore

        test GLL.ParseLongCycle.buildAbstractAst qGraph GLL.ParseLongCycle.numToString "LongCycle.dot" 14 18 3 1 GLL.ParseLongCycle.tokenData GLL.ParseLongCycle.tokenToNumber
    
    [<Test>]
    member this._41_Longest () =
        let qGraph = new ParserInputGraph<_>(0, 6)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseLongest.A 1)
            edg 1 2 (GLL.ParseLongest.A 2)
            edg 2 3 (GLL.ParseLongest.A 3)
            edg 3 4 (GLL.ParseLongest.A 4)
            edg 4 5 (GLL.ParseLongest.A 5)
            edg 5 6 (GLL.ParseLongest.RNGLR_EOF 0)
            ] |> ignore

        test GLL.ParseLongest.buildAbstractAst qGraph GLL.ParseLongest.numToString "Longest.dot" 91 123 14 1 GLL.ParseLongest.tokenData GLL.ParseLongest.tokenToNumber
         
    [<Test>]
    member this._42_Mixed () =
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseMixed.B 1)
            edg 1 2 (GLL.ParseMixed.A 2)
            edg 2 3 (GLL.ParseMixed.B 3)
            edg 3 4 (GLL.ParseMixed.A 4)
            edg 4 5 (GLL.ParseMixed.RNGLR_EOF 0)
            ] |> ignore

        test GLL.ParseMixed.buildAbstractAst qGraph GLL.ParseMixed.numToString "Mixed.dot" 24 27 6 0 GLL.ParseMixed.tokenData GLL.ParseMixed.tokenToNumber
    
    [<Test>]
    member this._43_Omit () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.ParseOmit.A 1)
            edg 1 2 (GLL.ParseOmit.B 2)
            edg 2 3 (GLL.ParseOmit.A 3)
            edg 3 4 (GLL.ParseOmit.RNGLR_EOF 0)
            ] |> ignore 

        test GLL.ParseOmit.buildAbstractAst qGraph GLL.ParseOmit.numToString "Omit.dot" 26 30 6 0 GLL.ParseOmit.tokenData GLL.ParseOmit.tokenToNumber
    
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

    [<Test>]
    member this._45_SimpleRightRecursion () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.SimpleRightRecursion.B 1)
             edg 1 2 (GLL.SimpleRightRecursion.B 2)
             edg 2 3 (GLL.SimpleRightRecursion.B 3)
             edg 3 4 (GLL.SimpleRightRecursion.RNGLR_EOF 0)
             ] |> ignore

        test GLL.SimpleRightRecursion.buildAbstractAst qGraph GLL.SimpleRightRecursion.numToString "SimpleRightRecursion.dot" 15 15 5 0 GLL.SimpleRightRecursion.tokenData GLL.SimpleRightRecursion.tokenToNumber

    [<Test>]
    member this._46_BadLeftRecursion () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.BadLeftRecursion.B 1)
             edg 1 2 (GLL.BadLeftRecursion.B 2)
             edg 2 3 (GLL.BadLeftRecursion.B 3)
             edg 3 4 (GLL.BadLeftRecursion.RNGLR_EOF 0)
             ] |> ignore

        test GLL.BadLeftRecursion.buildAbstractAst qGraph GLL.BadLeftRecursion.numToString "BadLeftRecursion.dot" 33 45 5 1 GLL.BadLeftRecursion.tokenData GLL.BadLeftRecursion.tokenToNumber

    [<Test>]
    member this._47_SimpleAmb () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.SimpleAmb.A 1)
             edg 1 2 (GLL.SimpleAmb.D 2)
             edg 2 3 (GLL.SimpleAmb.B 3)
             edg 3 4 (GLL.SimpleAmb.RNGLR_EOF 0)
             ] |> ignore

        test GLL.SimpleAmb.buildAbstractAst qGraph GLL.SimpleAmb.numToString "SimpleAmb.dot" 18 21 5 1 GLL.SimpleAmb.tokenData GLL.SimpleAmb.tokenToNumber
    
    [<Test>]
    member this._48_SimpleRightNull () =
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.SimpleRightNull.A 1)
             edg 1 2 (GLL.SimpleRightNull.A 1)
             edg 2 3 (GLL.SimpleRightNull.RNGLR_EOF 0)
             ] |> ignore

        test GLL.SimpleRightNull.buildAbstractAst qGraph GLL.SimpleRightNull.numToString "SimpleRightNull.dot" 22 24 6 0 GLL.SimpleRightNull.tokenData GLL.SimpleRightNull.tokenToNumber

    [<Test>]
    member this._49_SimpleLeftRecursion () =
        let qGraph = new ParserInputGraph<_>(0, 4)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.SimpleLeftRecursion.B 1)
             edg 1 2 (GLL.SimpleLeftRecursion.B 2)
             edg 2 3 (GLL.SimpleLeftRecursion.B 3)
             edg 3 4 (GLL.SimpleLeftRecursion.RNGLR_EOF 0)
             ] |> ignore

        test GLL.SimpleLeftRecursion.buildAbstractAst qGraph GLL.SimpleLeftRecursion.numToString "SimpleLeftRecursion.dot" 19 21 5 0 GLL.SimpleLeftRecursion.tokenData GLL.SimpleLeftRecursion.tokenToNumber

    [<Test>]
    member this._50_SimpleBranch () =
        let qGraph = new ParserInputGraph<_>(0, 3)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.ParseSimpleBranch.A 1)
             edg 1 2 (GLL.ParseSimpleBranch.C 2)
             edg 1 2 (GLL.ParseSimpleBranch.B 3)
             edg 2 3 (GLL.ParseSimpleBranch.RNGLR_EOF 0)
             ] |> ignore

        test GLL.ParseSimpleBranch.buildAbstractAst qGraph GLL.ParseSimpleBranch.numToString "SimpleBranch.dot" 14 15 5 1 GLL.ParseSimpleBranch.tokenData GLL.ParseSimpleBranch.tokenToNumber

    [<Ignore("It is for performance estimation only")>]
    [<Test>]
    member this._51_Conj1_Success () =
        let qGraph = new ParserInputGraph<_>(0, 10)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.Conj1.A 1)
             edg 1 2 (GLL.Conj1.A 2)
             edg 2 3 (GLL.Conj1.A 3)
             edg 3 4 (GLL.Conj1.B 3)
             edg 4 5 (GLL.Conj1.B 3)
             edg 5 6 (GLL.Conj1.B 3)
             edg 6 7 (GLL.Conj1.C 3)
             edg 7 8 (GLL.Conj1.C 3)
             edg 8 9 (GLL.Conj1.C 3)
             edg 9 10 (GLL.Conj1.RNGLR_EOF 0)
             ] |> ignore
        let r = GLL.Conj1.buildAbstractAst qGraph
        printfn "%A" r
        match r with
        | Error str ->
            printfn "Error"
            Assert.Fail("")
        | Success tree ->
            tree.AstToDot GLL.Conj1.numToString GLL.Conj1.tokenToNumber GLL.Conj1.tokenData (outputDir + "Conj1_Success.dot")
            Assert.Pass()

    [<Ignore("It is for performance estimation only")>]
    [<Test>]
    member this._52_Conj1_Fail () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.Conj1.A 1)
             edg 1 2 (GLL.Conj1.A 2)
             edg 2 3 (GLL.Conj1.A 3)
             edg 3 4 (GLL.Conj1.B 3)
             edg 4 5 (GLL.Conj1.B 3)
             edg 5 6 (GLL.Conj1.C 3)
             edg 6 7 (GLL.Conj1.C 3)
             edg 7 8 (GLL.Conj1.C 3)
             edg 8 9 (GLL.Conj1.RNGLR_EOF 0)
             ] |> ignore
        let r = GLL.Conj1.buildAbstractAst qGraph
        printfn "%A" r
        match r with
        | Error str ->
            printfn "Error"
            Assert.Pass()
        | Success tree ->
            tree.AstToDot GLL.Conj1.numToString GLL.Conj1.tokenToNumber GLL.Conj1.tokenData (outputDir + "Conj1_Fail.dot")
            Assert.Fail()

    [<Ignore("It is for performance estimation only")>]
    [<Test>]
    member this._53_Conj2_Success () =
        let qGraph = new ParserInputGraph<_>(0, 10)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.Conj2.A 1)
             edg 1 2 (GLL.Conj2.A 2)
             edg 2 3 (GLL.Conj2.A 3)
             edg 3 4 (GLL.Conj2.B 3)
             edg 4 5 (GLL.Conj2.C 3)
             edg 5 6 (GLL.Conj2.A 3)
             edg 6 7 (GLL.Conj2.A 3)
             edg 7 8 (GLL.Conj2.A 3)
             edg 8 9 (GLL.Conj2.B 3)
             edg 9 10 (GLL.Conj2.RNGLR_EOF 0)
             ] |> ignore
        let r = GLL.Conj2.buildAbstractAst qGraph
        printfn "%A" r
        match r with
        | Error str ->
            printfn "Error"
            Assert.Fail("")
        | Success tree ->
            tree.AstToDot GLL.Conj2.numToString GLL.Conj2.tokenToNumber GLL.Conj2.tokenData (outputDir + "Conj2_Success.dot")
            Assert.Pass()

    [<Ignore("It is for performance estimation only")>]
    [<Test>]
    member this._54_Conj2_Fail () =
        let qGraph = new ParserInputGraph<_>(0, 10)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.Conj2.A 1)
             edg 1 2 (GLL.Conj2.B 2)
             edg 2 3 (GLL.Conj2.A 3)
             edg 3 4 (GLL.Conj2.B 3)
             edg 4 5 (GLL.Conj2.C 3)
             edg 5 6 (GLL.Conj2.A 3)
             edg 6 7 (GLL.Conj2.A 3)
             edg 7 8 (GLL.Conj2.A 3)
             edg 8 9 (GLL.Conj2.B 3)
             edg 9 10 (GLL.Conj2.RNGLR_EOF 0)
             ] |> ignore
        let r = GLL.Conj2.buildAbstractAst qGraph
        printfn "%A" r
        match r with
        | Error str ->
            printfn "Error"
            Assert.Pass()
        | Success tree ->
            tree.AstToDot GLL.Conj2.numToString GLL.Conj2.tokenToNumber GLL.Conj2.tokenData (outputDir + "Conj2_Fail.dot")
            Assert.Fail("")

    [<Ignore("It is for performance estimation only")>]
    [<Test>]
    member this._55_Conj3_Success () =
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.Conj3.A 1)
             edg 1 2 (GLL.Conj3.B 2)
             edg 2 3 (GLL.Conj3.A 3)
             edg 3 4 (GLL.Conj3.B 3)
             edg 4 5 (GLL.Conj3.RNGLR_EOF 0)
             ] |> ignore
        let r = GLL.Conj3.buildAbstractAst qGraph
        printfn "%A" r
        match r with
        | Error str ->
            printfn "Error"
            Assert.Fail("")
        | Success tree ->
            tree.AstToDot GLL.Conj3.numToString GLL.Conj3.tokenToNumber GLL.Conj3.tokenData (outputDir + "Conj3_Success.dot")
            Assert.Fail("")

    [<Ignore("It is for performance estimation only")>]
    [<Test>]
    member this._56_Conj3_Fail () =
        let qGraph = new ParserInputGraph<_>(0, 7)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.Conj3.A 1)
             edg 1 2 (GLL.Conj3.A 2)
             edg 2 3 (GLL.Conj3.A 3)
             edg 3 4 (GLL.Conj3.B 3)
             edg 4 5 (GLL.Conj3.B 3)
             edg 5 6 (GLL.Conj3.B 3)
             edg 6 7 (GLL.Conj3.RNGLR_EOF 0)
             ] |> ignore
        let r = GLL.Conj3.buildAbstractAst qGraph
        printfn "%A" r
        match r with
        | Error str ->
            printfn "Error"
            Assert.Pass()
        | Success tree ->
            tree.AstToDot GLL.Conj3.numToString GLL.Conj3.tokenToNumber GLL.Conj3.tokenData (outputDir + "Conj3_Fail.dot")
            Assert.Fail("")    

    member this.GetFullGraph n tokens eof allPath =
        let graph = new ParserInputGraph<_>([|0 .. n - 1|],[|n|])
        for i in 0 .. n - 1 do
            [for t in [eof] do//tokens do 
                yield! [for j in 0 .. n - 1 do if i <> j then yield edg i j t ]]
            |> graph.AddVerticesAndEdgeRange
            |> ignore

//        [for i in 0 .. n - 1 -> edg i n eof] 
//        |> graph.AddVerticesAndEdgeRange
//        |> ignore

        graph

    member this.GetLineraBrackets n lbr rbr eof =
        let graph = new ParserInputGraph<_>(0, n + 1)
        for i in 0 .. 2 .. n - 1 do
            [ edg i (i + 1) lbr 
              edg (i + 1) (i + 2) rbr]
            |> graph.AddVerticesAndEdgeRange
            |> ignore

        edg n (n + 1) eof
        |> graph.AddVerticesAndEdge
        |> ignore

        graph
    
    member this.GetLineraForBad n =
        let graph = new ParserInputGraph<_>(0, n)
        for i in 0 .. n - 2 do
            edg i (i + 1) (GLL.BadLeftRecursion.B 0) 
            |> graph.AddVerticesAndEdge
            |> ignore

        edg (n - 1) n (GLL.BadLeftRecursion.RNGLR_EOF 1)
        |> graph.AddVerticesAndEdge
        |> ignore

        graph

    member this.PerformanceTestFullGraphUnambBraces() =
        printfn "----------Unamb brackets---------------"
        let g = this.GetFullGraph 2 [GLL.BadLeftRecursion.B 0] (GLL.BadLeftRecursion.RNGLR_EOF 2) true           
        let r = GLL.BadLeftRecursion.buildAbstractAst g
        for i in 1 .. 60 do
            let g = this.GetFullGraph i [GLL.StrangeBrackets.LBR 0; GLL.StrangeBrackets.RBR 1] (GLL.StrangeBrackets.RNGLR_EOF 2) true
            let start = System.DateTime.Now            
            let r = GLL.StrangeBrackets.buildAbstractAst g
            let time = (System.DateTime.Now - start).TotalMilliseconds
            match r with 
            | Error str ->
                printfn "Error"
            
            | Success tree ->
                let nodes,_,_,_ = tree.CountCounters
                printfn "%A: %A: %A" i time nodes
                //tree.AstToDot GLL.StrangeBrackets.numToString GLL.StrangeBrackets.tokenToNumber GLL.StrangeBrackets.tokenData "Nondet.dot"
            
    member this.PerformanceTestFullGraphAmbBraces() =
        printfn "----------Amb brackets---------------"
        let g = this.GetFullGraph 2 [GLL.BadLeftRecursion.B 0] (GLL.BadLeftRecursion.RNGLR_EOF 2) true           
        let r = GLL.BadLeftRecursion.buildAbstractAst g
        for i in 1 .. 60 do
            let g = this.GetFullGraph i [GLL.Brackets2.LBR 0; GLL.Brackets2.RBR 1] (GLL.Brackets2.RNGLR_EOF 2) true
            let start = System.DateTime.Now            
            let r = GLL.Brackets2.buildAbstractAst g
            let time = (System.DateTime.Now - start).TotalMilliseconds
            match r with 
            | Error str ->
                printfn "Error"
            
            | Success tree ->
                let nodes,_,_,_ = tree.CountCounters
                printfn "%A: %A : %A" i time nodes
                //tree.AstToDot GLL.StrangeBrackets.numToString GLL.StrangeBrackets.tokenToNumber GLL.StrangeBrackets.tokenData "Nondet.dot"
    

    member this.PerformanceTestFullGraphBadLeftRec() =
        printfn "----------BAD---------------"
        let g = this.GetFullGraph 2 [GLL.BadLeftRecursion.B 0] (GLL.BadLeftRecursion.RNGLR_EOF 2) true           
        let r = GLL.BadLeftRecursion.buildAbstractAst g
        for i in 1 .. 60 do
            let g = this.GetFullGraph i [GLL.BadLeftRecursion.B 0] (GLL.BadLeftRecursion.RNGLR_EOF 2) true
            let start = System.DateTime.Now            
            let r = GLL.BadLeftRecursion.buildAbstractAst g
            let time = (System.DateTime.Now - start).TotalMilliseconds
            match r with 
            | Error str ->
                printfn "Error"
            
            | Success tree ->
                let nodes,_,_,_ = tree.CountCounters
                printfn "%A: %A : %A" i time nodes
                //tree.AstToDot GLL.StrangeBrackets.numToString GLL.StrangeBrackets.tokenToNumber GLL.StrangeBrackets.tokenData "Nondet.dot"
    
    member this.PerformanceTestLinearUnambBraces() =
        printfn "----------Unamb brackets---------------"
        for i in 100 .. 50 .. 5000 do
            let g = this.GetLineraBrackets i (GLL.StrangeBrackets.LBR 0) (GLL.StrangeBrackets.RBR 1) (GLL.StrangeBrackets.RNGLR_EOF 2)
            let start = System.DateTime.Now
            let r = ref (Unchecked.defaultof<_>)
            for i in 0..4 do            
                r := GLL.StrangeBrackets.buildAbstractAst g
            let time = (System.DateTime.Now - start).TotalMilliseconds / 5.0
            match !r with 
            | Error str ->
                printfn "Error: %A" str
            
            | Success tree ->
                let nodes,_,_,_ = tree.CountCounters
                printfn "%A: %A: %A" i time nodes
                //tree.AstToDot GLL.StrangeBrackets.numToString GLL.StrangeBrackets.tokenToNumber GLL.StrangeBrackets.tokenData "Nondet.dot"
            
    member this.PerformanceTestLinearAmbBraces() =
        printfn "----------Amb brackets---------------"
        for i in 100 .. 50 .. 1000 do
            let g = this.GetLineraBrackets i (GLL.Brackets2.LBR 0) (GLL.Brackets2.RBR 1) (GLL.Brackets2.RNGLR_EOF 2)
            let start = System.DateTime.Now            
            let r = GLL.Brackets2.buildAbstractAst g
            let time = (System.DateTime.Now - start).TotalMilliseconds
            match r with 
            | Error str ->
                printfn "Error"
            
            | Success tree ->
                let nodes,_,_,_ = tree.CountCounters
                printfn "%A: %A : %A" i time nodes
                //tree.AstToDot GLL.StrangeBrackets.numToString GLL.StrangeBrackets.tokenToNumber GLL.StrangeBrackets.tokenData "Nondet.dot"
    

    member this.PerformanceTestLinearBadLeftRec() =
        printfn "----------BAD---------------"
        for i in 1 .. 100 do
            let g = this.GetLineraForBad i
            let start = System.DateTime.Now            
            let r = GLL.BadLeftRecursion.buildAbstractAst g
            let time = (System.DateTime.Now - start).TotalMilliseconds
            match r with 
            | Error str ->
                printfn "Error"
            
            | Success tree ->
                let nodes,_,_,_ = tree.CountCounters
                printfn "%A: %A : %A" i time nodes
                //tree.AstToDot GLL.StrangeBrackets.numToString GLL.StrangeBrackets.tokenToNumber GLL.StrangeBrackets.tokenData "Nondet.dot"
    
    [<Test>]
    member this._57_random_unambiguous_edges_function () =    
        let tokens = [|GLL.StrangeBrackets.LBR 1; GLL.StrangeBrackets.RBR 2|]
        let vCount = 1000
        let innerCyclesCount = 10
        for i in vCount..50..2*vCount do
            let cumulitiveTime = ref 0.0 
            printfn ""
            for j in 0..innerCyclesCount-1 do
                let input = newRandomGraph vCount i tokens (GLL.StrangeBrackets.RNGLR_EOF 0)
                let start = System.DateTime.Now
                let res = GLL.StrangeBrackets.buildAbstractAst input
                cumulitiveTime := !cumulitiveTime + (System.DateTime.Now - start).TotalMilliseconds
                match res with
                | Success _ -> printf "+"
                | _ -> printf "!"
            printfn ""
            printfn "%A:%A" i (!cumulitiveTime / (float innerCyclesCount))


    [<Test>]
    member this._58_random_unambiguous_edges_function () =    
//        let tokens = 
//            [GLL.StrangeBrackets.LBR 1; GLL.StrangeBrackets.RBR 2]
//            @ (List.init 2 (fun _ -> GLL.StrangeBrackets.ANY 3))
//            |> Array.ofList
//        let vCount = 2000
//        let tokenTypeCount = 3
//        let innerCyclesCount = 1
//        let step = 20
        let tokens = 
            [GLL.StrangeBrackets.LBR 1; GLL.StrangeBrackets.RBR 2]            
            //@ (List.init 2 (fun _ -> GLL.StrangeBrackets.ANY 3))
            @ [GLL.StrangeBrackets.LBR 1; GLL.StrangeBrackets.RBR 2]
            //@ (List.init 2 (fun _ -> GLL.StrangeBrackets.ANY 3))
            |> Array.ofList
        let vCount = 1500
        let tokenTypeCount = 3
        let innerCyclesCount = 1
        let step = 20
        let result = new ParserInputGraph<_>([|0..vCount-1|], [|vCount|])
        for i in 0..vCount-1 do result.AddVerticesAndEdge(edg i vCount (GLL.StrangeBrackets.RNGLR_EOF 0)) |> ignore                
        for i in vCount::(List.init (((tokenTypeCount * vCount)/step) - 1) (fun i -> step)) do
            let cumulitiveTime = ref 0.0 
            //printfn ""            
            let input = newRandomGraph2 result i tokens (GLL.StrangeBrackets.RNGLR_EOF 0)
            for j in 0..innerCyclesCount-1 do
                let start = System.DateTime.Now
                let res = GLL.StrangeBrackets.buildAbstractAst input
                cumulitiveTime := !cumulitiveTime + (System.DateTime.Now - start).TotalMilliseconds
//                match res with
//                | Success _ -> printf "+"
//                | _ -> printf "!"
            //printfn ""
            printfn "%A:%A" result.EdgeCount (!cumulitiveTime / (float innerCyclesCount))

    
    member this._Brackets_demo () =
        let qGraph = new ParserInputGraph<_>(0, 10)
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.Brackets2.LBR 1)            
            edg 2 3 (GLL.Brackets2.RBR 3)
            edg 3 4 (GLL.Brackets2.LBR 4)
            edg 5 6 (GLL.Brackets2.RBR 6)
            edg 6 7 (GLL.Brackets2.LBR 7)            
            edg 8 9 (GLL.Brackets2.RBR 9)
            edg 9 10 (GLL.Brackets2.RNGLR_EOF 0)
            ] |> ignore

        test GLL.Brackets2.buildAbstractAst qGraph GLL.Brackets2.numToString "Brackets2.dot" 14 15 5 1 GLL.Brackets2.tokenData GLL.Brackets2.tokenToNumber

    member this.getFullGraph (n) =
        let qGraph = new ParserInputGraph<_>([|0..n-1|], [|n|])
        qGraph.AddVerticesAndEdgeRange
           [edg 0 1 (GLL.Brackets2.LBR 1)            
            edg 2 3 (GLL.Brackets2.RBR 3)
            edg 3 4 (GLL.Brackets2.LBR 4)
            edg 5 6 (GLL.Brackets2.RBR 6)
            edg 6 7 (GLL.Brackets2.LBR 7)            
            edg 8 9 (GLL.Brackets2.RBR 9)
            edg 9 10 (GLL.Brackets2.RNGLR_EOF 0)
            ] |> ignore
        qGraph

    member this.getGraphForAvg n k lbr rbr eof =
        let qGraph = new ParserInputGraph<_>([|0..n-1|], [|n|])

        for l in 1..k do
            [for i in 0 .. n - l - 1 -> edg i (i + l) (lbr i)]
            @ [for i in 0 .. n - l - 1 -> edg (i + l) i  (rbr i)]
            |> qGraph.AddVerticesAndEdgeRange
            |> ignore
           
            for i in n - l .. n - 1 do
                [edg i (l - (n - 1 - i) - 1) (lbr 1)
                ; edg (l - (n - 1 - i) - 1) i (rbr 1)]
                |> qGraph.AddVerticesAndEdgeRange
                |> ignore

        [for i in 0 .. n - 1 -> edg i n (eof 0)]
        |> qGraph.AddVerticesAndEdgeRange
        |> ignore

        qGraph

    member this.getTwoCycledGraph n all lbr rbr eof =
        let qGraph = new ParserInputGraph<_>((if all then [|0..5 * n - 2|] else [|3|]), [|5 * n - 1|])

        [for i in 0 .. 3 * n - 2 -> edg i (i + 1) (lbr i)]
        |> qGraph.AddVerticesAndEdgeRange
        |> ignore
           
        [edg (3 * n - 1) 0 (lbr 0)]
        |> qGraph.AddVerticesAndEdgeRange
        |> ignore

        [for i in 3 * n .. 5 * n - 3 -> edg i (i + 1) (rbr i)]
        |> qGraph.AddVerticesAndEdgeRange
        |> ignore

        [edg (5 * n - 2) 0 (rbr 0)
         edg 0 (3 * n) (rbr 0)
        ]
        |> qGraph.AddVerticesAndEdgeRange
        |> ignore

        [for i in 0 .. 5 * n - 2 -> edg i (5 * n - 1) (eof 0)]
        |> qGraph.AddVerticesAndEdgeRange
        |> ignore

        qGraph

    member this.getLinearGraph n all lbr rbr eof =
        let qGraph = new ParserInputGraph<_>((if all then [|0 .. n * 2|] else [|0|]), [|n * 2 + 1|])

        [for i in 0 .. n -> edg i (i + 1) (lbr i)]
        |> qGraph.AddVerticesAndEdgeRange
        |> ignore
           
        [for i in n .. 2 * n -> edg i (i + 1) (rbr i)]
        |> qGraph.AddVerticesAndEdgeRange
        |> ignore

        [for i in 0 .. 2 * n -> edg i (2 * n + 1) (eof 0)]
        |> qGraph.AddVerticesAndEdgeRange
        |> ignore

        qGraph

    member this._AvgPerfTest_unamb () =    
        //let g1 = this.getGraphForAvg 3 2 GLL.Brackets2.LBR GLL.Brackets2.RBR GLL.Brackets2.RNGLR_EOF       
        //g1.PrintToDot "ooo.dot" string
        for i in 2000 .. 100 .. 8000 do 
            let g = this.getGraphForAvg i 1 GLL.Brackets2.LBR GLL.Brackets2.RBR GLL.Brackets2.RNGLR_EOF       
            let start = System.DateTime.Now
            for i in 0..4 do
                let res = GLL.Brackets2.buildAbstractAst g 
                ()
            printfn "%A: %A" i ((System.DateTime.Now - start).TotalMilliseconds/5.0)

    member this._Brackets_performance_all_amb () =        
        for i in 200 .. 100 .. 2000 do 
            let g = this.getTwoCycledGraph i true GLL.Brackets2.LBR GLL.Brackets2.RBR GLL.Brackets2.RNGLR_EOF       
            let start = System.DateTime.Now
            for i in 0..4 do
                let res = GLL.Brackets2.buildAbstractAst g 
                ()
            printfn "%A: %A" i ((System.DateTime.Now - start).TotalMilliseconds/5.0)

     member this._Brackets_performance_all_unamb () =        
        for i in 200 .. 100 .. 2000 do 
            let g = this.getTwoCycledGraph i true GLL.StrangeBrackets.LBR GLL.StrangeBrackets.RBR GLL.StrangeBrackets.RNGLR_EOF       
            let start = System.DateTime.Now
            for i in 0..4 do
                let res = GLL.StrangeBrackets.buildAbstractAst g 
                ()
            printfn "%A: %A" i ((System.DateTime.Now - start).TotalMilliseconds/5.0)

    member this._Brackets_performance_single_unamb () =        
        for i in 200 .. 100 .. 2000 do 
            let g = this.getTwoCycledGraph i false GLL.Brackets2.LBR GLL.Brackets2.RBR GLL.Brackets2.RNGLR_EOF       
            let start = System.DateTime.Now
            for i in 0..4 do
                let res = GLL.Brackets2.buildAbstractAst g 
                ()
            printfn "%A: %A" i ((System.DateTime.Now - start).TotalMilliseconds/5.0)

    member this._Brackets_performance_single_amb () =        
        for i in 200 .. 100 .. 2000 do 
            let g = this.getTwoCycledGraph i false GLL.StrangeBrackets.LBR GLL.StrangeBrackets.RBR GLL.StrangeBrackets.RNGLR_EOF       
            let start = System.DateTime.Now
            for i in 0..4 do
                let res = GLL.StrangeBrackets.buildAbstractAst g 
                ()
            printfn "%A: %A" i ((System.DateTime.Now - start).TotalMilliseconds/5.0)

    member this._Brackets_performance_Linear_all_unamb () =        
        for i in 200 .. 50 .. 4000 do 
            let g = this.getLinearGraph i true GLL.StrangeBrackets.LBR GLL.StrangeBrackets.RBR GLL.StrangeBrackets.RNGLR_EOF       
            let start = System.DateTime.Now
            for i in 0..4 do
                let res = GLL.StrangeBrackets.buildAbstractAst g 
                ()
            printfn "%A: %A" i ((System.DateTime.Now - start).TotalMilliseconds/5.0)

     member this._Brackets_performance_Linear_single_unamb () =        
        for i in 200 .. 100 .. 4000 do 
            let g = this.getLinearGraph i false GLL.StrangeBrackets.LBR GLL.StrangeBrackets.RBR GLL.StrangeBrackets.RNGLR_EOF       
            let start = System.DateTime.Now
            for i in 0..4 do
                let res = GLL.StrangeBrackets.buildAbstractAst g 
                ()
            printfn "%A: %A" i ((System.DateTime.Now - start).TotalMilliseconds/5.0)


//    [<Ignore("It is for performance estimation only")>]
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

[<EntryPoint>]
let f x =
    //System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.LowLatency
//    for p in 
//        [
//         @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\1.1.ttl"
//         @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\wine.rdf"
//         @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\pizza.owl"
//         @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\foaf.rdf" 
//         @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\people_pets.rdf"
//        ] do
//
//            let f = 
//                YC.GLL.Abstarct.Tests.RDFPerformance.loadFromFile p
//                 
//            printfn "triples in %A: %A" (System.IO.Path.GetFileName p) f.Triples.Count
    YC.GLL.Abstarct.Tests.RDFPerformance.parse @"C:\gsv\projects\YC\YaccConstructor\tests\data\RDF\foaf.rdf"
//    let t = new ``GLL abstract parser tests``()
//    let f () = 
//                t._AvgPerfTest_unamb()
//                //let r = parseGraphCYK (t.GetFullGraph 4 [] GLL.Brackets2.RNGLR_EOF  true)
//                //printfn "Result size: %A" r.Count
//               //t.PerformanceTestFullGraphBadLeftRec()
//               //t.PerformanceTestFullGraphUnambBraces()
//               //t.PerformanceTestFullGraphAmbBraces()
////               t.PerformanceTestLinearBadLeftRec()
//               //t.PerformanceTestLinearUnambBraces()
////               t.PerformanceTestLinearAmbBraces()
//              //_35_Expression() //
//    //let th = new System.Threading.Thread(f, 10000000)
//    //th.Start()
//    f()
    0

(*
++++++++++
1050:2.22569

++++++++++
1100:3.00639

++++++++++
1150:18.99244

++++++++++
1200:22.32902

++++++++++
1250:157.24992

++++++++++
1300:550.44073

++++++++++
1350:1427.97743

++++++++++
1400:5150.66121

++++++++++
1450:5017.79941

++++++++++
1500:20857.22014

+

*)