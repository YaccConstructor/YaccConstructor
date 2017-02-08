module YC.Parsers.GLL.Tests.SimpleGraphInputTests

open System.IO
open QuickGraph
open NUnit.Framework
open AbstractAnalysis.Common
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon

open GLL.SimpleRightRecursion
open GLL.BadLeftRecursion
open GLL.SimpleAmb

open GLL.SimpleRightNull
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

open GLL.ParseCalc
open GLL.ParseCond
open GLL.ParseCounter
open GLL.ParseCycle
open GLL.ParseEps2
open GLL.ParseEpsilon
open GLL.ParseExpr
open GLL.ParseFirst
open GLL.ParseLolCalc
open GLL.ParseLongCycle
open GLL.ParseLongCycle_BAD
open GLL.ParseLongest
open GLL.ParseMixed
open GLL.ParseOmit
open System.Collections.Generic

let parse = Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput.getAllRangesForStartState

let test parsTables input tokToNum expected =
    let res = 
        parse parsTables input
        |> Set.ofSeq
        |> Array.ofSeq
        |> Array.sort
    printfn "%A" res
    Assert.AreEqual(expected |> Array.sort, res) 

[<TestFixture>]
type ``GLL tests on SimpleGraph input witaut SPPF`` () =
    
    let edg f t tId l = new ParserEdge<_>(f, t, tId l)

    [<Test>]
    member this._01_PrettySimpleCalc_SequenceInput () =
        let input = new SimpleGraphInput<_>([|0<positionInInput>|], id)
        let tId t = GLL.PrettySimpleCalc.tokenToNumber t * 1<token>
        let p (x,y) = x * 1<positionInInput>, y * 1<positionInInput>
        input.AddVerticesAndEdgeRange
            [edg 0 1 tId (GLL.PrettySimpleCalc.NUM 1)
             edg 1 2 tId (GLL.PrettySimpleCalc.PLUS 2)
             edg 2 3 tId (GLL.PrettySimpleCalc.NUM 3)
             ] |> ignore     
        test GLL.PrettySimpleCalc.parserSource input GLL.PrettySimpleCalc.tokenToNumber [|p(0,0);p(0,1);p(0,3);p(2,2);p(2,3)|]

