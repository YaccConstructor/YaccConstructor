module YC.Parsers.GLL.Tests.LinearInputTests

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

let parse = Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput.isPsrsed

let test parsTables input tokToNum expected =
    let res = 
        (Array.map (fun t -> tokToNum t * 1<token>) input)
        |> LinearInput
        |> parse parsTables 
    Assert.AreEqual(expected, res) 

[<TestFixture>]
type ``GLL tests on linear input witout SPPF`` () =

    [<Test>]
    member this._01_PrettySimpleCalc_SequenceInput () =
        let input = [|GLL.PrettySimpleCalc.NUM 1; GLL.PrettySimpleCalc.PLUS 2; GLL.PrettySimpleCalc.NUM 3|]        
        test  GLL.PrettySimpleCalc.parserSource input GLL.PrettySimpleCalc.tokenToNumber true

