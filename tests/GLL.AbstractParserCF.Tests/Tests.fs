module GLLAbstractParserCFTests

open System.IO
open System.Collections.Generic

open NUnit.Framework
open QuickGraph

open YaccConstructor.API
open Yard.Generators.GLL.AbstractParserCF
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common

let grammarsDir = @"C:\Users\User\Projects\YaccConstructor\tests\GLL.AbstractParserCF.Tests\"

let getParserSource grammarFile =    
    generate (grammarsDir + grammarFile)
             "YardFrontend" "GLLGenerator" 
             None
             ["ExpandMeta"]
             [] :?> ParserSourceGLL

let parseCF = Yard.Generators.GLL.AbstractParserCF.parse

let testCFParser grammar1 grammar2 =
    let source1, source2 = getParserSource grammar1, getParserSource grammar2
    parseCF source1 source2

[<TestFixture>]
type ``GLLAbstractCFParserTests`` () =
    
    // infinite loop (embedded recursion)
    [<Test>]
    member this.``Test_CF``() =        
        let gss1, gss2, count = testCFParser "BracketsLeft.yrd" "BracketsRight.yrd"
        gss1.ToDot (grammarsDir + "gss.dot")
        gss2.ToDot (grammarsDir + "gss2.dot")
        printfn "%i" count
        Directory.GetCurrentDirectory() |> printfn "%s"        
        Assert.IsTrue true

    [<Test>]
    member this.``Brackets``() =
        let gss1, gss2, count = testCFParser "BracketsLeft.yrd" "Brackets_nonrec.yrd"
        let x = getAllCompleteRangesForState gss1 (0<positionInGrammar>) (1<positionInGrammar>)
        printfn ""
        x |> Seq.iter (fun (s, e) -> printf "(%i, %i); " s e)
        Assert.IsTrue true

    [<Test>]
    member this.``Cycle_brackets``() =
        let gss1, gss2, count = testCFParser "BracketsLeft.yrd" "Cycle_brackets.yrd"
        let x = getAllCompleteRangesForState gss1 (0<positionInGrammar>) (1<positionInGrammar>)
        printfn ""
        x |> Seq.iter (fun (s, e) -> printf "(%i, %i); " s e)
        Assert.IsFalse false

    [<Test>]
    member this.``Simple_substr``() =
        let gss1, gss2, count = testCFParser "Simple_substr_t.yrd" "Simple_substr_d.yrd"
        let x = getAllCompleteRangesForState gss1 (0<positionInGrammar>) (1<positionInGrammar>)
        printfn ""
        x |> Seq.iter (fun (s, e) -> printf "(%i, %i); " s e)
        Assert.IsFalse false
