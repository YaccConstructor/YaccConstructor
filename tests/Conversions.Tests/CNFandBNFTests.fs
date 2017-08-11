module CNFandBNFTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Yard.Core.Helpers
open Conversions.TransformAux
open NUnit.Framework
open ConversionsTests
open Yard.Core.Helpers

[<Ignore("Should be fixed! Ignored for temporary release.")>]
[<TestFixture>]
type ``CNFandBNF tests`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "ToCNF")
    let path f = System.IO.Path.Combine(basePath, f)
      
    let applyConversion (conversion:Conversion) loadIL = 
        {
            loadIL
                with grammar = conversion.ConvertGrammar (loadIL.grammar, [||])                               
        }

    [<Test>]
    member test.``ToCNF1`` () =
            let rules = System.IO.File.ReadAllText(path "grammar1_res.txt").Replace("\r", "")
            runTest2 (path "grammar1.yrd") conversionCNF rules 

    [<Test>]
    member test.``ToCNF2`` () =
            let rules = System.IO.File.ReadAllText(path "grammar2_res.txt").Replace("\r", "")
            runTest2 (path "grammar2.yrd") conversionCNF rules
                       
    [<Test>]
    member test.``ToBNFconj`` () =
            let rules = System.IO.File.ReadAllText(path "grammar3_res.txt").Replace("\r", "")
            runTest2 (path "grammar3.yrd") conversionBNFconj rules 

    [<Test>]
    member test.``ToBNFbool`` () =
            let rules = System.IO.File.ReadAllText(path "grammar4_res.txt").Replace("\r", "")
            runTest2 (path "grammar4.yrd") conversionBNFbool rules 
