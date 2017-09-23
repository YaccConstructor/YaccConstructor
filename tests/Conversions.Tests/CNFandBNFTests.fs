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
    member test.``ToCNF`` () =
            let rules = 
                (verySimpleRules "yard_s_2"
                    [{dummyRule with rule = PRef (Source.t "yard_s_2_4", None)}
                     {dummyRule with rule = PRef (Source.t "x", None)}])
                @(verySimpleNotStartRules "yard_s_1"
                    [{dummyRule with rule = PRef (Source.t "x", None)}
                     {dummyRule with rule = PRef (Source.t "yard_s_2", None)}]) 
                @(verySimpleNotStartRules "x"
                    [{dummyRule with rule = PRef (Source.t "yard_s3", None)}
                     {dummyRule with rule = PRef (Source.t "y", None)}])   
                @(verySimpleNotStartRules "x"
                    [{dummyRule with rule = PRef (Source.t "yard_s_2_4", None)}
                     {dummyRule with rule = PRef (Source.t "y", None)}])      
                @(verySimpleNotStartRules "yard_s_1"
                    [{dummyRule with rule = PRef (Source.t "yard_s_2_4", None)}
                     {dummyRule with rule = PRef (Source.t "x", None)}]) 
                 @(verySimpleNotStartRules "y"
                     [{dummyRule with rule = PToken (Source.t "CC")}]) 
                  @(verySimpleNotStartRules "yard_s_2"
                     [{dummyRule with rule = PToken (Source.t "B")}]) 
                  @(verySimpleNotStartRules "x"
                     [{dummyRule with rule = PToken (Source.t "A")}]) 
                  @(verySimpleNotStartRules "x"
                     [{dummyRule with rule = PToken (Source.t "B")}]) 
                  @(verySimpleNotStartRules "y"
                     [{dummyRule with rule = PRef (Source.t "yard_s_3", None)}
                      {dummyRule with rule = PRef (Source.t "y", None)}]) 
                  @(verySimpleNotStartRules "yard_s_1"
                     [{dummyRule with rule = PToken (Source.t "B")}])
                  @(verySimpleNotStartRules "y"
                     [{dummyRule with rule = PRef (Source.t "yard_s_2_4", None)}
                      {dummyRule with rule = PRef (Source.t "y", None)}])
                  @(verySimpleNotStartRules "y"
                     [{dummyRule with rule = PToken (Source.t "B")}])
                  @(verySimpleRules "s"
                     [{dummyRule with rule = PRef (Source.t "yard_s_3", None)}
                      {dummyRule with rule = PRef (Source.t "yard_s_1", None)}])
                  @(verySimpleNotStartRules "yard_s_3"
                     [{dummyRule with rule = PToken (Source.t "A")}])
                  @(verySimpleNotStartRules "yard_s_2_4"
                     [{dummyRule with rule = PToken (Source.t "B")}])
            runTest (path "grammar1.yrd") conversionCNF rules 
