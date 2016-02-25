module ExpandRepeatTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Conversions.TransformAux
open NUnit.Framework
open ConversionsTests
open Yard.Core.Helpers

[<TestFixture>]
type ``Expand repeat tests`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "ExpandRepeat")
    let path f = System.IO.Path.Combine(basePath, f)

    [<Test>]                                                                                            
    member test.``Simple repeat 1`` () =
        (verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}]
        ) @ (
              simpleNotStartRules "yard_repeat_1"
              <| PAlt
                   (PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)} ],None,None),
                    PAlt (PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}],None,None),
                          PAlt (PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)} ; {dummyRule with rule = PRef (Source.t("x"),None)} ],None,None),
                                PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)} ; {dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}],None,None)))))
        
        |> runTest (path "repeat1.yrd") expandRepeat          

    [<Test>]                                                                                            
    member test.``Simple repeat 2`` () =
        (verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t("z"),None)};
                 {dummyRule with rule = PRef (Source.t("x"),None)}]
        ) @ (
              simpleNotStartRules "x"
                <| PSeq([{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}], None, None)
        ) @ (
              simpleNotStartRules "yard_repeat_1"  
              <| PAlt
                   (PSeq ([{dummyRule with rule = PRef (Source.t("m"),None)}],None,None),
                    PSeq ([{dummyRule with rule = PRef (Source.t("m"),None)}; {dummyRule with rule = PRef (Source.t("m"),None)}],None,None))
             )
        
        |> runTest (path "repeat2.yrd") expandRepeat       