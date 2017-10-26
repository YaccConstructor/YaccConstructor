module ExpandRepeatTests

open Yard.Core
open Yard.Core.IL
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
        (simpleRules "s"
              <| PAlt
                   (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)} ],None,None),
                     (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}],None,None)))
        ) @ (
            simpleNotStartRules "yard_repeat_1"
                <| PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}],None,None)
             )       
        |> runTest (path "repeat1.yrd") expandRepeat          


    [<Test>]                                                                                            
    member test.``Repeat with meta rule`` () =
        ( metaRules "do"  (PAlt (PSeq ([{dummyRule with rule = PRef (Source.t("a"),None)}],None,None),
                                (PSeq ([{dummyRule with rule = PRef (Source.t("b"),None)}],None,None)))) [Source.t("a"); Source.t("b")]
        ) @ (
            let innerRule = PAlt(PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}],None,None),
                                                        (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}],None,None)))
            let metaRule = PMetaRef (Source.t("do"), None, [PSeq ([{dummyRule with rule = innerRule}],None,None)])
            simpleRules "s"
                <| PSeq([{dummyRule with rule = metaRule}], None, None)
        ) @ (
            simpleNotStartRules "yard_repeat_1" <| PRef (Source.t("x"),None)   
        )       
        |> runTest (path "repeat2.yrd") expandRepeat       

    [<Test>]                                                                                            
    member test.``Repeat with many rule`` () =
        (simpleRules "s"
              <| PMany
                   (PAlt
                     (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)} ],None,None),
                     (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}],None,None))))
        ) @ (
              simpleNotStartRules "yard_repeat_1"
              <| PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}],None,None)
        ) 
        |> runTest (path "repeat3.yrd") expandRepeat   

    [<Test>]                                                                                            
    member test.``Repeat with inner repeat`` () =
        (simpleRules "s"
                <| PAlt (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}],None,None),
                        (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}],None,None)))
        ) @ (
              simpleNotStartRules "yard_repeat_1"
              <| PAlt
                     (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_2"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_2"),None)} ],None,None),
                     (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_2"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_2"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_2"),None)}],None,None)))
        ) @ (
              simpleNotStartRules "yard_repeat_2"
              <| PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}],None,None)
             )       
        |> runTest (path "repeat4.yrd") expandRepeat   

    [<Test>]                                                                                            
    member test.``Repeat without upper bound`` () =
        (simpleRules "s"
              <| PSeq ([{dummyRule with rule = PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)};],None,None)}; {dummyRule with rule = PMany <| PRef (Source.t("yard_repeat_1"),None)}],None,None)
        ) @ (
              simpleNotStartRules "yard_repeat_1"
              <| PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}],None,None)
             )       
        |> runTest (path "repeat5.yrd") expandRepeat   