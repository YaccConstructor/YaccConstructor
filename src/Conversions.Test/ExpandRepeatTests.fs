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

    //[<Test>]                                                                                            
    member test.``Simple repeat 1`` () =
        (verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}]
        ) @ (
              simpleNotStartRules "yard_repeat_1"
              <| PAlt
                   (PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)} ],None,None),
                     (PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}],None,None)))
             )       
        |> runTest (path "repeat1.yrd") expandRepeat          


   // [<Test>]                                                                                            
    member test.``Repeat with meta rule`` () =
        ( metaRules "do"  (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}],None,None)) [Source.t("a"); Source.t("b")]
        ) @ (
            verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t("yard_repeat_2"),None)}]
        ) @ (
            simpleNotStartRules "yard_repeat_1"
                <| PAlt
                        (PSeq ([{dummyRule with rule = PRef (Source.t("a"),None)}],None,None),
                        (PSeq ([{dummyRule with rule = PRef (Source.t("b"),None)}],None,None)))
        ) @ (
            simpleNotStartRules "yard_repeat_2"
                <| PMetaRef (Source.t("do"), None, [PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_3"),None)}],None,None); PRef (Source.t("t"), None)] ) 
        ) @ (
            simpleNotStartRules "yard_repeat_3"
                <| PAlt
                        (PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}],None,None),
                        (PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}],None,None)))        
        )       
        |> runTest (path "repeat2.yrd") expandRepeat       

   // [<Test>]                                                                                            
    member test.``Repeat with many rule`` () =
        (verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}]
        ) @ (
              simpleNotStartRules "yard_repeat_1"
              <| PMany
                   (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_2"),None)}],None,None))
        ) @ (
              simpleNotStartRules "yard_repeat_2"
              <| PAlt
                     (PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)} ],None,None),
                     (PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}],None,None)))
             )       
        |> runTest (path "repeat3.yrd") expandRepeat   

   // [<Test>]                                                                                            
    member test.``Repeat with inner repeat`` () =
        (verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}]
        ) @ (
              simpleNotStartRules "yard_repeat_1"
              <| PAlt
                   (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_2"),None)}],None,None),
                     (PSeq ([{dummyRule with rule = PRef (Source.t("yard_repeat_2"),None)}; {dummyRule with rule = PRef (Source.t("yard_repeat_2"),None)}],None,None)))
        ) @ (
              simpleNotStartRules "yard_repeat_2"
              <| PAlt
                     (PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)} ],None,None),
                     (PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)}],None,None)))
             )       
        |> runTest (path "repeat4.yrd") expandRepeat   

   // [<Test>]                                                                                            
    member test.``Repeat without upper bound`` () =
        (verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t("yard_repeat_1"),None)}]
        ) @ (
              simpleNotStartRules "yard_repeat_1"
              <| PSeq ([{dummyRule with rule = PSeq ([{dummyRule with rule = PRef (Source.t("x"),None)}; {dummyRule with rule = PRef (Source.t("x"),None)};],None,None)}; {dummyRule with rule = PMany <| PRef (Source.t("x"),None)}],None,None)              
             )       
        |> runTest (path "repeat5.yrd") expandRepeat   