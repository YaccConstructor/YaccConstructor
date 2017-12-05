module RegularApproximationTests

open Yard.Core
open Yard.Core.IL
open Conversions.TransformAux
open NUnit.Framework
open ConversionsTests
open Yard.Core.Helpers

[<TestFixture>]
type ``Regular approximation tests`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "RegularApproximation")
    let path f = System.IO.Path.Combine(basePath, f)

    [<Test>]
    member test.``Simple approximation 1`` () =
        (simpleNotStartRules "yard_s_1"
                <| PSeq ([],None,None))
        @
        (simpleNotStartRules "yard_a_2"
                <| PSeq ([],None,None))
        @
        (simpleNotStartRules "yard_b_3"
                <| PSeq ([],None,None))
        @
        (simpleRules "s"
                <| PRef (Source("a"),None))
        @
        (simpleNotStartRules "yard_a_2"
                <| PSeq ([{dummyRule with rule = PToken <| Source("X")}; {dummyRule with rule = PRef (Source("yard_s_1"),None)}],None,None))
        @
        (simpleNotStartRules "a"
                <| PRef (Source("b"),None))
        @
        (simpleNotStartRules "yard_b_3"
                <| PRef (Source("yard_a_2"),None))
        @
        (simpleNotStartRules "a"
                <| PRef (Source("yard_a_2"),None))
        @
        (simpleNotStartRules "b"
                <| PRef (Source("a"),None))
        @
        (simpleNotStartRules "yard_a_2"
                <| PRef (Source("s"),None))
        @
        (simpleNotStartRules "yard_s_1"
                <| PSeq ([{dummyRule with rule = PToken <| Source("Y")}; {dummyRule with rule = PRef (Source("yard_b_3"),None)}],None,None))
        @
        (simpleNotStartRules "b"
                <| PSeq ([{dummyRule with rule = PToken <| Source("Z")}; {dummyRule with rule = PRef (Source("yard_b_3"),None)}],None,None))
        |> runTest (path "simpleTest1.yrd") regularApproximarion

    [<Test>]
    member test.``Simple approximation 2`` () =
        (simpleNotStartRules "yard_s_1"
                <| PSeq ([],None,None))
        @
        (simpleRules "s"
                <| PSeq ([{dummyRule with rule = PToken <| Source("A")}; {dummyRule with rule = PRef (Source("s"),None)}],None,None))
        @
        (simpleNotStartRules "yard_s_1"
                <| PSeq ([{dummyRule with rule = PToken <| Source("A")}; {dummyRule with rule = PRef (Source("yard_s_1"),None)}],None,None))
        @
        (simpleRules "s"
                <| PRef (Source("yard_s_1"),None))
        |> runTest (path "simpleTest2.yrd") regularApproximarion

    [<Test>]
    member test.``Simple approximation 3`` () =
        (simpleNotStartRules "yard_e_1"
                <| PSeq ([],None,None))
        @
        (simpleNotStartRules "yard_t_2"
                <| PSeq ([],None,None))
        @
        (simpleNotStartRules "yard_f_3"
                <| PSeq ([],None,None))
        @
        (simpleRules "e"
                <| PRef (Source("e"),None))
        @
        (simpleNotStartRules "yard_e_1"
                <| PSeq ([{dummyRule with rule = PToken <| Source("PLUS")}; {dummyRule with rule = PRef (Source("t"),None)}],None,None))
        @
        (simpleNotStartRules "yard_t_2"
                <| PRef (Source("yard_e_1"),None))
        @
        (simpleRules "e"
                <| PRef (Source("t"),None))
        @
        (simpleNotStartRules "yard_t_2"
                <| PRef (Source("yard_e_1"),None))
        @
        (simpleNotStartRules "t"
                <| PRef (Source("t"),None))
        @
        (simpleNotStartRules "yard_t_2"
                <| PSeq ([{dummyRule with rule = PToken <| Source("MUL")}; {dummyRule with rule = PRef (Source("f"),None)}],None,None))
        @
        (simpleNotStartRules "yard_f_3"
                <| PRef (Source("yard_t_2"),None))
        @
        (simpleNotStartRules "t"
                <| PRef (Source("f"),None))
        @
        (simpleNotStartRules "yard_f_3"
                <| PRef (Source("yard_t_2"),None))
        @
        (simpleNotStartRules "f"
                <| PSeq ([{dummyRule with rule = PToken <| Source("L")}; {dummyRule with rule = PRef (Source("e"),None)}],None,None))
        @
        (simpleNotStartRules "yard_e_1"
                <| PSeq ([{dummyRule with rule = PToken <| Source("R")}; {dummyRule with rule = PRef (Source("yard_f_3"),None)}],None,None))
        @
        (simpleNotStartRules "f"
                <| PSeq ([{dummyRule with rule = PToken <| Source("A")}; {dummyRule with rule = PRef (Source("yard_f_3"),None)}],None,None))
        |> runTest (path "simpleTest3.yrd") regularApproximarion
