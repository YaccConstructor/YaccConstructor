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
        (simpleNotStartRules "s'"
                <| PSeq ([],None,None))
        @
        (simpleNotStartRules "a'"
                <| PSeq ([],None,None))
        @
        (simpleNotStartRules "b'"
                <| PSeq ([],None,None))
        @
        (simpleRules "s"
                <| PRef (Source("a"),None))
        @
        (simpleNotStartRules "a'"
                <| PSeq ([{dummyRule with rule = PToken <| Source("X")}; {dummyRule with rule = PRef (Source("s'"),None)}],None,None))
        @
        (simpleNotStartRules "a"
                <| PRef (Source("b"),None))
        @
        (simpleNotStartRules "b'"
                <| PRef (Source("a'"),None))
        @
        (simpleNotStartRules "a"
                <| PRef (Source("a'"),None))
        @
        (simpleNotStartRules "b"
                <| PRef (Source("a"),None))
        @
        (simpleNotStartRules "a'"
                <| PRef (Source("s"),None))
        @
        (simpleNotStartRules "s'"
                <| PSeq ([{dummyRule with rule = PToken <| Source("Y")}; {dummyRule with rule = PRef (Source("b'"),None)}],None,None))
        @
        (simpleNotStartRules "b"
                <| PSeq ([{dummyRule with rule = PToken <| Source("Z")}; {dummyRule with rule = PRef (Source("b'"),None)}],None,None))
        |> runTest (path "simpleTest1.yrd") regularApproximarion

    [<Test>]
    member test.``Simple approximation 2`` () =
        (simpleNotStartRules "s'"
                <| PSeq ([],None,None))
        @
        (simpleRules "s"
                <| PSeq ([{dummyRule with rule = PToken <| Source("A")}; {dummyRule with rule = PRef (Source("s"),None)}],None,None))
        @
        (simpleNotStartRules "s'"
                <| PSeq ([{dummyRule with rule = PToken <| Source("A")}; {dummyRule with rule = PRef (Source("s'"),None)}],None,None))
        @
        (simpleRules "s"
                <| PRef (Source("s'"),None))
        |> runTest (path "simpleTest2.yrd") regularApproximarion

    [<Test>]
    member test.``Simple approximation 3`` () =
        (simpleNotStartRules "e'"
                <| PSeq ([],None,None))
        @
        (simpleNotStartRules "t'"
                <| PSeq ([],None,None))
        @
        (simpleNotStartRules "f'"
                <| PSeq ([],None,None))
        @
        (simpleRules "e"
                <| PRef (Source("e"),None))
        @
        (simpleNotStartRules "e'"
                <| PSeq ([{dummyRule with rule = PToken <| Source("PLUS")}; {dummyRule with rule = PRef (Source("t"),None)}],None,None))
        @
        (simpleNotStartRules "t'"
                <| PRef (Source("e'"),None))
        @
        (simpleRules "e"
                <| PRef (Source("t"),None))
        @
        (simpleNotStartRules "t'"
                <| PRef (Source("e'"),None))
        @
        (simpleNotStartRules "t"
                <| PRef (Source("t"),None))
        @
        (simpleNotStartRules "t'"
                <| PSeq ([{dummyRule with rule = PToken <| Source("MUL")}; {dummyRule with rule = PRef (Source("f"),None)}],None,None))
        @
        (simpleNotStartRules "f'"
                <| PRef (Source("t'"),None))
        @
        (simpleNotStartRules "t"
                <| PRef (Source("f"),None))
        @
        (simpleNotStartRules "f'"
                <| PRef (Source("t'"),None))
        @
        (simpleNotStartRules "f"
                <| PSeq ([{dummyRule with rule = PToken <| Source("L")}; {dummyRule with rule = PRef (Source("e"),None)}],None,None))
        @
        (simpleNotStartRules "e'"
                <| PSeq ([{dummyRule with rule = PToken <| Source("R")}; {dummyRule with rule = PRef (Source("f'"),None)}],None,None))
        @
        (simpleNotStartRules "f"
                <| PSeq ([{dummyRule with rule = PToken <| Source("A")}; {dummyRule with rule = PRef (Source("f'"),None)}],None,None))
        |> runTest (path "simpleTest3.yrd") regularApproximarion
