module Yard.Utils.OffsetMap.Test

open System.IO
open System.Collections.Generic
open NUnit.Framework
open Yard.Utils.StructClass
open Yard.Utils.InfoClass
open Yard.Utils.SourceText


[<TestFixture>]
type UtilsTest () =
    let p  = new ProjInfo()
    let map : int64<symbol>[] = [| 0L<symbol>; 30L<symbol>; 44L<symbol>; 88L<symbol>; 116L<symbol>;
                               274L<symbol>; 333L<symbol>; 448L<symbol>; 511L<symbol>; 690L<symbol>;
                               820L<symbol>; 1042L<symbol> |]
    do p.AddLine 1<id> map

    [<Test>]
    member test.``Test for random values of line and column`` () =
        let id = 1<id>
        let line = 5<line>
        let column = 20<symbol> 
        let packTrinity = Pack id line column         
        let offset = p.GetAbsoluteOffset packTrinity
        let packPair = PackPair 1<id> offset
        let actual = p.GetCoordinates packPair
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for the extreme lower values of line`` () =
        let id = 1<id>
        let line = 1<line>
        let column = 25<symbol>
        let packTrinity = Pack id line column         
        let offset = p.GetAbsoluteOffset packTrinity
        let packPair = PackPair 1<id> offset
        let actual = p.GetCoordinates packPair
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for the extreme upper values of line`` () =
        let id = 1<id>
        let line = 11<line>
        let column = 23<symbol>
        let packTrinity = Pack id line column         
        let offset = p.GetAbsoluteOffset packTrinity
        let packPair = PackPair 1<id> offset
        let actual = p.GetCoordinates packPair
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for the extreme lower values of column`` () =
        let id = 1<id>
        let line = 4<line>
        let column = 1<symbol>
        let packTrinity = Pack id line column         
        let offset = p.GetAbsoluteOffset packTrinity
        let packPair = PackPair 1<id> offset
        let actual = p.GetCoordinates packPair
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for the extreme upper values of column`` () =
        let id = 1<id>
        let line = 4<line>
        let column = 27<symbol>
        let packTrinity = Pack id line column         
        let offset = p.GetAbsoluteOffset packTrinity
        let packPair = PackPair 1<id> offset
        let actual = p.GetCoordinates packPair
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for the extreme lower values of line and column`` () =
        let id = 1<id>
        let line = 1<line>
        let column = 1<symbol>
        let packTrinity = Pack id line column         
        let offset = p.GetAbsoluteOffset packTrinity
        let packPair = PackPair 1<id> offset
        let actual = p.GetCoordinates packPair
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for the extreme upper values of line and column`` () =
        let id = 1<id>
        let line = 11<line>
        let column = 130<symbol>
        let packTrinity = Pack id line column         
        let offset = p.GetAbsoluteOffset packTrinity
        let packPair = PackPair 1<id> offset
        let actual = p.GetCoordinates packPair
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for line and column in received Map`` () =
        let basePass = @"../../../Tests/SourceText/Map.txt"
        let StreamElement = new StreamReader(basePass, System.Text.Encoding.UTF8)
        let map = p.GetMap StreamElement
        p.AddLine 2<id> map
        let line = 6<line>
        let column = 5<symbol>
        let packTrinity = Pack 2<id> line column         
        let offset = p.GetAbsoluteOffset packTrinity
        let packPair = PackPair 2<id> offset
        let actual = p.GetCoordinates packPair
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for get Map with empty line`` () =
        try
            let line = 4<line>
            let column = 5<symbol>
            let packTrinity = Pack 2<id> line column         
            let offset = p.GetAbsoluteOffset packTrinity
            let packPair = PackPair 2<id> offset
            let actual = p.GetCoordinates packPair
            Assert.AreEqual(line, actual.Line)            
            Assert.AreEqual(column, actual.Column)
        with 
        | ex -> ()

    [<Test>]
    member test.``Test for get right Map`` () =
        let basePass = @"../../../Tests/SourceText/LittleMap.txt"
        let StreamElement = new StreamReader(basePass, System.Text.Encoding.UTF8)
        let map = p.GetMap StreamElement
        let actual : int64<symbol>[] = [| 0L<symbol>; 11L<symbol>; 13L<symbol>; 31L<symbol>; 33L<symbol>; 
                                        48L<symbol>; 52L<symbol>;|]
        Assert.AreEqual(map, actual)

    [<Test>]
    member test.``Test for get right empty Map`` () =
        let basePass = @"../../../Tests/SourceText/EmptyMap.txt"
        let StreamElement = new StreamReader(basePass, System.Text.Encoding.UTF8)
        let map = p.GetMap StreamElement
        let actual : int64<symbol>[] = [| 0L<symbol> |]
        Assert.AreEqual(map, actual)

    [<Test>]
    member test.``Test for empty Map`` () =
        try
            let basePass = @"../../../Tests/SourceText/EmptyMap.txt"
            let StreamElement = new StreamReader(basePass, System.Text.Encoding.UTF8)
            let map = p.GetMap StreamElement
            p.AddLine 3<id> map
            let line = 4<line>
            let column = 5<symbol>
            let packTrinity = Pack 2<id> line column         
            let offset = p.GetAbsoluteOffset packTrinity
            let packPair = PackPair 2<id> offset
            let actual = p.GetCoordinates packPair
            Assert.AreEqual(line, actual.Line)
            Assert.AreEqual(column, actual.Column)
        with 
        | ex -> ()

    [<Test>]
    member test.``Test for line and column in received Map (Unix style)`` () =
        let basePass = @"../../../Tests/SourceText/Map (Unix style).txt"
        let StreamElement = new StreamReader(basePass, System.Text.Encoding.UTF8)
        let map = p.GetMap StreamElement
        p.AddLine 4<id> map
        let line = 6<line>
        let column = 5<symbol>
        let packTrinity = Pack 2<id> line column         
        let offset = p.GetAbsoluteOffset packTrinity
        let packPair = PackPair 2<id> offset
        let actual = p.GetCoordinates packPair
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for get right Map (Unix style)`` () =
        let basePass = @"../../../Tests/SourceText/LittleMap (Unix style).txt"
        let StreamElement = new StreamReader(basePass, System.Text.Encoding.UTF8)
        let map = p.GetMap StreamElement
        let actual : int64<symbol>[] = [| 0L<symbol>; 10L<symbol>; 11L<symbol>; 28L<symbol>; 29L<symbol>; 
                                        43L<symbol>; 47L<symbol>;|]
        Assert.AreEqual(map, actual)

    [<Test>]
    member test.``Test for get right empty Map (Unix style)`` () =
        let basePass = @"../../../Tests/SourceText/EmptyMap (Unix style).txt"
        let StreamElement = new StreamReader(basePass, System.Text.Encoding.UTF8)
        let map = p.GetMap StreamElement
        let actual : int64<symbol>[] = [| 0L<symbol> |]
        Assert.AreEqual(map, actual)

    [<Test>]
    member test.``Test for empty Map (Unix style)`` () =
        try
            let basePass = @"../../../Tests/SourceText/EmptyMap (Unix style).txt"
            let StreamElement = new StreamReader(basePass, System.Text.Encoding.UTF8)
            let map = p.GetMap StreamElement
            p.AddLine 5<id> map
            let line = 4<line>
            let column = 5<symbol>
            let packTrinity = Pack 2<id> line column         
            let offset = p.GetAbsoluteOffset packTrinity
            let packPair = PackPair 2<id> offset
            let actual = p.GetCoordinates packPair
            Assert.AreEqual(line, actual.Line)
            Assert.AreEqual(column, actual.Column)
        with 
        | ex -> ()


(*[<EntryPoint>]
let f _ = 
    (new UtilsTest ()).``Test for random values of line and column``()
    0*)