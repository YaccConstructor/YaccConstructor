module Yard.Utils.OffsetMap.Test

open System.IO
open NUnit.Framework

[<TestFixture>]
type UtilsTest () =
    [<Test>]
    member test.``Test for random values of line and column`` () =
        let map : int[] = [| 0; 30; 44; 88; 116; 274; 333; 448; 511; 690; 820; 1042 |]
        let line = 5
        let column = 20
        let offset = Yard.Utils.OffsetMap.getAbsoluteOffset map line column
        let actual = Yard.Utils.OffsetMap.getCoordinates map offset
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for the extreme lower values of line`` () =
        let map : int[] = [| 0; 30; 44; 88; 116; 274; 333; 448; 511; 690; 820; 1042 |]
        let line = 1
        let column = 25
        let offset = Yard.Utils.OffsetMap.getAbsoluteOffset map line column
        let actual = Yard.Utils.OffsetMap.getCoordinates map offset
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for the extreme upper values of line`` () =
        let map : int[] = [| 0; 30; 44; 88; 116; 274; 333; 448; 511; 690; 820; 1042 |]
        let line = 11
        let column = 23
        let offset = Yard.Utils.OffsetMap.getAbsoluteOffset map line column
        let actual = Yard.Utils.OffsetMap.getCoordinates map offset
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for the extreme lower values of column`` () =
        let map : int[] = [| 0; 30; 44; 88; 116; 274; 333; 448; 511; 690; 820; 1042 |]
        let line = 4
        let column = 1
        let offset = Yard.Utils.OffsetMap.getAbsoluteOffset map line column
        let actual = Yard.Utils.OffsetMap.getCoordinates map offset
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for the extreme upper values of column`` () =
        let map : int[] = [| 0; 30; 44; 88; 116; 274; 333; 448; 511; 690; 820; 1042 |]
        let line = 4
        let column = 27 
        let offset = Yard.Utils.OffsetMap.getAbsoluteOffset map line column
        let actual = Yard.Utils.OffsetMap.getCoordinates map offset
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for the extreme lower values of line and column`` () =
        let map : int[] = [| 0; 30; 44; 88; 116; 274; 333; 448; 511; 690; 820; 1042 |]
        let line = 1
        let column = 1
        let offset = Yard.Utils.OffsetMap.getAbsoluteOffset map line column
        let actual = Yard.Utils.OffsetMap.getCoordinates map offset
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for the extreme upper values of line and column`` () =
        let map : int[] = [| 0; 30; 44; 88; 116; 274; 333; 448; 511; 690; 820; 1042 |]
        let line = 11
        let column = 130
        let offset = Yard.Utils.OffsetMap.getAbsoluteOffset map line column
        let actual = Yard.Utils.OffsetMap.getCoordinates map offset
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column)

    [<Test>]
    member test.``Test for line and column in received Map`` () =
        let basePass = @"../../../../Tests/SourceText/Map.txt"
        let map : int[] = Yard.Utils.OffsetMap.getMap basePass
        let line = 6
        let column = 5
        let offset = Yard.Utils.OffsetMap.getAbsoluteOffset map line column
        let actual = Yard.Utils.OffsetMap.getCoordinates map offset
        Assert.AreEqual(line, actual.Line)
        Assert.AreEqual(column, actual.Column) 

    [<Test>]
    member test.``Test for get Map with empty line`` () =
        try
            let basePass = @"../../../../Tests/SourceText/Map.txt"
            let map : int[] = Yard.Utils.OffsetMap.getMap basePass
            let line = 4
            let column = 5
            let offset = Yard.Utils.OffsetMap.getAbsoluteOffset map line column
            let actual = Yard.Utils.OffsetMap.getCoordinates map offset
            Assert.AreEqual(line, actual.Line)
            Assert.AreEqual(column, actual.Column)
        with 
        | ex -> ()

    [<Test>]
    member test.``Test for get right Map`` () =
        let basePass = @"../../../../Tests/SourceText/LittleMap.txt"
        let map : int[] = Yard.Utils.OffsetMap.getMap basePass
        let actual = [| 0; 10; 11; 28; 29; 43; 48 |]
        Assert.AreEqual(map, actual)

    [<Test>]
    member test.``Test for get right empty Map`` () =
        let basePass = @"../../../../Tests/SourceText/EmptyMap.txt"
        let map : int[] = Yard.Utils.OffsetMap.getMap basePass
        let actual = [| 0 |]
        Assert.AreEqual(map, actual)

    [<Test>]
    member test.``Test for empty Map`` () =
        try
            let basePass = @"../../../../Tests/SourceText/EmptyMap.txt"
            let map : int[] = Yard.Utils.OffsetMap.getMap basePass
            let line = 4
            let column = 5
            let offset = Yard.Utils.OffsetMap.getAbsoluteOffset map line column
            let actual = Yard.Utils.OffsetMap.getCoordinates map offset
            Assert.AreEqual(line, actual.Line)
            Assert.AreEqual(column, actual.Column)
        with 
        | ex -> ()