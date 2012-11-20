module Yard.Utils.SourceText.Test

open NUnit.Framework

[<TestFixture>]
type ``UtilsTest`` () =
    [<Test>]
    member test.``Test1`` () =
        let id = 11
        let column = 12
        let line = 17
        let pack = Yard.Utils.SourceText.Pack id column line
        let actual = Yard.Utils.SourceText.RePack pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(column, actual.Column)
        Assert.AreEqual(line, actual.Line)


    [<Test>]
    member test.``Test2`` () = 
        let trinity : uint64 = 111UL
        let (repack : Trinity) = Yard.Utils.SourceText.RePack trinity
        let actual = Yard.Utils.SourceText.Pack repack.Id repack.Column repack.Line
        Assert.AreEqual(trinity, actual)


    [<Test>]
    member test.``Test3`` () =
        let id = 0
        let column = 0
        let line = 0
        let pack = Yard.Utils.SourceText.Pack id column line
        let actual = Yard.Utils.SourceText.RePack pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(column, actual.Column)
        Assert.AreEqual(line, actual.Line)


    [<Test>]
    member test.``Test4`` () =
        let id = 0
        let column = 0
        let line = 1

        let pack = Yard.Utils.SourceText.Pack id column line
        let actual = Yard.Utils.SourceText.RePack pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(column, actual.Column)
        Assert.AreEqual(line, actual.Line)


    [<Test>]
    member test.``Test5`` () =
        let id = 131071
        let column = 131071
        let line = 1073741823
        let pack = Yard.Utils.SourceText.Pack id column line
        let actual = Yard.Utils.SourceText.RePack pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(column, actual.Column)
        Assert.AreEqual(line, actual.Line)


    [<Test>]
    member test.``Test6`` () = 
        let trinity : uint64 = 0UL
        let (repack : Trinity) = Yard.Utils.SourceText.RePack trinity
        let actual = Yard.Utils.SourceText.Pack repack.Id repack.Column repack.Line
        Assert.AreEqual(trinity, actual)

    [<Test>]
    member test.``Test7`` () = 
        let trinity : uint64 = 18446744073709551615UL
        let (repack : Trinity) = Yard.Utils.SourceText.RePack trinity
        let actual = Yard.Utils.SourceText.Pack repack.Id repack.Column repack.Line
        Assert.AreEqual(trinity, actual)

