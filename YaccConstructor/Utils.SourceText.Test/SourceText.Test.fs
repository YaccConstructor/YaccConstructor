module Yard.Utils.SourceText.Test

open NUnit.Framework

[<TestFixture>]
type UtilsTest () =
    [<Test>]
    member test.``Test for random values of id column line`` () =
        let id = 11
        let column = 12
        let line = 17
        let pack = Yard.Utils.SourceText.Pack id column line
        let actual = Yard.Utils.SourceText.RePack pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(column, actual.Column)
        Assert.AreEqual(line, actual.Line)


    [<Test>]
    member test.``Test for random value of trinity`` () = 
        let trinity = 111UL
        let repack = Yard.Utils.SourceText.RePack trinity
        let actual = Yard.Utils.SourceText.Pack repack.Id repack.Column repack.Line
        Assert.AreEqual(trinity, actual)


    [<Test>]
    member test.``Test for the extreme lower values of id column line`` () =
        let id = 0
        let column = 0
        let line = 0
        let pack = Yard.Utils.SourceText.Pack id column line
        let actual = Yard.Utils.SourceText.RePack pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(column, actual.Column)
        Assert.AreEqual(line, actual.Line)


    [<Test>]
    member test.``Test for the extreme lower values of id column line (with using value equaled 1)`` () =
        let id = 0
        let column = 0
        let line = 1
        let pack = Yard.Utils.SourceText.Pack id column line
        let actual = Yard.Utils.SourceText.RePack pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(column, actual.Column)
        Assert.AreEqual(line, actual.Line)


    [<Test>]
    member test.``Test for the extreme upper values of id column line`` () =
        let id = 131071
        let column = 131071
        let line = 1073741823
        let pack = Yard.Utils.SourceText.Pack id column line
        let actual = Yard.Utils.SourceText.RePack pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(column, actual.Column)
        Assert.AreEqual(line, actual.Line)


    [<Test>]
    member test.``Test for the extreme lower value of trinity`` () = 
        let trinity = 0UL
        let repack = Yard.Utils.SourceText.RePack trinity
        let actual = Yard.Utils.SourceText.Pack repack.Id repack.Column repack.Line
        Assert.AreEqual(trinity, actual)

    [<Test>]
    member test.``Test for the extreme upper value of trinity`` () = 
        let trinity = 18446744073709551615UL
        let repack = Yard.Utils.SourceText.RePack trinity
        let actual = Yard.Utils.SourceText.Pack repack.Id repack.Column repack.Line
        Assert.AreEqual(trinity, actual)

