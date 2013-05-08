module Yard.Utils.SourceText.Test

open NUnit.Framework
open Yard.Utils.InfoClass
open Yard.Utils.StructClass

//tests for Pair: Pack, RePack

[<TestFixture>]
type UtilsTest () =
    [<Test>]
    member test.``Test for random values of id column line`` () =
        let id = 11<id>
        let column = 12<symbol>
        let line = 17<line>
        let pack = Yard.Utils.SourceText.Pack id line column
        let actual = Yard.Utils.SourceText.RePack pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(column, actual.Column)
        Assert.AreEqual(line, actual.Line)


    [<Test>]
    member test.``Test for random value of trinity`` () = 
        let trinity = 111UL
        let repack = Yard.Utils.SourceText.RePack trinity
        let actual = Yard.Utils.SourceText.Pack repack.Id repack.Line repack.Column
        Assert.AreEqual(trinity, actual)


    [<Test>]
    member test.``Test for the extreme lower values of id column line`` () =
        let id = 0<id>
        let column = 0<symbol>
        let line = 0<line>
        let pack = Yard.Utils.SourceText.Pack id line column 
        let actual = Yard.Utils.SourceText.RePack pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(column, actual.Column)
        Assert.AreEqual(line, actual.Line)


    [<Test>]
    member test.``Test for the extreme lower values of id column line (with using value equaled 1)`` () =
        let id = 0<id>
        let column = 0<symbol>
        let line = 1<line>
        let pack = Yard.Utils.SourceText.Pack id line column 
        let actual = Yard.Utils.SourceText.RePack pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(column, actual.Column)
        Assert.AreEqual(line, actual.Line)


    [<Test>]
    member test.``Test for the extreme upper values of id column line`` () =
        let id = 131071<id>
        let column = 131071<symbol>
        let line = 1073741823<line>
        let pack = Yard.Utils.SourceText.Pack id line column 
        let actual = Yard.Utils.SourceText.RePack pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(column, actual.Column)
        Assert.AreEqual(line, actual.Line)


    [<Test>]
    member test.``Test for the extreme lower value of trinity`` () = 
        let trinity = 0UL
        let repack = Yard.Utils.SourceText.RePack trinity
        let actual = Yard.Utils.SourceText.Pack repack.Id repack.Line repack.Column 
        Assert.AreEqual(trinity, actual)

    [<Test>]
    member test.``Test for the extreme upper value of trinity`` () = 
        let trinity = 18446744073709551615UL
        let repack = Yard.Utils.SourceText.RePack trinity
        let actual = Yard.Utils.SourceText.Pack repack.Id repack.Line repack.Column 
        Assert.AreEqual(trinity, actual)

    [<Test>]
    member test.``Test for random values of id offset`` () =
        let id = 11<id>
        let offset = 1567L<symbol>
        let pack = Yard.Utils.SourceText.PackPair id offset
        let actual = Yard.Utils.SourceText.RePackPair pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(offset, actual.AbsoluteOffset)


    [<Test>]
    member test.``Test for random value of pair`` () = 
        let pair = 111UL
        let repack = Yard.Utils.SourceText.RePackPair pair
        let actual = Yard.Utils.SourceText.PackPair repack.Id repack.AbsoluteOffset
        Assert.AreEqual(pair, actual)


    [<Test>]
    member test.``Test for the extreme lower values of id offset`` () =
        let id = 0<id>
        let offset = 0L<symbol>
        let pack = Yard.Utils.SourceText.PackPair id offset
        let actual = Yard.Utils.SourceText.RePackPair pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(offset, actual.AbsoluteOffset)


    [<Test>]
    member test.``Test for the extreme upper values of id offset`` () =
        let id = 131071<id>
        let offset = 140736414482432L<symbol>
        let pack = Yard.Utils.SourceText.PackPair id offset
        let actual = Yard.Utils.SourceText.RePackPair pack
        Assert.AreEqual(id, actual.Id)
        Assert.AreEqual(offset, actual.AbsoluteOffset)


    [<Test>]
    member test.``Test for the extreme lower value of pair`` () = 
        let pair = 0UL
        let repack = Yard.Utils.SourceText.RePackPair pair
        let actual = Yard.Utils.SourceText.PackPair repack.Id repack.AbsoluteOffset
        Assert.AreEqual(pair, actual)

    [<Test>]
    member test.``Test for the extreme upper value of pair`` () = 
        let pair = 18446744073709551615UL
        let repack = Yard.Utils.SourceText.RePackPair pair
        let actual = Yard.Utils.SourceText.PackPair repack.Id repack.AbsoluteOffset
        Assert.AreEqual(pair, actual)

