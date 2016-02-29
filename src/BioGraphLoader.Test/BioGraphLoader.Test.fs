module YC.BIO.BioGraphLoader.Test

open NUnit.Framework
open YC.BIO.BioGraphLoader
open System.IO

[<TestFixture>]
type ``Biology graph loader tests``() = 
    let basePath = "../../../Tests/bio/"

    [<Test>]
    member this.``From file to QuickGraph``() = 
        let path = Path.Combine(basePath,"biodata_1/saves/00_before_repeat_resolution/graph")        
        let g = loadGraphFormFileToQG path 100
        Assert.AreEqual(597,g.VertexCount,"Nodes count mismatch")
        Assert.AreEqual(910,g.EdgeCount,"Edges count mismatch")