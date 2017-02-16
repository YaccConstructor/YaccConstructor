module GraphParsingTests

open System.IO
open QuickGraph
open NUnit.Framework
open RDFPerfomance

[<TestFixture>]
type ``Graph parsing tests``() =  
    member this._01_PrettySimpleCalc_SequenceInput () =
        Assert.True(true);

[<EntryPoint>]
let f x =
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.LowLatency
    let t = new ``Graph parsing tests``()
    RDFPerfomance.performTests ()
    0
