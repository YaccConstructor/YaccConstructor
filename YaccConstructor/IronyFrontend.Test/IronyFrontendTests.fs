module Main

open IronyFrontendTests
open Yard.Frontends.IronyFrontend
open Yard.Core
open NUnit.Framework

let run () =
    let frontend = new IronyFrontend() :> IFrontend
    let ilTree = frontend.ParseGrammar (new GSeq())
    ilTree

[<TestFixture>]
type ``Irony frontend tests`` () =
    [<Test>]
    member test.``Seq test 1`` () =
        let res = run ()
        printfn "tree: %A" res
        Assert.AreEqual(1,1)