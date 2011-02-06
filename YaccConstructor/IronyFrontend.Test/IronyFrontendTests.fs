module Main

open IronyFrontendTests
open Yard.Frontends.IronyFrontend
open Yard.Core
open NUnit.Framework

let run ironyGrammar =
    let frontend = new IronyFrontend() :> IFrontend
    let ilTree = frontend.ParseGrammar ironyGrammar
    ilTree

[<TestFixture>]
type ``Irony frontend tests`` () =
    [<Test>]
    member test.``Seq test 1`` () =
        let res = run (new GSeq())
        printfn "tree: %A" res
        Assert.AreEqual(1,1)
        
    [<Test>] 
    member test.``Issue22 test`` () =
        let res = run (new TermName())
        printfn "tree: %A" res
        Assert.AreEqual(1,1)