module EliminateLeftRecursionTests

open Yard.Core
open Yard.Core.IL
open NUnit.Framework
open ConversionsTests
open Yard.Core.Helpers

[<TestFixture>]
type ``Conversions eliminate left recursion tests`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "EliminateLeftRecursion")    

    let applyConversion = applyConversion eliminateLeftRecursion        

    let runTest srcFile =
        let srcFile = System.IO.Path.Combine(basePath, srcFile)
        let ilTree = fe.ParseGrammar srcFile
        Namer.initNamer ilTree.grammar
        let ilTreeConverted = applyConversion ilTree 
        let expected =
            try
                srcFile + ".ans" |> fe.ParseGrammar
            with
            | e -> printfn "%s" e.Message
                   failwith e.Message
        
        let dump1 = treeDump.Generate expected |> string
        let dump2 = treeDump.Generate ilTreeConverted |> string
        Assert.IsTrue (ILComparators.GrammarEqualsWithoutLineNumbers ilTreeConverted.grammar expected.grammar)

    [<Test>]
    member test.``Test immediate recursion``() = runTest "simple.yrd"

    [<Test>]
    member test.``Test hidden recursion``() = runTest "hidden.yrd"

    [<Test>]
    member test.``Test epsilon rule`` () = runTest "epsilon.yrd"