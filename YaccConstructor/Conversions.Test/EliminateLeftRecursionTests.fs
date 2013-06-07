module EliminateLeftRecursionTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.Conversions.EliminateLeftRecursion
open NUnit.Framework
open ConversionsTests

[<TestFixture>]
type ``Conversions eliminate left recursion tests`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "EliminateLeftRecursion")
    let conversion = "EliminateLeftRecursion"
    
    let frontend = getFrontend "YardFrontend"

    let runTest srcFile =
        let srcFile = System.IO.Path.Combine(basePath, srcFile)
        let ilTree = frontend.ParseGrammar srcFile
        Namer.initNamer ilTree.grammar
        let ilTreeConverted = ConversionsManager.ApplyConversion conversion ilTree 
        let expected =
            try
                srcFile + ".ans" |> frontend.ParseGrammar
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