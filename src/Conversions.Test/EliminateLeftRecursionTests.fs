module EliminateLeftRecursionTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Definition
open Yard.Core.Conversions.EliminateLeftRecursion
open NUnit.Framework
open ConversionsTests
open Yard.Core.Helpers

[<TestFixture>]
type ``Conversions eliminate left recursion tests`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "EliminateLeftRecursion")    

    let applyConversion loadIL = 
        {
            loadIL
                with grammar = (new Conversions.EliminateLeftRecursion.EliminateLeftRecursion()).ConvertGrammar (loadIL.grammar, [||])                               
        }
    
    let frontend = getFrontend "YardFrontend"

    let runTest srcFile =
        let srcFile = System.IO.Path.Combine(basePath, srcFile)
        let ilTree = frontend.ParseGrammar srcFile
        Namer.initNamer ilTree.grammar
        let ilTreeConverted = applyConversion ilTree 
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