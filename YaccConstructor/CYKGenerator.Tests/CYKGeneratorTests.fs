module CYKGeneratorTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Yard.Core.Checkers
open NUnit.Framework
open System.Linq
open System.IO


[<TestFixture>]
type ``CYK generator tests`` () =
    let generator = new Yard.Generators.CYKGenerator.CYKGeneartorImpl()
    let parser = new Yard.Frontends.YardFrontend.YardFrontend()
    let basePath = "../../../../Tests/CYK"

    [<Test>]
    member test.``Simple one rule without lable test`` () =        
        let il = parser.ParseGrammar(Path.Combine(basePath, "basic_noLBL.yrd"))
        let result = generator.GenRulesList il
        Assert.AreEqual(result.Length,1)
        Assert.AreEqual(result.[0], 281479271743488UL)

    [<Test>]
    member test.``Simple one rule with lable without weight test`` () = 
        let il = parser.ParseGrammar(Path.Combine(basePath, "basic_LBL_no_weight.yrd"))
        let result = generator.GenRulesList il
        Assert.AreEqual(result.Length,1)
        Assert.AreEqual(result.[0], 281479271743744UL)

    [<Test>]
    member test.``Simple one rule term with lable without weight test`` () =        
        let il = parser.ParseGrammar(Path.Combine(basePath, "basic_term_LBL_no_weight.yrd"))
        let result = generator.GenRulesList il
        Assert.AreEqual(result.Length,1)
        Assert.AreEqual(result.[0], 281479271678208UL)

    [<Test>]
    member test.``Simple one rule term without lable test`` () =        
        let il = parser.ParseGrammar(Path.Combine(basePath, "basic_term_noLBL.yrd"))        
        let result = generator.GenRulesList il
        Assert.AreEqual(result.Length,1)
        Assert.AreEqual(result.[0], 281479271677952UL)

    [<Test>]
    member test.``Simple one rule term without lable code gen test`` () =        
        let il = parser.ParseGrammar(Path.Combine(basePath, "basic_term_noLBL.yrd"))
        let expectedCode = 
            ["namespace Yard.Generators.CYK"
            ; ""
            ; "open Yard.Core"
            ; "type cykToken = "
            ; "  | NUM"
            ; "let getTag token = "
            ; "  match token with "
            ; "  | NUM -> 1"
            ; "let rules = "
            ; "  [ 281479271677952u ]"
            ; "  |> Array.ofList"
            ] |> String.concat "\n"

        let code = generator.Generate il
        printfn "%s" expectedCode
        printfn "%s" "**************************"
        printfn "%s" code        
        Assert.AreEqual(expectedCode, code)        



