

module GenTests

open FsCheck
open Microsoft.FSharp.Text.Lexing
open Yard.Core.Main
open Yard.Core.GrammarParser

open NUnit.Framework

open Yard.Generators.RecursiveAscent.GrammarPreparer
open Yard.Generators.RecursiveAscent.Generator
open Yard.Generators.RecursiveAscent.IO
open Yard.Generators.RecursiveAscent


module module2 =
    [<TestFixture>]
    type ``Generator tests`` () =
          let rule = createStartRule "testrule" "testRule"
          let rule2 = createStartRule "testrule2" "testRule2"
          let _grammar = [rule; rule2]

          [<Test>] member test.``condition`` () =
                     let prop (x:int ) = (x > 5) ==> ( x > 4)
                                         |> trivial (x < 10)
                     quickCheck prop 


          [<Test>] member test.``get_all_t test`` () = 

                     Assert.AreEqual(3,get_all_t _grammar)


          [<Test>] member test.``generate test `` () =
                    Assert.AreEqual(  (_generate _grammar  ).ToString, "") 
          
          [<Test>] member test.``create_NFA test`` () =
                    let path = @"..\..\..\..\Tests\test002.yrd"
                    let codeGenerator = new CodeGenerator(path,path+".fs")
                    let finitAutomata = new FinitAutomataCreator(codeGenerator)
                    Assert.AreEqual(finitAutomata.FA_rules , "")           