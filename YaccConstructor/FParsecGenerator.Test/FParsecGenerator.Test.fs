module Yard.Generators.FParsecGenerator.Test

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Yard.Core.Checkers
open Yard.Tests.Helper
open NUnit.Framework
open System.Linq
open System.IO
open Lexer
open calc
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

let generated = "Generated"

[<TestFixture>]
type ``FParsec generator tests`` () =
    let iGenerator = new Yard.Generators.FParsecGenerator.FParsecGenerator()
    let parser = new Yard.Frontends.YardFrontend.YardFrontend()
    let basePath = "../../../Tests/FParsec"

    let fixIl ilTree =
        let ilTree = ref ilTree
        for constr in iGenerator.Constraints do
            let grammar = ilTree.Value.grammar
            if not <| constr.Check grammar then
                printfn "Constraint %s: applying %s..." constr.Name constr.Conversion.Name
                ilTree := {!ilTree with grammar = constr.Fix grammar}
        !ilTree

    [<Test>]
    member test.``Right generation of file`` () =  
        let inFile = "calc.yrd"
        let resultFile = System.IO.Path.GetFileNameWithoutExtension inFile + ".fs"
        let inFullPath = Path.Combine(basePath, inFile)
        let resultFullPath = Path.Combine(basePath, resultFile)
        let expectedFullPath = Path.Combine [|basePath; generated; resultFile|]
        let il = parser.ParseGrammar inFullPath |> fixIl
        let code = iGenerator.Generate il
        System.IO.File.Exists resultFullPath |> Assert.IsTrue
        filesAreEqual resultFullPath expectedFullPath      

    [<Test>]
    member test.``Right calculation`` () = 
        let compCalc = ws >>. expr  .>> eof
        let gogo s = run compCalc s
        let checkInPut =
            function
            | Success (v, _, _)  -> Assert.AreEqual(v.ToString(), "56")
            | Failure (msg, err, _) -> printf "%s" msg; failwith msg
        ( gogo  "2+2*3**(2+1)" ) |> checkInPut 

    [<Test>]
    member test.``Test for literals`` () =
        let inFile = "literals.yrd"
        let resultFile = System.IO.Path.GetFileNameWithoutExtension inFile + ".fs"
        let inFullPath = Path.Combine(basePath, inFile)
        let resultFullPath = Path.Combine(basePath, resultFile)
        let expectedFullPath = Path.Combine [|basePath; generated; resultFile|]
        let il = parser.ParseGrammar inFullPath |> fixIl
        let code = iGenerator.Generate il
        System.IO.File.Exists resultFullPath |> Assert.IsTrue
        filesAreEqual resultFullPath expectedFullPath

    [<Test>]
    member test.``Right calculation for literals`` () = 
        let compCalc = ws >>. literals.s  .>> eof
        let gogo s = run compCalc s
        let checkInPut =
            function
            | Success (v, _, _)  -> Assert.AreEqual(v.ToString(), "aaaa")
            | Failure (msg, err, _) -> printf "%s" msg; failwith msg
        ( gogo  "aaaa" ) |> checkInPut 