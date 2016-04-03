module RAGeneratorTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Yard.Core.Checkers
open YC.Tests.Helper
open NUnit.Framework
open System.IO

[<TestFixture>]
type ``RA generator tests`` () =
    let generator = new Yard.Generators.RAGenerator.RAGenerator()
    let parser = new Yard.Frontends.YardFrontend.YardFrontend()
    let basePath = "../../../../Tests/RA"

    [<Test>]
    member test.``RATest1`` () =        
        let il = parser.ParseGrammar(Path.Combine(basePath, "sampleRA.yrd"))
        let x = generator.Generate il
        ()