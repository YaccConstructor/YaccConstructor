module RNGLREBNFParserTest

open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.EBNF.Parser
open Yard.Generators.RNGLR.EBNF
open NUnit.Framework
open YC.Tests.Helper
open Yard.Examples.MSParser


[<TestFixture>]
type ``RNGLREBNF parser for MsSql grammar`` () =
    [<Test>]
    member test.``Test`` () =
        Assert.AreEqual(2, 2)