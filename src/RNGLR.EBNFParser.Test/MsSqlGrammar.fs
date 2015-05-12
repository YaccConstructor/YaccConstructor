module RNGLREBNFParserTest

open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.EBNF.Parser
open Yard.Generators.RNGLR.EBNF
open NUnit.Framework
open YC.Tests.Helper
open Yard.Examples.MSParser
open Lexer
open Yard.Utils.SourceText
open Yard.Utils.InfoClass
open Yard.Utils.StructClass
open LexerHelper
open System.IO
open YC.ReSharper.AbstractAnalysis.Languages.TSQL


[<TestFixture>]
type ``RNGLREBNF parser for MsSql grammar`` () =
    let printErr (num, token : 'a, msg) =
        printfn "Error in position %d on Token %A: %s" num token msg

    let printError (projInf : ProjInfo) tok msg file = 
            let coordinates = 
                let x,y = tokenPos tok 
                let x = projInf.GetCoordinates x
                let y = projInf.GetCoordinates y
                sprintf "(%A,%A) - (%A,%A)" (x.Line + 1<line>) x.Column (y.Line + 1<line>) y.Column
            let data =
                let d = tokenData tok
                if isLiteral tok then ""
                else (d :?> SourceText).text
            let name = tok |> tokenToNumber |> numToString
            printfn "Error in file %s at position %s on Token %s %s: %s" file coordinates name data msg
            printfn "%s" msg
           // dbg.drawGSSDot @"..\..\stack.dot"
            Assert.Fail msg 
    
    let runParserTest (file : string)= 
        let p = new ProjInfo()
        let mutable counter = 1<id>
        let StreamElement = new StreamReader(file, System.Text.Encoding.UTF8)

        let map = p.GetMap StreamElement
        Lexer.id <- counter
        p.AddLine counter map
        counter <- counter + 1<id>

        match justParse file with
        | Yard.Generators.RNGLR.EBNF.Parser.Error(num, tok, msg, _) -> 
            for toks in tok do
                printError p toks msg file
        | Yard.Generators.RNGLR.EBNF.Parser.Success (ast, _) -> 
            Assert.Pass()

    let basePath = "../../../../YC.Abstract.SQL/Tests/MSSqlParser"
    let bigFilesPath = "../../../../YC.Abstract.SQL/Tests/Materials/ms-sql"
    let spFolder = "sysprocs"
    let file name = System.IO.Path.Combine (basePath,name)
    let complexSpFile name = System.IO.Path.Combine (bigFilesPath, spFolder, name)
    [<Test>]
    member test.``test``() = 
        complexSpFile "sp_password.sql" |> runParserTest

    [<Test>]
    member test.``test_0050000.``() = 
        complexSpFile "test_0050000.sql" |> runParserTest

    [<Test>]
    member test.``test_0100000.``() = 
        complexSpFile "test_0100000.sql" |> runParserTest

    [<Test>]
    member test.``test_0150000.``() = 
        complexSpFile "test_0150000.sql" |> runParserTest

    [<Test>]
    member test.``test_0200000.``() = 
        complexSpFile "test_0200000.sql" |> runParserTest

    [<Test>]
    member test.``test_0250000.``() = 
        complexSpFile "test_0250000.sql" |> runParserTest

    [<Test>]
    member test.``test_0300000.``() = 
        complexSpFile "test_0300000.sql" |> runParserTest

    [<Test>]
    member test.``test_0350000.``() = 
        complexSpFile "test_0350000.sql" |> runParserTest

    [<Test>]
    member test.``test_0400000.``() = 
        complexSpFile "test_0400000.sql" |> runParserTest

    [<Test>]
    member test.``test_0450000.``() = 
        complexSpFile "test_0450000.sql" |> runParserTest

    [<Test>]
    member test.``test_0500000.``() = 
        complexSpFile "test_0500000.sql" |> runParserTest

    [<Test>]
    member test.``test_0550000.``() = 
        complexSpFile "test_0550000.sql" |> runParserTest

    [<Test>]
    member test.``test_0600000.``() = 
        complexSpFile "test_0600000.sql" |> runParserTest

    [<Test>]
    member test.``test_0650000.``() = 
        complexSpFile "test_0650000.sql" |> runParserTest

    [<Test>]
    member test.``test_0700000.``() = 
        complexSpFile "test_0700000.sql" |> runParserTest

    [<Test>]
    member test.``test_0750000.``() = 
        complexSpFile "test_0750000.sql" |> runParserTest

    [<Test>]
    member test.``test_0800000.``() = 
        complexSpFile "test_0800000.sql" |> runParserTest

    [<Test>]
    member test.``test_0850000.``() = 
        complexSpFile "test_0850000.sql" |> runParserTest

    [<Test>]
    member test.``test_0900000.``() = 
        complexSpFile "test_0900000.sql" |> runParserTest

    [<Test>]
    member test.``test_0950000.``() = 
        complexSpFile "test_0950000.sql" |> runParserTest

    [<Test>]
    member test.``test_1000000.``() = 
        complexSpFile "test_1000000.sql" |> runParserTest