// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

module RnglrParserTsqlChunkDriverTest

open NUnit.Framework
open LexerHelper
open Yard.Utils.StructClass
open Yard.Utils.SourceText
open Yard.Utils.InfoClass
open Yard.Examples.MSParser
open Lexer
open System.IO
open System.Threading
open YC.ReSharper.Languages.TSQL.RnglrChunkDriver

[<TestFixture>]
type ``MS-SQL parser tests`` () =

    let printError (projInf : ProjInfo) tok msg dbg file = 
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
        | Yard.Generators.RNGLR.Parser.Error (num, tok, msg, dbg, _) ->
            for toks in tok do
                printError p toks msg dbg file
        | Yard.Generators.RNGLR.Parser.Success (ast, _, _) ->
            //defaultAstToDot ast (file + ".dot")
            Assert.Pass()

    let basePath = "../../../Tests/MSSqlParser"
    let bigFilesPath = "../../../Tests/Materials/ms-sql"
    let spFolder = "sysprocs"
    let file name = System.IO.Path.Combine (basePath,name)
    let complexSpFile name = System.IO.Path.Combine (bigFilesPath, spFolder, name)

    [<Test>]
    member test.``Top level set.`` () =
        file "TopLevelSet.sql" |> runParserTest

    [<Test>]
    member test.``Case statement.`` () =
        file "CaseStmt.sql" |> runParserTest


    [<Test>]
    member test.``Top level sets.`` () =
        file "TopLevelSets.sql" |> runParserTest

    [<Test>]
    member test.``Create procedure without parameters.`` () =
        file "CreateProcWithoutParams.sql" |> runParserTest

    [<Test>]
    member test.``Select local var.`` () =
        file "SelectLocalVar.sql" |> runParserTest

    [<Test>]
    member test.``Create procedure.`` () =
        file "CreateProc.sql" |> runParserTest

    [<Test>]
    member test.``Declare local vars.`` () =
        file "DeclareLocalVars.sql" |> runParserTest

    [<Test>]
    member test.``Begin transaction.`` () =
        file "BeginTransaction.sql" |> runParserTest

    [<Test>]
    member test.``Begin mark transaction.`` () =
        file "MarkBeginTransaction.sql" |> runParserTest

    //[<Test>]
    member test.``Execute procedure. Very simple test.`` () =
        file "ExecProc_1.sql" |> runParserTest
        
    [<Test>]
    member test.``sp_addlogin complex test.`` () =
        complexSpFile "sp_addlogin.sql" |> runParserTest

    [<Test>]
    member test.``sp_addextendedproperty complex test.`` () =
        complexSpFile "sp_addextendedproperty.sql" |> runParserTest

    [<Test>]
    member test.``sp_addserver complex test.`` () =
        complexSpFile "sp_addserver.sql" |> runParserTest

    [<Test>]
    member test.``sp_adduser complex test.`` () =
        complexSpFile "sp_adduser.sql" |> runParserTest

    [<Test>]
    member test.``sp_autostats complex test.`` () =
        complexSpFile "sp_autostats.sql" |> runParserTest

    
    [<Test>]
    member test.``sp_droplogin complex test.`` () =
        complexSpFile "sp_droplogin.sql" |> runParserTest
   
    
    [<Test>]
    member test.``sp_help complex test.`` () =
        complexSpFile "sp_help.sql" |> runParserTest

    
    [<Test>]
    member test.``sp_helpindex complex test.`` () =
        complexSpFile "sp_helpindex.sql" |> runParserTest

    [<Test>]
    member test.``sp_password complex test.`` () =
        complexSpFile "sp_password.sql" |> runParserTest

    
    [<Test>]
    member test.``sp_revokedbaccess complex test.`` () =
        complexSpFile "sp_revokedbaccess.sql" |> runParserTest

    
    [<Test>]
    member test.``sp_tables_ex complex test.`` () =
        complexSpFile "sp_tables_ex.sql" |> runParserTest

    [<Test>]
    member test.``big test with enlarged stack``() =
        complexSpFile "test.sql" |> runParserTest
        let parseTask () = 
            complexSpFile "test.sql" |> runParserTest
        let thread = new Thread(parseTask, 20000000)
        thread.Name <- "BigTestParse"
        thread.Start()
        thread.Join()

[<EntryPoint>]
let main argv = 
    //(new ``MS-SQL parser tests``()).``big test with enlarged stack``()
    printfn "%A" argv
    0 // return an integer exit code
