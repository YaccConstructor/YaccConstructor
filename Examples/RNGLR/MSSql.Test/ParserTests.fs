// Driver.fs contains tests of MS-SQL parser.
//
//  Copyright 2012 Semen Grigorev <rsdpisuy@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.


// id Drivers Test for memory
module ParserTests

open NUnit.Framework
open LexerHelper
open Yard.Utils.StructClass
open Yard.Utils.SourceText
open Yard.Utils.InfoClass
open Yard.Examples.MSParser
open System.IO

[<TestFixture>]
type ``MS-SQL parser tests`` () =
    let runParserTest (file : string)= 
        let p = new ProjInfo()
        let mutable counter = 1<id>
        let StreamElement = new StreamReader(file, System.Text.Encoding.UTF8)

        let map = p.GetMap StreamElement
        Lexer.id <- counter
        p.AddLine counter map
        counter <- counter + 1<id>
        match MSSqlParser.justParse file with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, msg,dbg) ->
            let coordinates = 
                let x,y = tokenPos tok
                let x = p.GetCoordinates x
                let y = p.GetCoordinates y
                sprintf "(%A,%A) - (%A,%A)" (x.Line + 1<line>) x.Column (y.Line + 1<line>) y.Column
            let data =
                let d = tokenData tok
                if isLiteral tok then ""
                else (d :?> SourceText).text
            let name = tok |> tokenToNumber |> numToString
            printfn "Error in file %s at position %s on Token %s %s: %s" file coordinates name data msg
            printfn "%s" msg
            dbg.drawGSSDot @"..\..\stack.dot"
            Assert.Fail msg
        | Yard.Generators.RNGLR.Parser.Success ast ->
            Assert.Pass()

    let basePath = "../../../../../Tests/MSSqlParser"
    let bigFilesPath = "../../../../../Tests/Materials/ms-sql"
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

    //[<Test>]
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
