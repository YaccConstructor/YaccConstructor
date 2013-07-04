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
    let runParserTest (file:string) = 
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

    let basePath = "../../../../../Tests/PlSqlParser"
    let filePath = "../../../../../Tests/materials/pl-sql/jrxml2pdf-release/install"
    let file name = System.IO.Path.Combine (basePath,name)
    let file_1 name = System.IO.Path.Combine (filePath,name)

    [<Test>]
    member test.``Exucute procedure with assosiation operation.`` () =
        file "exec_proc_1.sql" |> runParserTest

    [<Test>]
    member test.``Exucute procedure with two parametres.`` () =
        file "exec_proc_2.sql" |> runParserTest

    [<Test>]
    member test.``Exucute procedure with one parametr.`` () =
        file "exec_proc_3.sql" |> runParserTest

    [<Test>]
    member test.``Exucute procedure without parametres.`` () =
        file "exec_proc_4.sql" |> runParserTest

    [<Test>]
    member test.``Programm with some procudure.`` () =
        file_1 "ACL_FOR_GOOGLE_MAPS.sql" |> runParserTest

        


