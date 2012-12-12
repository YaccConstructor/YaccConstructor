//  Driver.fs contains tests of MS-SQL parser.
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

module ParserTests

open NUnit.Framework

[<TestFixture>]
type ``MS-SQL parser tests`` () =
    let runParserTest file = 
        match MSSqlParser.justParse file with
        | Yard.Generators.RNGLR.Parser.Error (num, tok, msg,_) ->
            let msg = sprintf "Error in file %s on position %d on Token %A: %s" file num tok msg
            printfn "%s" msg
            Assert.Fail(msg)
        | Yard.Generators.RNGLR.Parser.Success ast ->
            Assert.Pass()

    let basePath = "../../../../../Tests/MSSqlParser"
    let file name = System.IO.Path.Combine(basePath,name)

    [<Test>]
    member test.``Top level set.`` () =
        file "TopLevelSet.sql" |> runParserTest

    [<Test>]
    member test.``Top level sets.`` () =
        file "TopLevelSets.sql" |> runParserTest

    [<Test>]
    member test.``Create procedure without parameters.`` () =
        file "CreateProcWithoutParams.sql" |> runParserTest

    [<Test>]
    member test.``Create procedure.`` () =
        file "CreateProc.sql" |> runParserTest

    [<Test>]
    member test.``Declare local vars.`` () =
        file "DeclareLocalVars.sql" |> runParserTest