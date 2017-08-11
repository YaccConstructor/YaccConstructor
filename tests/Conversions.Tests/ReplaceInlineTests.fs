//  ExpandInnerAltTests.fs contains unuit test for ExpandInnerAlt conversions
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

module ReplaceInlineTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Yard.Core.Helpers
open Conversions.TransformAux
open NUnit.Framework
open ConversionsTests
open Yard.Core.Helpers
open Conversions.ExpandInline

[<TestFixture>]
type ``Inline tests`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "Inline")
    let fe = getFrontend("YardFrontend")
    let conversion = new ReplaceInline()

    //[<Test>]
    member test.``Inline 1`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath, "inline1.yrd"))
        Namer.initNamer loadIL.grammar
        let result = {loadIL with grammar = conversion.ConvertGrammar loadIL.grammar}
        let rules = 
            (verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t("yard_exp_brackets_1"),None)}])
            @
            (verySimpleNotStartRules "yard_exp_brackets_1"
                [{dummyRule with rule = PRef (Source.t("x"),None)}
                ;{dummyRule with rule = PRef (Source.t("y"),None)}
                ]
            )
        let expected = defaultDefinition rules
        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
        result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)
