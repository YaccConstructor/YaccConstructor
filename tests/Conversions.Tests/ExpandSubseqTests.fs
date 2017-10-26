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


module ExpandSubseqTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.Helpers
open Conversions.TransformAux
open NUnit.Framework
open ConversionsTests
open ConversionsTests
open Yard.Core.Helpers

[<TestFixture>]
type ``Expand subseq tests`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "ExpandSubseq")
    let path f = System.IO.Path.Combine(basePath, f)

    [<Test>]
    member test.``Subseq 1`` () =
        (verySimpleRules "s"
            [{dummyRule with rule = PRef (Source.t "yard_exp_brackets_1",None)}]
        ) @ (
            verySimpleNotStartRules "yard_exp_brackets_1"
                [{dummyRule with rule = PRef (Source.t("x"),None)}
                ;{dummyRule with rule = PRef (Source.t("y"),None)}]
        )
        |> runTest (path "subseq1.yrd") expandSubSeq

    [<Test>]
    member test.``Subseq 2`` () =
        (verySimpleRules "s"
            [{dummyRule with rule = PRef (Source.t("yard_exp_brackets_1"),None)}
            ;{dummyRule with rule = PRef (Source.t("yard_exp_brackets_2"),None)}]
        ) @ (
            verySimpleNotStartRules "yard_exp_brackets_1"
                [{dummyRule with rule = PRef (Source.t("x"),None)}
                ;{dummyRule with rule = PRef (Source.t("y"),None)}]
        ) @ (
            verySimpleNotStartRules "yard_exp_brackets_2"
                [{dummyRule with rule = PRef (Source.t("n"),None)}
                ;{dummyRule with rule = PRef (Source.t("m"),None)}]
        )
        |> runTest (path "subseq2.yrd") expandSubSeq
    
    [<Test>]
    member test.``Inner subseq`` () =
        (verySimpleRules "s"
            [{dummyRule with rule = PRef (Source.t "yard_exp_brackets_1",None)}]
        ) @ (
            verySimpleNotStartRules "yard_exp_brackets_1"
                [{dummyRule with rule = PRef (Source.t "x", None)}
                ;{dummyRule with rule = PRef (Source.t "yard_exp_brackets_2", None)}
                ;{dummyRule with rule = PRef (Source.t "y", None)}]
        ) @ (
            verySimpleNotStartRules "yard_exp_brackets_2"
                [{dummyRule with rule = PRef (Source.t "n", None)}
                ;{dummyRule with rule = PRef (Source.t "m", None)}]
        )
        |> runTest (path "innerSubseq.yrd") expandSubSeq