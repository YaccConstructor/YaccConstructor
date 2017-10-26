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


module ExpandInnerAltTests

open Yard.Core
open Yard.Core.IL
open Conversions.TransformAux
open NUnit.Framework
open ConversionsTests
open Yard.Core.Helpers

[<TestFixture>]
type ``Expand inner alts tests`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "ExpandInnerAlt")
    let path f = System.IO.Path.Combine(basePath, f)

    [<Test>]                                                                                            
    member test.``Alt in seq 1`` () =
        (verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t("x"),None)}
                 {dummyRule with rule = PRef (Source.t("yard_exp_brackets_1"),None)}]
        ) @ (
              simpleNotStartRules "yard_exp_brackets_1"
              <| PAlt
                   (PSeq ([{dummyRule with rule = PRef (Source.t("y"),None)}],None,None),
                    PSeq ([{dummyRule with rule = PRef (Source.t("z"),None)}],None,None))
        )
        |> runTest (path "altInSeq1.yrd") expandInnerAlt          

    [<Test>]
    member test.``Alt in seq 2`` () =
        (verySimpleRules "s"
            [{dummyRule with rule = PRef (Source.t "x",None)}
            ;{dummyRule with rule = PRef (Source.t "yard_exp_brackets_1", None)}
            ;{dummyRule with rule = PRef (Source.t "m", None)}]
        ) @ (
             simpleNotStartRules "yard_exp_brackets_1"
             <| PAlt
                 (PSeq ([{dummyRule with rule = PRef (Source.t "y", None)}],None,None),
                  PSeq ([{dummyRule with rule = PRef (Source.t "z", None)}],None,None))
        )
        |> runTest (path "altInSeq2.yrd") expandInnerAlt

    [<Test>]
    member test.``Alts in seq`` () =                
        (verySimpleRules "s"
            [{dummyRule with rule = PRef (Source.t "x", None)}
            ;{dummyRule with rule = PRef (Source.t "yard_exp_brackets_1", None)}
            ;{dummyRule with rule = PRef (Source.t "yard_exp_brackets_2", None)}]
        ) @ (
            simpleNotStartRules "yard_exp_brackets_1"
            <| PAlt
               (PSeq ([{dummyRule with rule = PRef (Source.t "y", None)}],None,None),
                PSeq ([{dummyRule with rule = PRef (Source.t "z", None)}],None,None))
        ) @ (
            simpleNotStartRules "yard_exp_brackets_2"
            <| PAlt
                (PSeq ([{dummyRule with rule = PRef (Source.t "m", None)}],None,None),
                 PSeq ([{dummyRule with rule = PRef (Source.t "n", None)}],None,None))
        )
        |> runTest (path "altsInSeq.yrd") expandInnerAlt

    [<Test>]
    member test.``Nested alts`` () =
        (verySimpleRules "s"
            [{dummyRule with rule = PRef (Source.t "yard_exp_brackets_1", None)}]
        ) @ (
            simpleNotStartRules "yard_exp_brackets_1"
            <| PAlt
                (PSeq ([{dummyRule with rule = PRef (Source.t "y", None)}],None,None),
                 PSeq ([{dummyRule with rule = PRef (Source.t "yard_exp_brackets_2", None)}],None,None))
            ) @ (
            simpleNotStartRules "yard_exp_brackets_2"
            <| PAlt
                (PSeq ([{dummyRule with rule = PRef (Source.t "m", None)}],None,None),
                 PSeq ([{dummyRule with rule = PRef (Source.t "n",None)}],None,None))
        )
        |> runTest (path "nestedAlts.yrd") expandInnerAlt