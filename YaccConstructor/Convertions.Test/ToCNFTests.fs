//  ToCNFTests.fs contains unuit test for ToCNF conversions
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


module ToCNFTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Convertions.TransformAux
open NUnit.Framework
open ConvertionsTests

[<TestFixture>]
type ``CNF tests`` () =
    let basePath = System.IO.Path.Combine(convertionTestPath, "ToCNF")
    let fe = getFrontend("YardFrontend")
    let conversionCNF = "ToCNF"
    let conversionChain = "DeleteChainRule"
    let conversionEps = "DeleteEpsRule"

    [<Test>]
    member test.``Delete eps rule`` () =
        Namer.resetRuleEnumerator()
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"eps_0.yrd"))
        let result = ConvertionsManager.ApplyConvertion conversionEps loadIL
        let expected = 
            {
                info = {fileName = ""}
                head = None
                grammar =
                     [{
                            name = "s"
                            args = []
                            body =
                        
                                PSeq([                                        
                                        {dummyRule with rule = PRef (("x", (0, 0)),None)}
                                        ;{dummyRule with rule = PRef (("yard_exp_brackets_1", (0, 0)),None)}],None, None)                        
                            _public = true
                            metaArgs = []
                         };
                         {
                            name = "yard_exp_brackets_1"
                            args = []
                            body =
                             PAlt
                               (PSeq ([{dummyRule with rule = PRef (("y", (7, 8)),None)}],None,None),
                                PSeq ([{dummyRule with rule = PRef (("z", (9, 10)),None)}],None,None));
                            _public = false
                            metaArgs = []}]
                foot = None
                options = Map.empty
            }

        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
        result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)


    [<Test>]
    member test.``Delete chain-rule`` () =
        Namer.resetRuleEnumerator()
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"chain_0.yrd"))
        let result = ConvertionsManager.ApplyConvertion conversionChain loadIL
        let expected = 
             {
                info = {fileName = ""}
                head = None
                grammar =
                     [{
                            name = "s"
                            args = []
                            body =
                                PSeq([                                        
                                        {dummyRule with rule = PRef (("x", (0, 0)),None)}
                                        ;{dummyRule with rule = PRef (("yard_exp_brackets_1", (0, 0)),None)}
                                        ;{dummyRule with rule = PRef (("m", (0, 0)),None)}],None, None)
                            _public = true
                            metaArgs = []
                         };
                         {
                            name = "yard_exp_brackets_1"
                            args = []
                            body =
                             PAlt
                               (PSeq ([{dummyRule with rule = PRef (("y", (7, 8)),None)}],None,None),
                                PSeq ([{dummyRule with rule = PRef (("z", (9, 10)),None)}],None,None));
                            _public = false
                            metaArgs = []}]
                foot = None
                options = Map.empty
            }

        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
        result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)

    [<Test>]
    member test.``to CNF`` () =
        Namer.resetRuleEnumerator()
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"cnf_0.yrd"))
        let result = ConvertionsManager.ApplyConvertion conversionCNF loadIL
        let expected = 
             {
                info = {fileName = ""}
                head = None
                grammar =
                     [{
                            name = "s"
                            args = []
                            body =
                                PSeq([                                        
                                        {dummyRule with rule = PRef (("x", (0, 0)),None)}
                                        ;{dummyRule with rule = PRef (("yard_exp_brackets_1", (0, 0)),None)}
                                        ;{dummyRule with rule = PRef (("yard_exp_brackets_2", (0, 0)),None)}],None, None)                        
                            _public = true
                            metaArgs = []
                         };
                         {
                            name = "yard_exp_brackets_1"
                            args = []
                            body =
                             PAlt
                               (PSeq ([{dummyRule with rule = PRef (("y", (7, 8)),None)}],None,None),
                                PSeq ([{dummyRule with rule = PRef (("z", (9, 10)),None)}],None,None));
                            _public = false
                            metaArgs = []
                         };
                         {
                            name = "yard_exp_brackets_2"
                            args = []
                            body =
                             PAlt
                               (PSeq ([{dummyRule with rule = PRef (("m", (7, 8)),None)}],None,None),
                                PSeq ([{dummyRule with rule = PRef (("n", (9, 10)),None)}],None,None));
                            _public = false
                            metaArgs = []}]
                foot = None
                options = Map.empty
            }

        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
        result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)
