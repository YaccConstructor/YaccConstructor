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
    member test.``ToCNF test`` () =
        Namer.resetRuleEnumerator()
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"cnf_0.yrd"))
        let result = ConvertionsManager.ApplyConvertion conversionCNF loadIL
        let expected = 
            {
                info = {fileName = ""}
                head = None
                grammar = [{
                                name = "e"
                                args = []
                                body = 
                                    PSeq (
                                            [{
                                                omit = false
                                                rule = PRef (("new_A", (0, 0)),None)
                                                binding = None
                                                checker = None
                                            }; 
                                            {
                                                omit = false
                                                rule = PRef (("e", (6, 7)),None)
                                                binding = None
                                                checker = None
                                            }],
                                            None,
                                            None)
                                _public = true
                                metaArgs = []
                          };
                          {
                                name = "new_A"
                                args = []
                                body = 
                                    PSeq (
                                            [{
                                                omit = false
                                                rule = PToken ("A", (4, 5))
                                                binding = None
                                                checker = None
                                            }],
                                            None,
                                            None
                                         )   
                                _public = false
                                metaArgs = []
                          }]
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
            {info = {fileName = ""}
             head = None
             grammar =
                [{
                        name = "e"
                        args = []
                        body = PSeq ([{omit = false
                                       rule = PToken ("STRING", (11, 17))
                                       binding = None
                                       checker = None}],None,None)
                        _public = true
                        metaArgs = []
                }; 
                {
                        name = "s"
                        args = []
                        body = PSeq ([{ omit = false
                                        rule = PToken ("STRING", (11, 17))
                                        binding = None
                                        checker = None}],None,None)
                        _public = false
                        metaArgs = []
                }]
             foot = None
             options = Map.empty}

        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
        result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)

    [<Test>]
    member test.``delete Eps rule test`` () =
        Namer.resetRuleEnumerator()
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"eps_0.yrd"))
        let result = ConvertionsManager.ApplyConvertion conversionEps loadIL
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar =
                 [{
                         name = "e"
                         args = []
                         body = PSeq ([],None,None)
                         _public = true
                         metaArgs = []
                 }; 
                 {       name = "e"
                         args = []
                         body = PSeq ([{ omit = false
                                         rule = PRef (("s", (4, 5)),None)
                                         binding = None
                                         checker = None}],None,None)
                         _public = true
                         metaArgs = []
                 };
                 {
                         name = "s"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken ("STRING", (23, 29))
                                        binding = None
                                        checker = None}],None,None)
                         _public = false
                         metaArgs = []
                 }]
             foot = None
             options = Map.empty}
        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
        result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)
