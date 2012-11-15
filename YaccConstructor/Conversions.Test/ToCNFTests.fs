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
open Conversions.TransformAux
open NUnit.Framework
open ConversionsTests

[<TestFixture>]
type ``CNF tests`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "ToCNF")
    let fe = getFrontend("YardFrontend")
    let conversionCNF = "ToCNF"
    let conversionChain = "DeleteChainRule"
    let conversionEps = "DeleteEpsRule"

    [<Test>]
    member test.``ToCNF test`` () =
        Namer.resetRuleEnumerator()
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"cnf_0.yrd"))
        let result = ConversionsManager.ApplyConversion conversionCNF loadIL
        let expected = 
            {
                info = {fileName = ""}
                head = None
                grammar = [{
                                name = Source.t("e")
                                args = []
                                body = 
                                    PSeq (
                                            [{
                                                omit = false
                                                rule = PRef (Source.t "new_A", None)
                                                binding = None
                                                checker = None
                                            }; 
                                            {
                                                omit = false
                                                rule = PRef (Source.t "e", None)
                                                binding = None
                                                checker = None
                                            }],
                                            None,
                                            None)
                                _public = true
                                metaArgs = []
                          };
                          {
                                name = Source.t("new_A")
                                args = []
                                body = 
                                    PSeq (
                                            [{
                                                omit = false
                                                rule = PToken (Source.t "A")
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
        let result = ConversionsManager.ApplyConversion conversionChain loadIL
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar =
                [{
                    name = Source.t("e")
                    args = []
                    body = PSeq ([{
                                    omit = false
                                    rule = PToken (Source.t "STRING")
                                    binding = None
                                    checker = None
                                }],None,None)
                    _public = true
                    metaArgs = []
                }
                ;{
                    name = Source.t("s")
                    args = []
                    body = PSeq ([{
                                    omit = false
                                    rule = PToken (Source.t "STRING")
                                    binding = None
                                    checker = None
                                }],None,None)
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
        let result = ConversionsManager.ApplyConversion conversionEps loadIL
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar =
                 [{
                         name = Source.t("e")
                         args = []
                         body = PSeq ([],None,None)
                         _public = true
                         metaArgs = []
                 }; 
                 {       name = Source.t("e")
                         args = []
                         body = PSeq ([{ omit = false
                                         rule = PRef (Source.t "s", None)
                                         binding = None
                                         checker = None}],None,None)
                         _public = true
                         metaArgs = []
                 };
                 {
                         name = Source.t("s")
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken (Source.t "STRING")
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
