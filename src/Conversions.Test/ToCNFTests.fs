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
open Yard.Core.Helpers
open Conversions.TransformAux
open NUnit.Framework
open ConversionsTests
open Yard.Core.Helpers

[<TestFixture>]
type ``CNF tests`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "ToCNF")
    let fe = getFrontend("YardFrontend")
    let conversionExpandTLAlt = new Conversions.ExpandTopLevelAlt.ExpandTopLevelAlt()
    let conversionExpandEBNF = new Conversions.ExpandEbnfStrict.ExpandEbnf()
    let conversionLongRules = new Conversions.ToCNF.SplitLongRule()
    let conversionEps = new Conversions.ToCNF.DeleteEpsRule()
    let conversionChain = new Conversions.ToCNF.DeleteChainRule()
    let conversionRenamer = new Conversions.ToCNF.RenameTerm()
    let conversionCNF = new Conversions.ToCNF.ToCNF()
    let noMeta = new Conversions.ExpandMeta.ExpandMeta()

    let applyConversion (conversion:Conversion) loadIL = 
        {
            loadIL
                with grammar = conversion.ConvertGrammar (loadIL.grammar, [||])                               
        }

    [<Test>]
    member test.``Simple ToCNF mssql``() =
        let msqlrootPath = @"../../../src/YC.GrammarZOO/SQL/TSQL"
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(msqlrootPath, "mssql.yrd"))
        Namer.initNamer loadIL.grammar
        let result = loadIL |> applyConversion conversionExpandTLAlt |> 
                            applyConversion conversionExpandEBNF |> applyConversion noMeta |> applyConversion conversionCNF
        //result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(true)

    [<Test>]
    member test.``DeleteLongRule test`` () =
            let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"longrule_0.yrd"))
            Namer.initNamer loadIL.grammar
            let result = loadIL |> applyConversion conversionExpandTLAlt |> applyConversion conversionLongRules
            let rules = 
                (verySimpleRules "e"
                    [{
                        omit = false
                        rule = PToken (Source.t "A")
                        binding = None
                        checker = None
                    }; {
                        omit = false
                        rule = PRef (Source.t "yard_e_2", None)
                        binding = None
                        checker = None
                    }]
                ) @ (
                 verySimpleNotStartRules "yard_e_1"
                    [{
                        omit = false
                        rule = PRef (Source.t "a", None)
                        binding = None
                        checker = None
                    };{
                        omit = false
                        rule = PToken (Source.t "B")
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "yard_e_2"
                    [{
                        omit = false
                        rule = PRef (Source.t "yard_e_1", None)
                        binding = None
                        checker = None
                    };{
                        omit = false
                        rule = PRef (Source.t "e", None)
                        binding = None
                        checker = None
                    }])
            let expected = defaultDefinition rules
            expected |> treeDump.Generate |> string |> printfn "%s"
            printfn "%s" "************************"
           // result |> treeDump.Generate |> string |> printfn "%s"
            Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)

    [<Test>]
    member test.``Delete Eps rule test 1`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"eps_0.yrd"))
        Namer.initNamer loadIL.grammar
        let result = applyConversion conversionEps loadIL
        let rules =
            (verySimpleRules "e"
                [{
                    omit = false
                    rule = PRef (Source.t "s", None)
                    binding = None
                    checker = None
                }]
            ) @ (
                verySimpleNotStartRules "s"
                    [{
                        omit = false
                        rule = PToken (Source.t "STRING")
                        binding = None
                        checker = None
                    }]
            )
        
        let expected = defaultDefinition rules
        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
       // result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)

    [<Test>]
    member test.``Delete Eps rule test 2`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"eps_1.yrd"))
        let result = loadIL |> applyConversion conversionExpandTLAlt |> applyConversion conversionCNF
        let expected = 
            defaultDefinition
                   [{name = Source.t "x";
                    args = [];
                    body = PSeq ([{omit = false;
                                   rule = PToken (Source.t "ID");
                                   binding = None;
                                   checker = None;}],None,None);
                    isStart = true;
                    isPublic = false;
                    metaArgs = [];};
                   {name = Source.t "x";
                    args = [];
                    body = PSeq ([{omit = false;
                                   rule = PRef (Source.t "s",None);
                                   binding = None;
                                   checker = None;}; {omit = false;
                                                      rule = PRef (Source.t "new_ID",None);
                                                      binding = None;
                                                      checker = None;}],None,None);
                    isStart = true;
                    isPublic = false;
                    metaArgs = [];};
                   {name = Source.t "x";
                    args = [];
                    body = PSeq ([{omit = false;
                                   rule = PRef (Source.t "y",None);
                                   binding = None;
                                   checker = None;}; {omit = false;
                                                      rule = PRef (Source.t "yard_s_1",None);
                                                      binding = None;
                                                      checker = None;}],None,None);
                    isStart = true;
                    isPublic = false;
                    metaArgs = [];}; {name = Source.t "s";
                                      args = [];
                                      body = PSeq ([{omit = false;
                                                     rule = PToken (Source.t "ID");
                                                     binding = None;
                                                     checker = None;}],None,None);
                                      isStart = false;
                                      isPublic = false;
                                      metaArgs = [];};
                   {name = Source.t "s";
                    args = [];
                    body = PSeq ([{omit = false;
                                   rule = PRef (Source.t "s",None);
                                   binding = None;
                                   checker = None;}; {omit = false;
                                                      rule = PRef (Source.t "new_ID",None);
                                                      binding = None;
                                                      checker = None;}],None,None);
                    isStart = false;
                    isPublic = false;
                    metaArgs = [];};
                   {name = Source.t "s";
                    args = [];
                    body = PSeq ([{omit = false;
                                   rule = PRef (Source.t "y",None);
                                   binding = None;
                                   checker = None;}; {omit = false;
                                                      rule = PRef (Source.t "yard_s_1",None);
                                                      binding = None;
                                                      checker = None;}],None,None);
                    isStart = false;
                    isPublic = false;
                    metaArgs = [];}; {name = Source.t "y";
                                      args = [];
                                      body = PSeq ([{omit = false;
                                                     rule = PToken (Source.t "ID");
                                                     binding = None;
                                                     checker = None;}],None,None);
                                      isStart = false;
                                      isPublic = false;
                                      metaArgs = [];};
                   {name = Source.t "y";
                    args = [];
                    body = PSeq ([{omit = false;
                                   rule = PRef (Source.t "s",None);
                                   binding = None;
                                   checker = None;}; {omit = false;
                                                      rule = PRef (Source.t "new_ID",None);
                                                      binding = None;
                                                      checker = None;}],None,None);
                    isStart = false;
                    isPublic = false;
                    metaArgs = [];};
                   {name = Source.t "y";
                    args = [];
                    body = PSeq ([{omit = false;
                                   rule = PRef (Source.t "y",None);
                                   binding = None;
                                   checker = None;}; {omit = false;
                                                      rule = PRef (Source.t "yard_s_1",None);
                                                      binding = None;
                                                      checker = None;}],None,None);
                    isStart = false;
                    isPublic = false;
                    metaArgs = [];}; {name = Source.t "yard_s_1";
                                      args = [];
                                      body = PSeq ([{omit = false;
                                                     rule = PToken (Source.t "ID");
                                                     binding = None;
                                                     checker = None;}],None,None);
                                      isStart = false;
                                      isPublic = false;
                                      metaArgs = [];};
                   {name = Source.t "yard_s_1";
                    args = [];
                    body = PSeq ([{omit = false;
                                   rule = PRef (Source.t "s",None);
                                   binding = None;
                                   checker = None;}; {omit = false;
                                                      rule = PRef (Source.t "new_ID",None);
                                                      binding = None;
                                                      checker = None;}],None,None);
                    isStart = false;
                    isPublic = false;
                    metaArgs = [];}; {name = Source.t "new_ID";
                                      args = [];
                                      body = PSeq ([{omit = false;
                                                     rule = PToken (Source.t "ID");
                                                     binding = None;
                                                     checker = None;}],None,None);
                                      isStart = false;
                                      isPublic = false;
                                      metaArgs = []};]
        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
       // result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)

    [<Test>]
    member test.``Delete chain-rule`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"chain_0.yrd"))
        Namer.initNamer loadIL.grammar
        let result = loadIL |> applyConversion conversionExpandTLAlt |> applyConversion conversionChain 
        let rules =
            (verySimpleRules "e"
                [{
                    omit = false
                    rule = PToken (Source.t "STRING")
                    binding = None
                    checker = None
                }]
            ) @ (
                verySimpleNotStartRules "a"
                    [{
                        omit = false
                        rule = PToken (Source.t "STRING")
                        binding = None
                        checker = None
                    }]
            )@(
                verySimpleNotStartRules "s"
                    [{
                        omit = false
                        rule = PToken (Source.t "STRING")
                        binding = None
                        checker = None
                    }]
            )

        let expected = defaultDefinition rules
        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
      //  result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)
    
    [<Test>]
    member test.``TermRenamer test`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"renameTerm_0.yrd"))
        Namer.initNamer loadIL.grammar
        let result = loadIL |> applyConversion conversionExpandTLAlt |> applyConversion conversionRenamer
        let rules = 
                (verySimpleRules "x"
                    [{
                        omit = false
                        rule = PRef(Source.t "new_A", None)
                        binding = None
                        checker = None
                    }; {
                        omit = false
                        rule = PRef (Source.t "new_B", None)
                        binding = None
                        checker = None
                    }]
                ) @ (
                 verySimpleNotStartRules "z"
                    [{
                        omit = false
                        rule = PRef (Source.t "z", None)
                        binding = None
                        checker = None
                    };{
                        omit = false
                        rule = PRef (Source.t "x", None)
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "new_A"
                    [{
                        omit = false
                        rule = PToken (Source.t "A")
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "new_B"
                    [{
                        omit = false
                        rule = PToken (Source.t "B")
                        binding = None
                        checker = None
                    }])
        let expected = defaultDefinition rules
        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
      //  result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)

    [<Test>]
    member test.``ToCNF test`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"cnf_0.yrd"))
        Namer.initNamer loadIL.grammar
        let result = loadIL |> applyConversion conversionExpandTLAlt |> applyConversion conversionCNF
        let rules = 
            (verySimpleRules "e"
                [{
                    omit = false
                    rule = PRef (Source.t "new_A", None)
                    binding = None
                    checker = None
                }; {
                    omit = false
                    rule = PRef (Source.t "e", None)
                    binding = None
                    checker = None
                }]
            ) @ (
             verySimpleNotStartRules "new_A"
                [{
                    omit = false
                    rule = PToken (Source.t "A")
                    binding = None
                    checker = None
                }]
            )
        let expected = defaultDefinition rules
        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
      //  result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)

    [<Test>]
    member test.``ToCNF Larger Grammar test 1`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"grammar.yrd"))
        Namer.initNamer loadIL.grammar
        let result = loadIL |> applyConversion conversionExpandTLAlt |> applyConversion conversionCNF
        let l = result.grammar.Head.rules.Length
        printfn "%s" "************************"
      //  result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue((l = 15))
       

    [<Test>]
    member test.``ToCNF Larger Grammar test 2`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"grammar.yrd"))
        Namer.initNamer loadIL.grammar
        let result = loadIL |> applyConversion conversionExpandTLAlt |> applyConversion conversionCNF
        let rules = 
                (verySimpleRules "s"
                    [{
                        omit = false
                        rule = PRef(Source.t "new_A", None)
                        binding = None
                        checker = None
                    }; {
                        omit = false
                        rule = PRef (Source.t "yard_s_2", None)
                        binding = None
                        checker = None
                    }]
                ) @ (
                 verySimpleRules "s"
                    [{
                        omit = false
                        rule = PRef (Source.t "new_A", None)
                        binding = None
                        checker = None
                    };{
                        omit = false
                        rule = PRef (Source.t "z", None)
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "x"
                    [{
                        omit = false
                        rule = PRef (Source.t "new_A", None)
                        binding = None
                        checker = None
                    };{
                        omit = false
                        rule = PRef (Source.t "y", None)
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "x"
                    [{
                        omit = false
                        rule = PRef (Source.t "new_B", None)
                        binding = None
                        checker = None
                    };{
                        omit = false
                        rule = PRef (Source.t "y", None)
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "y"
                    [{
                        omit = false
                        rule = PRef (Source.t "new_A", None)
                        binding = None
                        checker = None
                    };{
                        omit = false
                        rule = PRef (Source.t "y", None)
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "y"
                    [{
                        omit = false
                        rule = PRef (Source.t "new_B", None)
                        binding = None
                        checker = None
                    };{
                        omit = false
                        rule = PRef (Source.t "y", None)
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "y"
                    [{
                        omit = false
                        rule = PToken (Source.t "CC")
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "z"
                    [{
                        omit = false
                        rule = PRef (Source.t "z", None)
                        binding = None
                        checker = None
                    };{
                        omit = false
                        rule = PRef (Source.t "x", None)
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "yard_s_1"
                    [{
                        omit = false
                        rule = PToken (Source.t "B")
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "yard_s_1"
                    [{
                        omit = false
                        rule = PRef (Source.t "x", None)
                        binding = None
                        checker = None
                    };{
                        omit = false
                        rule = PRef (Source.t "new_B", None)
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "yard_s_2"
                    [{
                        omit = false
                        rule = PToken (Source.t "B")
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "yard_s_2"
                    [{
                        omit = false
                        rule = PRef (Source.t "x", None)
                        binding = None
                        checker = None
                    };{
                        omit = false
                        rule = PRef (Source.t "new_B", None)
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "yard_s_2"
                    [{
                        omit = false
                        rule = PRef (Source.t "yard_s_1", None)
                        binding = None
                        checker = None
                    };{
                        omit = false
                        rule = PRef (Source.t "x", None)
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "new_A"
                    [{
                        omit = false
                        rule = PToken (Source.t "A")
                        binding = None
                        checker = None
                    }])@(
                 verySimpleNotStartRules "new_B"
                    [{
                        omit = false
                        rule = PToken (Source.t "B")
                        binding = None
                        checker = None
                    }])
        let expected = defaultDefinition rules
        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
       // result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)
        
        

       