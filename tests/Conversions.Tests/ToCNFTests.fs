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
    let path f = System.IO.Path.Combine(basePath, f)
      
    let applyConversion (conversion:Conversion) loadIL = 
        {
            loadIL
                with grammar = conversion.ConvertGrammar (loadIL.grammar, [||])                               
        }

    [<Test>]
    member test.``Simple ToCNF mssql``() =
        let msqlrootPath = @"../src/YC.GrammarZOO/SQL/TSQL"
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(msqlrootPath, "mssql.yrd"))
        Namer.initNamer loadIL.grammar
        let result = loadIL |> applyConversion expandTopLevelAlt |> 
                            applyConversion expandEbnf |> applyConversion expandMeta |> applyConversion conversionCNF
        //result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(true)

    [<Test>]
    member test.``DeleteLongRule test`` () =
            let rules = 
                (verySimpleRules "e"
                    [{dummyRule with rule = PToken (Source.t "A")}
                     {dummyRule with rule = PRef (Source.t "yard_e_2", None)}]) 
               @(verySimpleNotStartRules "yard_e_1"
                    [{dummyRule with rule = PRef (Source.t "a", None)}
                     {dummyRule with rule = PToken (Source.t "B")}])
               @(verySimpleNotStartRules "yard_e_2"
                    [{dummyRule with rule = PRef (Source.t "yard_e_1", None)}
                     {dummyRule with rule = PRef (Source.t "e", None)}])
            runTest (path "longrule_0.yrd") conversionLongRules rules

    [<Test>]
    member test.``Delete Eps rule test 1`` () =
        let rules =
            verySimpleRules "e" [{dummyRule with rule = PRef (Source.t "s", None)}]
          @ verySimpleNotStartRules "s" [{dummyRule with rule = PToken (Source.t "STRING")}]
        
        runTest (path "eps_0.yrd") conversionEps rules

    [<Test>]
    member test.``Delete Eps rule test 2`` () =
        let expected = 
            verySimpleRules "x" [ {dummyRule with rule = PToken (Source.t "ID")}]
          @ verySimpleRules "x" [ {dummyRule with rule = PRef (Source.t "s",None)}
                                  {dummyRule with rule = PRef (Source.t "new_ID",None)}]                   
          @ verySimpleRules "x" [ {dummyRule with rule = PRef (Source.t "y",None)}
                                  {dummyRule with rule = PRef (Source.t "yard_s_1",None)}]
          @ verySimpleNotStartRules "s" [ {dummyRule with rule = PToken (Source.t "ID")}]
          @ verySimpleNotStartRules "s" [ {dummyRule with rule = PRef (Source.t "s",None)}
                                          {dummyRule with rule = PRef (Source.t "new_ID",None)}]                   
          @ verySimpleNotStartRules "s" [ {dummyRule with rule = PRef (Source.t "y",None)}
                                          {dummyRule with rule = PRef (Source.t "yard_s_1",None)}]
          @ verySimpleNotStartRules "y" [ {dummyRule with rule = PToken (Source.t "ID")}]
          @ verySimpleNotStartRules "y" [ {dummyRule with rule = PRef (Source.t "s",None)}
                                          {dummyRule with rule = PRef (Source.t "new_ID",None)}]                   
          @ verySimpleNotStartRules "y" [ {dummyRule with rule = PRef (Source.t "y",None)}
                                          {dummyRule with rule = PRef (Source.t "yard_s_1",None)}]
          @ verySimpleNotStartRules "yard_s_1" [ {dummyRule with rule = PToken (Source.t "ID")}]
          @ verySimpleNotStartRules "yard_s_1" [ {dummyRule with rule = PRef (Source.t "s",None)}
                                                 {dummyRule with rule = PRef (Source.t "new_ID",None)}]                   
          @ verySimpleNotStartRules "new_ID" [ {dummyRule with rule = PToken (Source.t "ID")}]

        runTest (path "eps_1.yrd") conversionCNF expected

    [<Test>]
    member test.``Delete chain-rule`` () =
        let rules =
            verySimpleRules "e" [{dummyRule with rule = PToken (Source.t "STRING")}]
          @ verySimpleNotStartRules "a" [{dummyRule with rule = PToken (Source.t "STRING")}]
          @ verySimpleNotStartRules "s" [{dummyRule with rule = PToken (Source.t "STRING")}]

        runTest (path "chain_0.yrd") conversionChain rules
    
    [<Test>]
    member test.``TermRenamer test`` () =
        let rules = 
                verySimpleRules "x"
                    [{dummyRule with rule = PRef(Source.t "new_A", None)}
                     {dummyRule with rule =  PRef (Source.t "new_B", None)}]
               @ verySimpleNotStartRules "z"
                    [{dummyRule with rule =  PRef (Source.t "z", None)}
                     {dummyRule with rule =  PRef (Source.t "x", None)}]
               @ verySimpleNotStartRules "new_A" [{dummyRule with rule =  PToken (Source.t "A")}]
               @ verySimpleNotStartRules "new_B" [{dummyRule with rule =  PToken (Source.t "B")}]
        
        runTest (path "renameTerm_0.yrd") conversionRenamer rules

    [<Test>]
    member test.``ToCNF test`` () =
        let rules = 
            verySimpleRules "e"
                [{dummyRule with rule = PRef (Source.t "new_A", None)}
                 {dummyRule with rule = PRef (Source.t "e", None)}]
          @ verySimpleNotStartRules "new_A"
                [{dummyRule with rule = PToken (Source.t "A")}]
          
        runTest (path "cnf_0.yrd") conversionCNF rules

    [<Test>]
    member test.``ToCNF Larger Grammar test 1`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"grammar.yrd"))
        Namer.initNamer loadIL.grammar
        let result = loadIL |> applyConversion expandTopLevelAlt |> applyConversion conversionCNF
        let l = result.grammar.Head.rules.Length
        printfn "%s" "************************"
      //  result |> treeDump.Generate |> string |> printfn "%s"
        Assert.AreEqual(15, l)
       
    [<Test>]
    member test.``ToCNF Larger Grammar test 2`` () =
        let rules = 
                verySimpleRules "s"
                    [{dummyRule with rule = PRef(Source.t "new_A", None)}
                     {dummyRule with rule = PRef (Source.t "yard_s_2", None)}]
              @ verySimpleNotStartRules "s"
                    [{dummyRule with rule = PRef (Source.t "new_A", None)}
                     {dummyRule with rule = PRef (Source.t "z", None)}]
              @ verySimpleNotStartRules "x"
                    [{dummyRule with rule = PRef (Source.t "new_A", None)}
                     {dummyRule with rule = PRef (Source.t "y", None)}]
              @ verySimpleNotStartRules "x"
                    [{dummyRule with rule = PRef (Source.t "new_B", None)}
                     {dummyRule with rule = PRef (Source.t "y", None)}]
              @ verySimpleNotStartRules "y"
                    [{dummyRule with rule = PRef (Source.t "new_A", None)}
                     {dummyRule with rule = PRef (Source.t "y", None)}]
              @ verySimpleNotStartRules "y"
                    [{dummyRule with rule = PRef (Source.t "new_B", None)}
                     {dummyRule with rule = PRef (Source.t "y", None)}]
              @ verySimpleNotStartRules "y"
                    [{dummyRule with rule = PToken (Source.t "CC")}]
              @ verySimpleNotStartRules "z"
                    [{dummyRule with rule = PRef (Source.t "z", None)}
                     {dummyRule with rule = PRef (Source.t "x", None)}]
              @ verySimpleNotStartRules "yard_s_1"
                    [{dummyRule with rule = PToken (Source.t "B")}]
              @ verySimpleNotStartRules "yard_s_1"
                    [{dummyRule with rule = PRef (Source.t "x", None)}
                     {dummyRule with rule = PRef (Source.t "new_B", None)}]
              @ verySimpleNotStartRules "yard_s_2"
                    [{dummyRule with rule = PToken (Source.t "B")}]
              @ verySimpleNotStartRules "yard_s_2"
                    [{dummyRule with rule = PRef (Source.t "x", None)}
                     {dummyRule with rule = PRef (Source.t "new_B", None)}]
              @ verySimpleNotStartRules "yard_s_2"
                    [{dummyRule with rule = PRef (Source.t "yard_s_1", None)}
                     {dummyRule with rule = PRef (Source.t "x", None)}]
              @ verySimpleNotStartRules "new_A"
                    [{dummyRule with rule = PToken (Source.t "A")}]
              @ verySimpleNotStartRules "new_B"
                    [{dummyRule with rule = PToken (Source.t "B")}]

        runTest (path "grammar.yrd") conversionCNF rules
        
        

       