//  ConvertionsTests.fs contains unuit test for Convertions
//
//  Copyright 2009-2011 Konstantin Ulitin <ulitin.k@gmail.com>
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


module ConvertionsTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open NUnit.Framework


let convertionTestPath = @"../../../../Tests/Convertions/"

[<TestFixture>]
type ``Convertions tests`` () =
    [<Test>]
    member test.``ExpandBrackets tests. Lexer seq test`` () =
        Namer.resetRuleEnumerator()
        let dummyRule = {omit=false; binding=None; checker=None; rule=PToken("DUMMY",(0,0))}
        let ilTree = 
            {
                info = { fileName = "" } 
                head = None 
                foot = None 
                grammar = 
                    [{ 
                        name = "s"
                        args = []
                        metaArgs = []
                        _public = true
                        body =
                          (([
                                {dummyRule with rule=PToken("NUMBER",(0,0))};
                                {dummyRule with rule=PAlt(PToken("ALT1",(0,0)),PToken("ALT2",(0,0)))}
                                {dummyRule with rule=PToken("CHUMBER",(0,0))};
                            ], None)
                            |> PSeq
                            , PToken("OUTER",(0,0)))
                            |> PAlt
                    }
                ]
            }
        let ilTreeConverted = ConvertionsManager.ApplyConvertion "ExpandBrackets" ilTree
#if DEBUG
        printfn "%A" ilTreeConverted
#endif
        let correctConverted:t<Source.t,Source.t> = {
            info = {fileName = ""}
            head = None;
            grammar = 
                [{
                    name = "s"
                    args = []
                    body =
                        PAlt(
                            PSeq([
                                    {dummyRule with rule = PToken ("NUMBER", (0, 0))};
                                    {dummyRule with rule = PRef (("yard_exp_brackets_1", (0, 0)),None)}; 
                                    {dummyRule with rule = PToken ("CHUMBER", (0, 0))}
                            ],None)
                            , PToken ("OUTER", (0, 0)))
                    _public = true
                    metaArgs = []
                 };
                 {
                    name = "yard_exp_brackets_1"
                    args = []
                    body = PAlt (PToken ("ALT1", (0, 0)),PToken ("ALT2", (0, 0)))
                    _public = false
                    metaArgs = []
                 }]
            foot = None
        }
        Assert.AreEqual(ilTreeConverted, correctConverted)

    [<Test>]
    member test.``Expand Meta. PToken to PRef replacement test 1.`` () =
        Namer.resetRuleEnumerator()
        let frontend = FrontendsManager.Frontend "YardFrontend"        
        let ilTree:t<Source.t,Source.t> = 
            System.IO.Path.Combine(convertionTestPath,"PToken_to_PRef_1.yrd")
            |> frontend.ParseGrammar 
        let ilTreeConverted:t<Source.t,Source.t> = ConvertionsManager.ApplyConvertion "ExpandMeta" ilTree
        let expectedResult:t<Source.t,Source.t> =
            {info = {fileName = "../../../../Tests/Convertions/PToken_to_PRef_1.yrd";};
             head = None;
             grammar =
              [{name = "yard_metar_1";
                args = [];
                body = PSeq ([{omit = false;
                               rule = PToken ("NUMBER", (36, 42));
                               binding = None;
                               checker = None;}],None);
                _public = false;
                metaArgs = [];};
               {name = "s";
                args = [];
                body = PSeq ([{omit = false;
                               rule = PRef (("yard_metar_1", (29, 34)),None);
                               binding = None;
                               checker = None;}],None);
                _public = true;
                metaArgs = [];}];
             foot = None;}

#if DEBUG
        let generator = GeneratorsManager.Generator "TreeDump"
        printfn "%A\n" (generator.Generate ilTreeConverted)
#endif
        Assert.AreEqual(expectedResult, ilTreeConverted)

    [<Test>]
    member test.``Expand Meta. PToken to PRef replacement test 2.`` () =
        Namer.resetRuleEnumerator()        
        let frontend = FrontendsManager.Frontend "YardFrontend"        
        let ilTree = 
            System.IO.Path.Combine(convertionTestPath,"PToken_to_PRef_2.yrd")
            |> frontend.ParseGrammar 
        let ilTreeConverted = ConvertionsManager.ApplyConvertion "ExpandMeta" ilTree
        let expectedResult:t<Source.t,Source.t> =
            {info = {fileName = "../../../../Tests/Convertions/PToken_to_PRef_2.yrd";};
             head = None;
             grammar =
              [{name = "mrArg";
                args = [];
                body = PSeq ([{omit = false;
                               rule = PToken ("NUMBER", (8, 14));
                               binding = None;
                               checker = None;}],None);
                _public = false;
                metaArgs = [];}; {name = "yard_metar_1";
                                  args = [];
                                  body = PSeq ([{omit = false;
                                                 rule = PRef (("mrArg", (58, 63)),None);
                                                 binding = None;
                                                 checker = None;}],None);
                                  _public = false;
                                  metaArgs = [];};
               {name = "s";
                args = [];
                body = PSeq ([{omit = false;
                               rule = PRef (("yard_metar_1", (51, 56)),None);
                               binding = None;
                               checker = None;}],None);
                _public = true;
                metaArgs = [];}];
             foot = None;}


#if DEBUG
        let generator = GeneratorsManager.Generator "TreeDump"
        printfn "%A\n" (generator.Generate ilTreeConverted)
#endif
        Assert.AreEqual( expectedResult, ilTreeConverted)

    [<Test>]
    member test.``Expand Meta. Duplicate rules generation test.``()=
        Namer.resetRuleEnumerator()
        let frontend = FrontendsManager.Frontend "YardFrontend"        
        let ilTree = 
            System.IO.Path.Combine(convertionTestPath,"ExpandMeta_DuplicateRules.yrd")
            |> frontend.ParseGrammar 
        let ilTreeConverted = ConvertionsManager.ApplyConvertion ("ExpandMeta") ilTree
        let expectedResult = 
            {info =
              {fileName = "../../../../Tests/Convertions/ExpandMeta_DuplicateRules.yrd";};
             head = None;
             grammar =
              [{name = "yard_yardOption_1";
                args = [];
                body = PSeq ([{omit = false;
                               rule = PToken ("NUM", (164, 167));
                               binding = Some ("yard_item", (28, 37));
                               checker = None;}],Some (" Some yard_item ", (46, 62)));
                _public = false;
                metaArgs = [];};
               {name = "yard_yardOption_2";
                args = [];
                body = PSeq ([{omit = false;
                               rule = PToken ("STRING", (182, 188));
                               binding = Some ("yard_item", (28, 37));
                               checker = None;}],Some (" Some yard_item ", (46, 62)));
                _public = false;
                metaArgs = [];};
               {name = "yard_yardOption_4";
                args = [];
                body = PSeq ([{omit = false;
                               rule = PToken ("SEMI", (194, 198));
                               binding = Some ("yard_item", (28, 37));
                               checker = None;}],Some (" Some yard_item ", (46, 62)));
                _public = false;
                metaArgs = [];};
               {name = "yard_o_3";
                args = [];
                body = PSeq ([{omit = false;
                               rule = PRef (("yard_yardOption_4", (103, 113)),None);
                               binding = Some ("yard_item", (91, 100));
                               checker = None;}],Some (" Some yard_item ", (123, 139)));
                _public = false;
                metaArgs = [];};
               {name = "s";
                args = [];
                body =
                 PSeq
                   ([{omit = false;
                      rule = PRef (("yard_yardOption_1", (152, 162)),None);
                      binding = None;
                      checker = None;};
                     {omit = false;
                      rule = PRef (("yard_yardOption_2", (170, 180)),None);
                      binding = None;
                      checker = None;}; {omit = false;
                                         rule = PRef (("yard_o_3", (191, 192)),None);
                                         binding = None;
                                         checker = None;}],None);
                _public = true;
                metaArgs = [];}];
             foot = None;}

#if DEBUG
        let generator = GeneratorsManager.Generator "TreeDump"
        printfn "%A\n" (generator.Generate ilTreeConverted)
#endif      
        Assert.AreEqual( expectedResult, ilTreeConverted)

    [<Test>]
    member test.``ExpandBrackets. Sequence as sequence element test.``()=
        Namer.resetRuleEnumerator()
        let frontend = FrontendsManager.Frontend "YardFrontend"        
        let ilTree = 
            System.IO.Path.Combine(convertionTestPath,"expandbrackets_1.yrd")
            |> frontend.ParseGrammar 
        let ilTreeConverted = 
            ilTree 
            |> ConvertionsManager.ApplyConvertion "ExpandMeta"   
            |> ConvertionsManager.ApplyConvertion "ExpandEbnfStrict"   
            |> ConvertionsManager.ApplyConvertion "ExpandBrackets"   
        let hasNotInnerSeq = 
            ilTreeConverted.grammar 
            |> List.forall 
                (fun rule ->
                    let rec eachProd = function
                        | PAlt(a,b) -> eachProd a && eachProd b
                        | PSeq(elements, _) -> elements |> List.forall (fun elem -> match elem.rule with PSeq _ -> false | _ -> true)
                        | _ -> true
                    eachProd rule.body
                )
            
#if DEBUG
        let generator = GeneratorsManager.Generator "TreeDump"
        printfn "%A\n" (generator.Generate ilTreeConverted)
#endif
        Assert.True(hasNotInnerSeq)  