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

type TreeDump() = 
    interface IGenerator with
        member this.Name = "TreeDump"
        member this.Generate t = (sprintf "%A" t) :> obj
        member this.AcceptableProductionTypes = 
            List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>)
            |> List.map (fun unionCase -> unionCase.Name)
    end


[<TestFixture>]
type ``Convertions tests`` () =
    [<Test>]
    member test.``ExpandBrackets tests. Lexer seq test`` () =
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
        let ilTreeConverted = ConvertionsManager.ApplyConvertion ilTree ("ExpandBrackets")
        printfn "%A" ilTreeConverted
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
        let frontend = FrontendsManager.Frontend "YardFrontend"
        //let generator = GeneratorsManager.Generator ""
        let ilTree = 
            System.IO.Path.Combine(convertionTestPath,"PToken_to_PRef_1.yrd")
            |> frontend.ParseGrammar 
        let ilTreeConverted = ConvertionsManager.ApplyConvertion ilTree ("ExpandMeta")
        let expectedResult:t<Source.t,Source.t> =
            {info = {fileName = "..\..\..\..\Tests\Convertions\ptoken_to_pref_1.yrd";};
             head = None;
             grammar =
              [{name = "yard_metar_1";
                args = [];
                body = PSeq ([{omit = false;
                               rule = PToken ("NUMBER", (16, 20));
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
        Assert.AreEqual(expectedResult, ilTreeConverted)

    [<Test>]
    member test.``Expand Meta. PToken to PRef replacement test 2.`` () =
        GeneratorsManager.Register (new TreeDump())
        let frontend = FrontendsManager.Frontend "YardFrontend"
        let generator = GeneratorsManager.Generator "TreeDump"
        let ilTree = 
            System.IO.Path.Combine(convertionTestPath,"PToken_to_PRef_2.yrd")
            |> frontend.ParseGrammar 
        let ilTreeConverted = ConvertionsManager.ApplyConvertion ilTree ("ExpandMeta")
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
                                                 rule = PRef (("mrArg", (36, 41)),None);
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

        printfn "%A\n" (generator.Generate ilTreeConverted)
        Assert.AreEqual( expectedResult, ilTreeConverted)

