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
open Convertions.TransformAux
open NUnit.Framework

exception FEError of string
let ConvertionsManager = ConvertionsManager.ConvertionsManager()
let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager()

let convertionTestPath = @"../../../../Tests/Convertions/"
let GeneratorsManager = Yard.Core.GeneratorsManager.GeneratorsManager()

let getFrontend name =
        match FrontendsManager.Component name with
        | Some fe -> fe
        | None -> failwith (name + " is not found.")

let getBE name =
    match GeneratorsManager.Component name with
    | Some be -> be
    | None -> failwith (name + " is not found.")

let treeDump = getBE "TreeDump"

let dummyRule = {omit=false; binding=None; checker=None; rule=PToken("DUMMY",(0,0))}

[<TestFixture>]
type ``Convertions tests`` () =
    //[<Test>]
    member test.``ExpandBrackets tests. Lexer seq test`` () =
        Namer.resetRuleEnumerator()        
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
                            ], None, None)
                            |> PSeq
                            , PToken("OUTER",(0,0)))
                            |> PAlt
                    }
                ]
                options = Map.empty
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
                            ],None, None)
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
            options = Map.empty
        }
        Assert.AreEqual(ilTreeConverted, correctConverted)
    
    [<Test>]
    member test.``ExpandBrackets. Sequence as sequence element test.``()=
        let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager() 
        Namer.resetRuleEnumerator()
        let frontend =
            match FrontendsManager.Component "YardFrontend" with
               | Some fron -> fron
               | None -> failwith "YardFrontend is not found."         
        let ilTree = 
            System.IO.Path.Combine(convertionTestPath,"expandbrackets_1.yrd")
            |> frontend.ParseGrammar
        let ilTreeConverted = 
            ilTree 
            |> ConvertionsManager.ApplyConvertion "ExpandMeta"   
            |> ConvertionsManager.ApplyConvertion "ExpandEbnf"
            |> ConvertionsManager.ApplyConvertion "ExpandBrackets"   
        let hasNotInnerSeq = 
            ilTreeConverted.grammar 
            |> List.forall 
                (fun rule ->
                    let rec eachProd = function
                        | PAlt(a,b) -> eachProd a && eachProd b
                        | PSeq(elements, _, _) -> elements |> List.forall (fun elem -> match elem.rule with PSeq _ -> false | _ -> true)
                        | _ -> true
                    eachProd rule.body
                )
            
#if DEBUG
        let generator = 
           match GeneratorsManager.Component  "TreeDump" with
           | Some gen -> gen
           | None -> failwith "TreeDump is not found."
        printfn "%A\n" (generator.Generate ilTreeConverted)
#endif
        Assert.True(hasNotInnerSeq)
   
[<TestFixture>]
type ``Expand rop level alters`` () =
    let basePath = System.IO.Path.Combine(convertionTestPath, "ExpandTopLevelAlters")
    let fe = getFrontend("YardFrontend")
    let conversion = "ExpandTopLevelAlt"

    [<Test>]
    member test.``No alter`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"noAlters.yrd"))
        let result = ConvertionsManager.ApplyConvertion conversion loadIL
        let expected = 
            {
                info = {fileName = ""}
                head = None
                grammar =
                     [{
                            name = "s"
                            args = []
                            body = PSeq([{dummyRule with rule = PRef (("d", (0, 0)),None)}],None, None)
                            _public = true
                            metaArgs = []
                         };
                         {
                            name = "d"
                            args = []
                            body = PSeq([{dummyRule with rule = PToken ("NUM", (0, 0))}],None, None)
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
    member test.``One alter`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"oneAlter.yrd"))
        let result = ConvertionsManager.ApplyConvertion conversion loadIL
        let expected = 
            {
                info = {fileName = ""}
                head = None
                grammar =
                     [{
                            name = "s"
                            args = []
                            body = PSeq([{dummyRule with rule = PRef (("c", (0, 0)),None)}],None, None)
                            _public = true
                            metaArgs = []
                         };
                         {
                            name = "s"
                            args = []
                            body = PSeq([{dummyRule with rule = PRef (("d", (0, 0)),None)}],None, None)
                            _public = true
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
    member test.``Multi alters`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"multiAlters.yrd"))
        let result = ConvertionsManager.ApplyConvertion conversion loadIL
        let expected = 
            {
                info = {fileName = ""}
                head = None
                grammar =
                     [{
                            name = "s"
                            args = []
                            body = PSeq([{dummyRule with rule = PRef (("x", (0, 0)),None)}],None, None)
                            _public = true
                            metaArgs = []
                         };
                         {
                            name = "s"
                            args = []
                            body = PSeq([{dummyRule with rule = PRef (("y", (0, 0)),None)}],None, None)
                            _public = true
                            metaArgs = []
                         };
                         {
                            name = "s"
                            args = []
                            body = PSeq([{dummyRule with rule = PRef (("z", (0, 0)),None)}],None, None)
                            _public = true
                            metaArgs = []
                         };
                         {
                            name = "s"
                            args = []
                            body = PSeq([{dummyRule with rule = PRef (("m", (0, 0)),None)}],None, None)                        
                            _public = true
                            metaArgs = []
                         }]
                foot = None
                options = Map.empty
            }

        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
        result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)