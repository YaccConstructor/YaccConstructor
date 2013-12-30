//  ConversionsTests.fs contains unuit test for Conversions
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

module ConversionsTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Conversions.TransformAux
open NUnit.Framework


let dummyPos s = new Source.t(s)
let dummyToken s = PToken <| new Source.t(s)

exception FEError of string
let ConversionsManager = ConversionsManager.ConversionsManager()
let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager()

let conversionTestPath = @"../../../Tests/Conversions/"
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

let dummyRule : elem<Source.t,Source.t> = {omit=false; binding=None; checker=None; rule=PToken (Source.t "DUMMY")}

[<TestFixture>]
type ``Conversions tests`` () =
    //[<Test>]
    member test.``ExpandBrackets tests. Lexer seq test`` () =
        let rules =
            PAlt (
                PSeq(
                    [
                        {dummyRule with rule=dummyToken "NUMBER"};
                        {dummyRule with rule=PAlt(dummyToken "ALT1", dummyToken "ALT2")}
                        {dummyRule with rule=dummyToken "CHUMBER"};
                    ], None, None),
                dummyToken "OUTER")
            |> simpleRules "s"
        let ilTree = defaultDefinition rules
        Namer.initNamer ilTree.grammar
        let ilTreeConverted = ConversionsManager.ApplyConversion "ExpandBrackets" ilTree
#if DEBUG
        printfn "%A" ilTreeConverted
#endif
        let rules = 
            (simpleRules "s"
                <| PAlt(
                        PSeq([
                                {dummyRule with rule = dummyToken "NUMBER"};
                                {dummyRule with rule = PRef (dummyPos "yard_exp_brackets_1",None)}; 
                                {dummyRule with rule = dummyToken "CHUMBER"}
                        ],None, None)
                        , dummyToken "OUTER")
            ) @ (
                simpleNotStartRules "yard_exp_brackets_1"
                <| PAlt (dummyToken "ALT1", dummyToken "ALT2")
            )
        let correctConverted = defaultDefinition rules
        Assert.AreEqual(ilTreeConverted, correctConverted)
    
    [<Test>]
    member test.``ExpandBrackets. Sequence as sequence element test.``()=
        let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager() 
        let frontend =
            match FrontendsManager.Component "YardFrontend" with
               | Some fron -> fron
               | None -> failwith "YardFrontend is not found."         
        let ilTree = 
            System.IO.Path.Combine(conversionTestPath,"expandbrackets_1.yrd")
            |> frontend.ParseGrammar
        Namer.initNamer ilTree.grammar
        let ilTreeConverted = 
            ilTree 
            |> ConversionsManager.ApplyConversion "ExpandMeta"   
            |> ConversionsManager.ApplyConversion "ExpandEbnf"
            |> ConversionsManager.ApplyConversion "ExpandInnerAlt"
            |> ConversionsManager.ApplyConversion "ExpandBrackets"
        let hasNotInnerSeq = 
            ilTreeConverted.grammar
            |> List.forall (fun m ->
                m.rules |> List.forall
                    (fun rule ->
                        let rec eachProd = function
                            | PAlt(a,b) -> eachProd a && eachProd b
                            | PSeq(elements, _, _) ->
                                elements |> List.forall
                                    (fun elem -> match elem.rule with PSeq _ -> false | _ -> true)
                            | _ -> true
                        eachProd rule.body
                    )
                )
            
#if DEBUG
        let generator = 
           match GeneratorsManager.Component  "TreeDump" with
           | Some gen -> gen
           | None -> failwith "TreeDump is not found."
        printfn "%A\n" (generator.Generate ilTreeConverted)
#endif

        //treeDump.Generate expected |> string |> printfn "%s"
        treeDump.Generate ilTreeConverted |> string |> printfn "%s"
        Assert.True(hasNotInnerSeq)
   
[<TestFixture>]
type ``Expand rop level alters`` () =
    let basePath = System.IO.Path.Combine(conversionTestPath, "ExpandTopLevelAlters")
    let fe = getFrontend("YardFrontend")
    let conversion = "ExpandTopLevelAlt"

    [<Test>]
    member test.``No alter`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"noAlters.yrd"))
        let result = ConversionsManager.ApplyConversion conversion loadIL
        let rules =
            (verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t "d", None)}]
            ) @ (
                verySimpleNotStartRules "d"
                    [{dummyRule with rule = PToken (Source.t "NUM")}]
            )

        let expected = defaultDefinition rules
        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
        result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)

    [<Test>]
    member test.``One alter`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"oneAlter.yrd"))
        let result = ConversionsManager.ApplyConversion conversion loadIL
        let rules =
            (verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t "c", None)}]
            ) @ (
                verySimpleRules "s"
                    [{dummyRule with rule = PRef (Source.t "d", None)}]
            )

        let expected = defaultDefinition rules
        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
        result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)


    [<Test>]
    member test.``Multi alters`` () =
        let loadIL = fe.ParseGrammar (System.IO.Path.Combine(basePath,"multiAlters.yrd"))
        let result = ConversionsManager.ApplyConversion conversion loadIL
        let rules =
            (verySimpleRules "s"
                [{dummyRule with rule = PRef (Source.t "x", None)}]
            ) @ (
                verySimpleRules "s"
                    [{dummyRule with rule = PRef (Source.t "y", None)}]
            ) @ (
                verySimpleRules "s"
                    [{dummyRule with rule = PRef (Source.t "z", None)}]
            ) @ (
                verySimpleRules "s"
                    [{dummyRule with rule = PRef (Source.t "m", None)}]
            )
        let expected = defaultDefinition rules
        expected |> treeDump.Generate |> string |> printfn "%s"
        printfn "%s" "************************"
        result |> treeDump.Generate |> string |> printfn "%s"
        Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers expected.grammar result.grammar)