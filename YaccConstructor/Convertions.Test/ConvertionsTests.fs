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

let convertionTestPath = @"../../../../Tests/Convertions/"
let GeneratorsManager = Yard.Core.GeneratorsManager.GeneratorsManager()

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
                        | PSeq(elements, _) -> elements |> List.forall (fun elem -> match elem.rule with PSeq _ -> false | _ -> true)
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

    [<Test>]
    /// Source file name have to be in format 'convName1_convName2_descr.yrd'.
    /// Expected result 'sourceFileName.res'
    member test.``Meta tests in Tests\Convertions\Meta .``()=
        let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager()
        let frontend =
            match FrontendsManager.Component "YardFrontend" with
                   | Some fron -> fron
                   | None -> failwith "YardFrontend is not found." 
        (*let generator = 
           match GeneratorsManager.Component "YardPrinter" with
           | Some gen -> gen
           | None -> failwith "YardPrinter is not found." 
        printfn "%A" generator*)
        System.IO.Directory.EnumerateFiles(convertionTestPath+"Meta/","*.yrd") 
        |> Seq.iter 
            (fun srcFile ->  
                Namer.resetRuleEnumerator()
                printfn "file %s" srcFile
                // all convertions are in name without descr.yrd
                let convertions = [|"ExpandMeta"|]
                printfn "%A" convertions
                Namer.resetRuleEnumerator()
                let ilTree = (srcFile |> frontend.ParseGrammar)
                printfn "after Parsing"
                let reorder f a b = f b a
//                    let ilTreeConverted = Array.fold (reorder ConvertionsManager.ApplyConvertion) ilTree convertions
                let ilTreeConverted = Array.fold (reorder ConvertionsManager.ApplyConvertion) ilTree convertions
                printfn "after Convertion"
                Namer.resetRuleEnumerator()
                let expected =
                    try
                        srcFile + ".ans" |> frontend.ParseGrammar 
                    with
                        | e -> printfn "%s" e.Message
                               raise <| FEError e.Message
                printfn "after result parsing"
                //printf "result:%A\nexpected:\n%A\n" ilTreeConverted.grammar expected.grammar
                Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers ilTreeConverted.grammar expected.grammar) 

            )
        Assert.True(true)
        
    [<Test>]
    /// Source file name have to be in format 'convName1_convName2_descr.yrd'.
    /// Expected result 'sourceFileName.res'
    member test.``Batch tests in Tests\Convertions\Batch .``()=
        let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager()
        let frontend =
            match FrontendsManager.Component "YardFrontend" with
               | Some fron -> fron
               | None -> failwith "YardFrontend is not found."
        (*let generator = 
           match GeneratorsManager.Component "YardPrinter" with
           | Some gen -> gen
           | None -> failwith "YardPrinter is not found."
        printfn "%A" generator*)
        System.IO.Directory.EnumerateFiles(convertionTestPath+"Batch/","*.yrd") 
        |> Seq.iter 
            (fun srcFile ->  
                Namer.resetRuleEnumerator()
                printfn "file %s" srcFile
                let srcFileName = System.IO.Path.GetFileName(srcFile)
                let srcPrefix = System.IO.Path.GetFileNameWithoutExtension(srcFileName)
                let prefixSplitted = srcPrefix.Split('_')
                printfn "1" 
                // all convertions are in name without descr.yrd
                let convertions = prefixSplitted.[0..(Array.length prefixSplitted - 2)]
                printfn "2: %A" convertions
                Namer.resetRuleEnumerator()
                let ilTree = (srcFile |> frontend.ParseGrammar)
                printfn "3"
                let reorder f a b = f b a
//                    let ilTreeConverted = Array.fold (reorder ConvertionsManager.ApplyConvertion) ilTree convertions
                let ilTreeConverted = Array.fold (reorder ConvertionsManager.ApplyConvertion) ilTree convertions
                printfn "4"
                Namer.resetRuleEnumerator()
                let expected =
                    try
                        srcFile + ".res" |> frontend.ParseGrammar 
                    with
                        | e -> printfn "%s" e.Message
                               raise <| FEError e.Message
                printfn "5"
                printf "result:%A\nexpected:\n%A\n" ilTreeConverted.grammar expected.grammar
                Assert.IsTrue(ILComparators.GrammarEqualsWithoutLineNumbers ilTreeConverted.grammar expected.grammar) 
//                     with e ->
//                        printfn "%A" e 

            )
        Assert.True(true)