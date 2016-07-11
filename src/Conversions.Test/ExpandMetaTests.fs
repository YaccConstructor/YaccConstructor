//  ConversionsTests.fs contains unuit test for Conversions
//
//  Copyright 2009,2010,2011 Konstantin Ulitin <ulitin.k@gmail.com>
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

module ExpandMetaTests

open System.IO
open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Conversions.TransformAux
open NUnit.Framework
open ConversionsTests
open Yard.Core.Helpers
open Mono.Addins

[<SetUpFixture>]
type SetUp()=
    [<SetUp>]
    member this.SetUp () =
        AddinManager.Initialize()
        AddinManager.Registry.Update()

[<TestFixture>]
type ``Conversions expand metarules tests`` () =
    
    let basePath = System.IO.Path.Combine(conversionTestPath, "Meta")
    let applyConversion = applyConversion expandMeta

    let runMetaTest srcFile =
        let srcPathFile = System.IO.Path.Combine(basePath, srcFile)                                         
        let ilTree = fe.ParseGrammar srcPathFile                
        Namer.initNamer ilTree.grammar
        let ilTreeConverted = applyConversion ilTree 
        let expected =
            try
                srcPathFile + ".ans" |> fe.ParseGrammar
            with
            | e -> printfn "%s" e.Message
                   failwith e.Message
        
        //treeDump.Generate expected |> string |> printfn "%s"
        //treeDump.Generate ilTreeConverted |> string |> printfn "%s"
        if not <| ILComparators.GrammarEqualsWithoutLineNumbers ilTreeConverted.grammar expected.grammar then
            let text = (new Yard.Generators.YardPrinter.YardPrinter()).Generate { ilTree with grammar=ilTreeConverted.grammar }
            Directory.CreateDirectory "out" |> ignore
            File.WriteAllText (Path.Combine ("out", srcFile + ".ans"), text :?> string)
            Assert.Fail "Trees are not equal"
            

    [<Test>]    
    member test.``Meta test 1``()=runMetaTest("meta_1.yrd")

    [<Test>]    
    member test.``Meta test args``()=runMetaTest("meta_args_bad.yrd")

    [<Test>]    
    member test.``Meta test args order``()=runMetaTest("meta_args_order_bad.yrd")

    [<Test>]    
    member test.``Meta test attributes``()=runMetaTest("meta_attrs.yrd")

    [<Test>]    
    member test.``Meta test bindings``()=runMetaTest("meta_bind.yrd")

    [<Test>]    
    member test.``Meta test diff``()=runMetaTest("meta_diff_bad.yrd")

    [<Test>]    
    member test.``Meta test duplicate``()=runMetaTest("meta_duplicate.yrd")

    [<Test>]    
    member test.``Meta test enclosure``()=runMetaTest("meta_enclosure_bad.yrd")

    [<Test>]    
    member test.``Meta test enclosure simple``()=runMetaTest("meta_enclosure_simple_bad.yrd")

    [<Test>]    
    member test.``Meta test group``()=runMetaTest("meta_group.yrd")

    [<Test>]    
    member test.``Meta test 2``()=runMetaTest("meta_meta.yrd")

    [<Test>]    
    member test.``Meta test permutations``()=runMetaTest("meta_perm.yrd")

    [<Test>]    
    member test.``Meta test ptoken to pref``()=runMetaTest("meta_ptoken2pref.yrd")

    [<Test>]    
    member test.``Meta test ptoken to pref 2``()=runMetaTest("meta_ptoken2pref2.yrd")

    [<Test>]    
    member test.``Meta test recursion 1``()=runMetaTest("meta_rec.yrd")

    [<Test>]    
    member test.``Meta test recursion 2``()=runMetaTest("meta_recursion.yrd")

    [<Test>]    
    member test.``Meta test two calls``()=runMetaTest("meta_two.yrd")

    [<Test>]    
    member test.``Meta test modules easy``()=runMetaTest("meta_modules_easy.yrd")

    [<Test>]    
    member test.``Meta test modules hard``()=runMetaTest("meta_modules_hard.yrd")

