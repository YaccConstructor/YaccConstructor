//  ConvertionsTests.fs contains unuit test for Convertions
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

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Convertions.TransformAux
open NUnit.Framework
open ConvertionsTests

[<TestFixture>]
type ``Convertions expand metarules tests`` () =
    
    let basePath = System.IO.Path.Combine(convertionTestPath, "Meta")
    let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager()
    let convertion = "ExpandMeta" 
    let getFrontend name =
        match FrontendsManager.Component name with
        | Some fe -> fe
        | None -> failwith (name + " is not found.")

    let getBE name =
        match GeneratorsManager.Component name with
        | Some be -> be
        | None -> failwith (name + " is not found.")

    let frontend = getFrontend "YardFrontend"
    let treeDump = getBE "TreeDump"

    let runMetaTest srcFile =
        let srcFile = System.IO.Path.Combine(basePath, srcFile)                                         
        Namer.resetRuleEnumerator()
        let ilTree = frontend.ParseGrammar srcFile                
        let ilTreeConverted = ConvertionsManager.ApplyConvertion convertion ilTree 
        Namer.resetRuleEnumerator()
        let expected =
            try
                srcFile + ".ans" |> frontend.ParseGrammar
            with
            | e -> printfn "%s" e.Message
                   failwith e.Message
        
        treeDump.Generate expected |> string |> printfn "%s"
        treeDump.Generate ilTreeConverted |> string |> printfn "%s"
        Assert.IsTrue (grammarEqualsWithoutLineNumbers ilTreeConverted.grammar expected.grammar) 

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

