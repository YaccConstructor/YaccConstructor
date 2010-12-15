// RACCCoreTests.fs contains unit tests for RACCCore
//
//  Copyright 2010 Semen Grigorev <rsdpisuy@gmail.com>
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

module RACCCoreTests

open Microsoft.FSharp.Text.Lexing
open Yard.Generators.RACCGenerator
module Lexer = UserLexer
open NUnit.Framework

type Test<'a,'b,'c,'d,'f,'g,'h when 'a:comparison and 'f:comparison and 'g:comparison> =
    {
        tables     : Tables<'a,'b,'c,'d,'f,'g>
        actionsMap : System.Collections.Generic.IDictionary<string,'h>
        path       : string
        rightValue : seq<obj>
    }

let testPath = "..\\..\\..\\..\\Tests\\RACC\\"

let tests =
    [   (1,
            {
                tables     =
                    {
                        gotoSet = Tables_alt_in_cls.gotoSet
                        automataDict = Tables_alt_in_cls.autumataDict
                        items = Tables_alt_in_cls.items
                    }
                actionsMap = RACC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_1.yrd.in"
                rightValue = seq ["-" |> box]  
            })
        (2,
            {
                tables     =
                    {
                        gotoSet = Tables_alt_in_cls.gotoSet
                        automataDict = Tables_alt_in_cls.autumataDict
                        items = Tables_alt_in_cls.items
                    }
                actionsMap = RACC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_2.yrd.in"
                rightValue = seq ["-;-" |> box]  
            })
        (3,
            {
                tables     =
                    {
                        gotoSet = Tables_alt_in_cls.gotoSet
                        automataDict = Tables_alt_in_cls.autumataDict
                        items = Tables_alt_in_cls.items
                    }
                actionsMap = RACC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_3.yrd.in"
                rightValue = seq ["+" |> box]  
            })
        (4,
            {
                tables     =
                    {
                        gotoSet = Tables_alt_in_cls.gotoSet
                        automataDict = Tables_alt_in_cls.autumataDict
                        items = Tables_alt_in_cls.items
                    }
                actionsMap = RACC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_4.yrd.in"
                rightValue = seq ["+;+" |> box]  
            })
        (5,
            {
                tables     =
                    {
                        gotoSet = Tables_alt_in_cls.gotoSet
                        automataDict = Tables_alt_in_cls.autumataDict
                        items = Tables_alt_in_cls.items
                    }
                actionsMap = RACC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_5.yrd.in"
                rightValue = seq ["-;+" |> box]  
            })
        (6,
            {
                tables     =
                    {
                        gotoSet = Tables_alt_in_cls.gotoSet
                        automataDict = Tables_alt_in_cls.autumataDict
                        items = Tables_alt_in_cls.items
                    }
                actionsMap = RACC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_6.yrd.in"
                rightValue = seq ["+;-" |> box]  
            })
        (7,
            {
                tables     =
                    {
                        gotoSet = Tables_alt_in_cls.gotoSet
                        automataDict = Tables_alt_in_cls.autumataDict
                        items = Tables_alt_in_cls.items
                    }
                actionsMap = RACC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_7.yrd.in"
                rightValue = seq ["-;+;+;-;+;-;-" |> box]  
            })
        (8,
            {
                tables     =
                    {
                        gotoSet = Tables_Alt.gotoSet
                        automataDict = Tables_Alt.autumataDict
                        items = Tables_Alt.items
                    }
                actionsMap = RACC.Actions_Alt.ruleToAction
                path       = "test_alt\\test_alt_1.yrd.in"
                rightValue = seq ["Detected: +" |> box]  
            })
        (9,
            {
                tables     =
                    {
                        gotoSet = Tables_Alt.gotoSet
                        automataDict = Tables_Alt.autumataDict
                        items = Tables_Alt.items
                    }
                actionsMap = RACC.Actions_Alt.ruleToAction
                path       = "test_alt\\test_alt_2.yrd.in"
                rightValue = seq ["Detected: *" |> box]  
            })
        (10,
            {
                tables     =
                    {
                        gotoSet = Tables_Seq.gotoSet
                        automataDict = Tables_Seq.autumataDict
                        items = Tables_Seq.items
                    }
                actionsMap = RACC.Actions_Seq.ruleToAction
                path       = "test_seq\\test_seq_1.yrd.in"
                rightValue = seq [3.0 |> box]  
            })
        (11,
            {
                tables     =
                    {
                        gotoSet = Tables_Cls_tail.gotoSet
                        automataDict = Tables_Cls_tail.autumataDict
                        items = Tables_Cls_tail.items
                    }
                actionsMap = RACC.Actions_Cls_tail.ruleToAction
                path       = "test_cls_with_tail\\test_cls_with_tail_1.yrd.in"
                rightValue = seq ["tail minus" |> box]  
            })
        (12,
            {
                tables     =
                    {
                        gotoSet = Tables_Cls_tail.gotoSet
                        automataDict = Tables_Cls_tail.autumataDict
                        items = Tables_Cls_tail.items
                    }
                actionsMap = RACC.Actions_Cls_tail.ruleToAction
                path       = "test_cls_with_tail\\test_cls_with_tail_2.yrd.in"
                rightValue = seq ["list minus;tail minus" |> box]  
            })
        (13,
            {
                tables     =
                    {
                        gotoSet = Tables_Cls_head.gotoSet
                        automataDict = Tables_Cls_head.autumataDict
                        items = Tables_Cls_head.items
                    }
                actionsMap = RACC.Actions_Cls_head.ruleToAction
                path       = "test_cls_with_head\\test_cls_with_head_1.yrd.in"
                rightValue = seq ["Head minus" |> box]  
            })
        (14,
            {
                tables     =
                    {
                        gotoSet = Tables_Cls_head.gotoSet
                        automataDict = Tables_Cls_head.autumataDict
                        items = Tables_Cls_head.items
                    }
                actionsMap = RACC.Actions_Cls_head.ruleToAction
                path       = "test_cls_with_head\\test_cls_with_head_2.yrd.in"
                rightValue = seq ["Head minus;list minus" |> box]  
            })
        (15,
            {
                tables     =
                    {
                        gotoSet = Tables_Cls.gotoSet
                        automataDict = Tables_Cls.autumataDict
                        items = Tables_Cls.items
                    }
                actionsMap = RACC.Actions_Cls.ruleToAction
                path       = "test_cls\\test_cls_1.yrd.in"
                rightValue = seq ["" |> box]  
            })
        (16,
            {
                tables     =
                    {
                        gotoSet = Tables_Cls.gotoSet
                        automataDict = Tables_Cls.autumataDict
                        items = Tables_Cls.items
                    }
                actionsMap = RACC.Actions_Cls.ruleToAction
                path       = "test_cls\\test_cls_2.yrd.in"
                rightValue = seq ["*" |> box]  
            })
        (17,
            {
                tables     =
                    {
                        gotoSet = Tables_Cls.gotoSet
                        automataDict = Tables_Cls.autumataDict
                        items = Tables_Cls.items
                    }
                actionsMap = RACC.Actions_Cls.ruleToAction
                path       = "test_cls\\test_cls_3.yrd.in"
                rightValue = seq ["*;*" |> box]  
            })
        (18,
            {
                tables     =
                    {
                        gotoSet = Tables_Aritm_glr.gotoSet
                        automataDict = Tables_Aritm_glr.autumataDict
                        items = Tables_Aritm_glr.items
                    }
                actionsMap = RACC.Actions_Aritm_glr.ruleToAction
                path       = "test_arithm_glr\\test_arithm_glr_1.yrd.in"
                rightValue = seq [1 |> box]  
            })
        (19,
            {
                tables     =
                    {
                        gotoSet = Tables_Aritm_glr.gotoSet
                        automataDict = Tables_Aritm_glr.autumataDict
                        items = Tables_Aritm_glr.items
                    }
                actionsMap = RACC.Actions_Aritm_glr.ruleToAction
                path       = "test_arithm_glr\\test_arithm_glr_2.yrd.in"
                rightValue = seq [3 |> box]  
            })
        (20,
            {
                tables     =
                    {
                        gotoSet = Tables_Aritm_glr.gotoSet
                        automataDict = Tables_Aritm_glr.autumataDict
                        items = Tables_Aritm_glr.items
                    }
                actionsMap = RACC.Actions_Aritm_glr.ruleToAction
                path       = "test_arithm_glr\\test_arithm_glr_3.yrd.in"
                rightValue = seq [6 |> box]  
            })
        (21,
            {
                tables     =
                    {
                        gotoSet = Tables_Aritm_glr.gotoSet
                        automataDict = Tables_Aritm_glr.autumataDict
                        items = Tables_Aritm_glr.items
                    }
                actionsMap = RACC.Actions_Aritm_glr.ruleToAction
                path       = "test_arithm_glr\\test_arithm_glr_4.yrd.in"
                rightValue = seq [2 |> box]  
            })
        (22,
            {
                tables     =
                    {
                        gotoSet = Tables_Aritm_glr.gotoSet
                        automataDict = Tables_Aritm_glr.autumataDict
                        items = Tables_Aritm_glr.items
                    }
                actionsMap = RACC.Actions_Aritm_glr.ruleToAction
                path       = "test_arithm_glr\\test_arithm_glr_5.yrd.in"
                rightValue = seq [5 |> box; 3 |> box; -3 |> box; 3 |> box; -1 |> box]  
            })
        (23,
            {
                tables     =
                    {
                        gotoSet = Tables_L_attr.gotoSet
                        automataDict = Tables_L_attr.autumataDict
                        items = Tables_L_attr.items
                    }
                actionsMap = RACC.Actions_L_attr.ruleToAction
                path       = "test_l_attr\\test_l_attr_1.yrd.in"
                rightValue = seq [3 |> box]  
            })
        (24,
            {
                tables     =
                    {
                        gotoSet = Tables_Simple_checker.gotoSet
                        automataDict = Tables_Simple_checker.autumataDict
                        items = Tables_Simple_checker.items
                    }
                actionsMap = RACC.Actions_Simple_checker.ruleToAction
                path       = "test_simple_checker\\test_simple_checker_1.yrd.in"
                rightValue = seq [7 |> box]  
            })
        (25,
            {
                tables     =
                    {
                        gotoSet = Tables_Simple_checker.gotoSet
                        automataDict = Tables_Simple_checker.autumataDict
                        items = Tables_Simple_checker.items
                    }
                actionsMap = RACC.Actions_Simple_checker.ruleToAction
                path       = "test_simple_checker\\test_simple_checker_2.yrd.in"
                rightValue = seq [12 |> box; 12 |> box]  
            })

    ]
    |> dict

let run_common path = 
    let content = System.IO.File.ReadAllText(path)    
    let reader = new System.IO.StringReader(content) in
    LexBuffer<_>.FromTextReader reader

let run path tables actions =
    let buf = run_common path 
    let l = UserLexer.Lexer(buf)        
    let trees = TableInterpreter.run l tables            
    Seq.map (fun tree -> ASTInterpretator.interp actions tree) trees
    |> Seq.filter (function | Success _ -> true | _ -> false)
    |> Seq.map (function | Success x -> x | _ -> failwith "Incorrect filter")  

[<TestFixture>]
type ``RACC core tests`` ()=    
    [<Test>] 
    member test.``Alt in closure test 1`` () =
        let test = tests.[1]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt in closure test 2`` () =
        let test = tests.[2]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt in closure test 3`` () =
        let test = tests.[3]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt in closure test 4`` () =
        let test = tests.[4]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt in closure test 5`` () =
        let test = tests.[5]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt in closure test 6`` () =
        let test = tests.[6]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt in closure test 7`` () =
        let test = tests.[7]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt test 1`` () =
        let test = tests.[8]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt test 2`` () =
        let test = tests.[9]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Seq test 1`` () =
        let test = tests.[10]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure with tail test 1`` () =
        let test = tests.[11]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure with tail test 2`` () =
        let test = tests.[12]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure with head test 1`` () =
        let test = tests.[13]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure with head test 2`` () =
        let test = tests.[14]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure test 1`` () =
        let test = tests.[15]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure test 2`` () =
        let test = tests.[16]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure test 3`` () =
        let test = tests.[17]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Arithm glr test 2`` () =
        let test = tests.[19]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Arithm glr test 3`` () =
        let test = tests.[20]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Arithm glr test 4`` () =
        let test = tests.[21]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Arithm glr test 5`` () =
        let test = tests.[22]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Arithm glr test 1`` () =
        let test = tests.[18]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``L attribute test 1`` () =
        let test = tests.[23]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Simple checker test 1`` () =
        let test = tests.[24]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Simple checker test 2`` () =
        let test = tests.[25]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)