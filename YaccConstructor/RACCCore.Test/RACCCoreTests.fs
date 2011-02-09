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

type Test<'a,'b,'c,'d,'h when 'c:comparison and 'd:comparison> =
    {
        tables     : Tables<'a,'b,'c,'d>
        actionsMap : System.Collections.Generic.IDictionary<string,'h>
        path       : string
        rightValue : seq<obj>
    }

let testPath = "..\\..\\..\\..\\Tests\\RACC\\"
let performanceFolder = "Performance"

let tests =
    [   (1,
            {
                tables     =
                    {
                        gotoSet = Tables_alt_in_cls.gotoSet
                        automataDict = Tables_alt_in_cls.autumataDict                        
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
                    }
                actionsMap = RACC.Actions_Simple_checker.ruleToAction
                path       = "test_simple_checker\\test_simple_checker_2.yrd.in"
                rightValue = seq [12 |> box; 12 |> box]  
            })
        (26,
            {
                tables     =
                    {
                        gotoSet = Tables_Checker_on_glr.gotoSet
                        automataDict = Tables_Checker_on_glr.autumataDict
                    }
                actionsMap = RACC.Actions_Checker_on_glr.ruleToAction
                path       = "test_checker_on_glr\\test_checker_on_glr_1.yrd.in"
                rightValue = seq [3 |> box]  
            })
        (27,
            {
                tables     =
                    {
                        gotoSet = Tables_Checker_on_glr.gotoSet
                        automataDict = Tables_Checker_on_glr.autumataDict
                    }
                actionsMap = RACC.Actions_Checker_on_glr.ruleToAction
                path       = "test_checker_on_glr\\test_checker_on_glr_2.yrd.in"
                rightValue = seq [10 |> box]  
            })
        (28,
            {
                tables     =
                    {
                        gotoSet = Tables_Checker_on_glr.gotoSet
                        automataDict = Tables_Checker_on_glr.autumataDict
                    }
                actionsMap = RACC.Actions_Checker_on_glr.ruleToAction
                path       = "test_checker_on_glr\\test_checker_on_glr_3.yrd.in"
                rightValue = seq [11 |> box]  
            })
        (29,
            {
                tables     =
                    {
                        gotoSet = Tables_Checker_on_glr.gotoSet
                        automataDict = Tables_Checker_on_glr.autumataDict                        
                    }
                actionsMap = RACC.Actions_Checker_on_glr.ruleToAction
                path       = "test_checker_on_glr\\test_checker_on_glr_4.yrd.in"
                rightValue = seq [40 |> box]  
            })

        (30,
            {
                tables     =
                    {
                        gotoSet = Tables_Summator_1.gotoSet
                        automataDict = Tables_Summator_1.autumataDict                        
                    }
                actionsMap = RACC.Actions_Summator_1.ruleToAction
                path       = "test_summator_1\\test_summator_1_1.yrd.in"
                rightValue = seq [1.0 |> box]  
            })

        (31,
            {
                tables     =
                    {
                        gotoSet = Tables_Summator_1.gotoSet
                        automataDict = Tables_Summator_1.autumataDict                        
                    }
                actionsMap = RACC.Actions_Summator_1.ruleToAction
                path       = "test_summator_1\\test_summator_1_2.yrd.in"
                rightValue = seq [3.0 |> box]  
            })

        (32,
            {
                tables     =
                    {
                        gotoSet = Tables_Summator_1.gotoSet
                        automataDict = Tables_Summator_1.autumataDict                        
                    }
                actionsMap = RACC.Actions_Summator_1.ruleToAction
                path       = "test_summator_1\\test_summator_1_3.yrd.in"
                rightValue = seq [6.0 |> box]  
            })

        (33,
            {
                tables     =
                    {
                        gotoSet = Tables_Opt.gotoSet
                        automataDict = Tables_Opt.autumataDict                        
                    }
                actionsMap = RACC.Actions_Opt.ruleToAction
                path       = "test_opt\\test_opt_1.yrd.in"
                rightValue = seq ["1 +" |> box]  
            })

        (33,
            {
                tables     =
                    {
                        gotoSet = Tables_Opt.gotoSet
                        automataDict = Tables_Opt.autumataDict                        
                    }
                actionsMap = RACC.Actions_Opt.ruleToAction
                path       = "test_opt\\test_opt_2.yrd.in"
                rightValue = seq ["1" |> box]  
            })

        (34,
            {
                tables     =
                    {
                        gotoSet = Tables_Alt.gotoSet
                        automataDict = Tables_Alt.autumataDict                        
                    }
                actionsMap = RACC.Actions_Alt.ruleToAction
                path       = "test_alt\\test_alt_3.yrd.in"
                rightValue = seq ["0 NUMBER 1"]  
            })

        (35,
            {
                tables     =
                    {
                        gotoSet = Tables_alt_in_cls.gotoSet
                        automataDict = Tables_alt_in_cls.autumataDict                        
                    }
                actionsMap = RACC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_8.yrd.in"
                rightValue = seq ["5 NUMBER 2"]  
            })

        (36,
            {
                tables     =
                    {
                        gotoSet = Tables_Summator_1.gotoSet
                        automataDict = Tables_Summator_1.autumataDict                        
                    }
                actionsMap = RACC.Actions_Summator_1.ruleToAction
                path       = "test_summator_1\\test_summator_1_4.yrd.in"
                rightValue = seq ["4 PLUS +"]  
            })

        (37,
            {
                tables     =
                    {
                        gotoSet = Tables_Aritm_glr.gotoSet
                        automataDict = Tables_Aritm_glr.autumataDict                        
                    }
                actionsMap = RACC.Actions_Aritm_glr.ruleToAction
                path       = "test_arithm_glr\\test_arithm_glr_6.yrd.in"
                rightValue = seq ["4 MULT *"]  
            })
    ]
    |> dict

type TestStatus =
    | TSuccess of seq<obj>
    | TError of string

let run_common path = 
    let content = System.IO.File.ReadAllText(path)    
    let reader = new System.IO.StringReader(content) in
    LexBuffer<_>.FromTextReader reader

let runMain path tables actions =
    let buf = run_common path 
    let l = UserLexer.Lexer(buf)    
    let parseRes,cache,cc = TableInterpreter.run l tables    
    let res  = 
        match parseRes with
        | PSuccess (forest) -> 
            Seq.map (fun tree -> ASTInterpretator.interp actions cache tree) forest    
            |> Seq.filter (function | Success _ -> true | _ -> false)
            |> Seq.map (function | Success x -> x | _ -> failwith "Incorrect filter")
            |> TSuccess
        | PError (pos) -> 
            TError((l:>ILexer<string>).Get(if pos = 0 then 1 else pos) 
            |> fun x -> (String.concat " " [pos.ToString(); x.name; x.value]))

    res,cache,cc      

let run path tables actions = 
    match runMain path tables actions with
    | (TSuccess(r),_,_) -> r
    | _                 -> Seq.empty

let eRun path tables actions = 
    match runMain path tables actions with
    | (TError(r),_,_) -> 
    #if debug
        printf "Error: %A" r
    #endif
        Seq.singleton r
    | _               -> Seq.empty

let pRun path tables actions =
    match runMain path tables actions with
    | (_,_,cc) -> cc

type RACCPerformanceTests () =
    static member Test1 = 
        let test = tests.[1] 
        fun x -> pRun x test.tables test.actionsMap
        , testPath + "/test_alt_in_cls/" + performanceFolder

    static member Test2 = 
        let test = tests.[30] 
        fun x -> pRun x test.tables test.actionsMap
        , testPath + "/test_summator_1/" + performanceFolder

     

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
    member test.``Checker in glr arithmetic test 1`` () =
        let test = tests.[26]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Checker in glr arithmetic test 2`` () =
        let test = tests.[27]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Checker in glr arithmetic test 3`` () =
        let test = tests.[28]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Checker in glr arithmetic test 4`` () =
        let test = tests.[29]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Simple checker test 2`` () =
        let test = tests.[25]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Smmator_1 1`` () =
        let test = tests.[30]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Summator_1 2`` () =
        let test = tests.[31]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Summator_1 3`` () =
        let test = tests.[32]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Option 1`` () =
        let test = tests.[32]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Option 2`` () =
        let test = tests.[33]
        let res = run (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt error`` () =
        let test = tests.[34]
        let res = eRun (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt in cls error`` () =
        let test = tests.[35]
        let res = eRun (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Summator 1 error`` () =
        let test = tests.[36]
        let res = eRun (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Arithm glr error`` () =
        let test = tests.[37]
        let res = eRun (testPath + test.path) test.tables test.actionsMap
        Assert.AreEqual(test.rightValue,res)