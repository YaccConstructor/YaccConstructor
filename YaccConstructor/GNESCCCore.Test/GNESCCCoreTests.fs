// GNESCCCoreTests.fs contains unit tests for GNESCCCore
//
//  Copyright 2010, 2011 Semen Grigorev <rsdpisuy@gmail.com>
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

module GNESCCCoreTests

open Microsoft.FSharp.Text.Lexing
open Yard.Generators.GNESCCGenerator
open NUnit.Framework

type Test<'h, 'lexer> =
    {
        tables     : Tables
        actionsMap : System.Collections.Generic.IDictionary<int,'h>
        path       : string
        rightValue : seq<obj>
        lexer      : 'lexer
    }

let testPath = "..\\..\\..\\..\\Tests\\GNESCC\\"
let performanceFolder = "Performance"

let tests =
    [   (1,
            {
                tables     = Tables_alt_in_cls.tables                  
                actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_1.yrd.in"
                rightValue = seq ["-" |> box]
                lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer
            })
        (2,
            {
                tables     = Tables_alt_in_cls.tables
                actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_2.yrd.in"
                rightValue = seq ["-;-" |> box] 
                lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer
            })
        (3,
            {
                tables     = Tables_alt_in_cls.tables
                actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_3.yrd.in"
                rightValue = seq ["+" |> box]
                lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer    
            })
        (4,
            {
                tables     = Tables_alt_in_cls.tables
                actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_4.yrd.in"
                rightValue = seq ["+;+" |> box]
                lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer
            })
        (5,
            {
                tables     = Tables_alt_in_cls.tables
                actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_5.yrd.in"
                rightValue = seq ["-;+" |> box]
                lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer    
            })
        (6,
            {
                tables     = Tables_alt_in_cls.tables
                actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_6.yrd.in"
                rightValue = seq ["+;-" |> box]
                lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer    
            })
        (7,
            {
                tables     = Tables_alt_in_cls.tables
                actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_7.yrd.in"
                rightValue = seq ["-;+;+;-;+;-;-" |> box]
                lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer  
            })
        (8,
            {
                tables     = Tables_alt.tables
                actionsMap = GNESCC.Actions_alt.ruleToAction
                path       = "test_alt\\test_alt_1.yrd.in"
                rightValue = seq ["Detected: +" |> box]
                lexer      = fun buf -> Lexer_alt.Lexer(buf):>ILexer
            })
        (9,
            {
                tables     = Tables_alt.tables
                actionsMap = GNESCC.Actions_alt.ruleToAction
                path       = "test_alt\\test_alt_2.yrd.in"
                rightValue = seq ["Detected: *" |> box]
                lexer      = fun buf -> Lexer_alt.Lexer(buf):>ILexer  
            })
        (10,
            {
                tables     = Tables_seq.tables                    
                actionsMap = GNESCC.Actions_seq.ruleToAction
                path       = "test_seq\\test_seq_1.yrd.in"
                rightValue = seq [3.0 |> box]
                lexer      = fun buf -> Lexer_seq.Lexer(buf):>ILexer 
            })
        (11,
            {
                tables     = Tables_cls_with_tail.tables
                actionsMap = GNESCC.Actions_cls_with_tail.ruleToAction
                path       = "test_cls_with_tail\\test_cls_with_tail_1.yrd.in"
                rightValue = seq ["tail minus" |> box]
                lexer      = fun buf -> Lexer_cls_with_tail.Lexer(buf):>ILexer
            })
        (12,
            {
                tables     = Tables_cls_with_tail.tables
                actionsMap = GNESCC.Actions_cls_with_tail.ruleToAction
                path       = "test_cls_with_tail\\test_cls_with_tail_2.yrd.in"
                rightValue = seq ["list minus;tail minus" |> box]
                lexer      = fun buf -> Lexer_cls_with_tail.Lexer(buf):>ILexer
            })
        (13,
            {
                tables     = Tables_cls_with_head.tables
                actionsMap = GNESCC.Actions_cls_with_head.ruleToAction
                path       = "test_cls_with_head\\test_cls_with_head_1.yrd.in"
                rightValue = seq ["Head minus" |> box]
                lexer      = fun buf -> Lexer_cls_with_head.Lexer(buf):>ILexer
            })
        (14,
            {
                tables     = Tables_cls_with_head.tables
                actionsMap = GNESCC.Actions_cls_with_head.ruleToAction
                path       = "test_cls_with_head\\test_cls_with_head_2.yrd.in"
                rightValue = seq ["Head minus;list minus" |> box]
                lexer      = fun buf -> Lexer_cls_with_head.Lexer(buf):>ILexer
            })
        (15,
            {
                tables     = Tables_cls.tables
                actionsMap = GNESCC.Actions_cls.ruleToAction
                path       = "test_cls\\test_cls_1.yrd.in"
                rightValue = seq ["" |> box]
                lexer      = fun buf -> Lexer_cls.Lexer(buf):>ILexer
            })
        (16,
            {
                tables     = Tables_cls.tables
                actionsMap = GNESCC.Actions_cls.ruleToAction
                path       = "test_cls\\test_cls_2.yrd.in"
                rightValue = seq ["*" |> box]
                lexer      = fun buf -> Lexer_cls.Lexer(buf):>ILexer
            })
        (17,
            {
                tables     = Tables_cls.tables
                actionsMap = GNESCC.Actions_cls.ruleToAction
                path       = "test_cls\\test_cls_3.yrd.in"
                rightValue = seq ["*;*" |> box]
                lexer      = fun buf -> Lexer_cls.Lexer(buf):>ILexer
            })
        (18,
            {
                tables     = Tables_arithm_glr.tables
                actionsMap = GNESCC.Actions_arithm_glr.ruleToAction
                path       = "test_arithm_glr\\test_arithm_glr_1.yrd.in"
                rightValue = seq [1 |> box]
                lexer      = fun buf -> Lexer_arithm_glr.Lexer(buf):>ILexer
            })
        (19,
            {
                tables     = Tables_arithm_glr.tables
                actionsMap = GNESCC.Actions_arithm_glr.ruleToAction
                path       = "test_arithm_glr\\test_arithm_glr_2.yrd.in"
                rightValue = seq [3 |> box]
                lexer      = fun buf -> Lexer_arithm_glr.Lexer(buf):>ILexer
            })
        (20,
            {
                tables     = Tables_arithm_glr.tables
                actionsMap = GNESCC.Actions_arithm_glr.ruleToAction
                path       = "test_arithm_glr\\test_arithm_glr_3.yrd.in"
                rightValue = seq [6 |> box]
                lexer      = fun buf -> Lexer_arithm_glr.Lexer(buf):>ILexer
            })
        (21,
            {
                tables     = Tables_arithm_glr.tables
                actionsMap = GNESCC.Actions_arithm_glr.ruleToAction
                path       = "test_arithm_glr\\test_arithm_glr_4.yrd.in"
                rightValue = seq [2 |> box]
                lexer      = fun buf -> Lexer_arithm_glr.Lexer(buf):>ILexer
            })
        (22,
            {
                tables     = Tables_arithm_glr.tables
                actionsMap = GNESCC.Actions_arithm_glr.ruleToAction
                path       = "test_arithm_glr\\test_arithm_glr_5.yrd.in"
                rightValue = seq [5 |> box; 3 |> box; -3 |> box; 3 |> box; -1 |> box]
                lexer      = fun buf -> Lexer_arithm_glr.Lexer(buf):>ILexer
            })
        (23,
            {
                tables     = Tables_l_attr.tables
                actionsMap = GNESCC.Actions_l_attr.ruleToAction
                path       = "test_l_attr\\test_l_attr_1.yrd.in"
                rightValue = seq [3 |> box]
                lexer      = fun buf -> Lexer_L_attr.Lexer(buf):>ILexer
            })
        (24,
            {
                tables     = Tables_simple_checker.tables
                actionsMap = GNESCC.Actions_simple_checker.ruleToAction
                path       = "test_simple_checker\\test_simple_checker_1.yrd.in"
                rightValue = seq [7 |> box]
                lexer      = fun buf -> Lexer_simple_checker.Lexer(buf):>ILexer
            })
        (25,
            {
                tables     = Tables_simple_checker.tables
                actionsMap = GNESCC.Actions_simple_checker.ruleToAction
                path       = "test_simple_checker\\test_simple_checker_2.yrd.in"
                rightValue = seq [12 |> box; 12 |> box]
                lexer      = fun buf -> Lexer_simple_checker.Lexer(buf):>ILexer
            })
        (26,
            {
                tables     = Tables_checker_on_glr.tables
                actionsMap = GNESCC.Actions_checker_on_glr.ruleToAction
                path       = "test_checker_on_glr\\test_checker_on_glr_1.yrd.in"
                rightValue = seq [3 |> box]
                lexer      = fun buf -> Lexer_checker_on_glr.Lexer(buf):>ILexer
            })
        (27,
            {
                tables     = Tables_checker_on_glr.tables
                actionsMap = GNESCC.Actions_checker_on_glr.ruleToAction
                path       = "test_checker_on_glr\\test_checker_on_glr_2.yrd.in"
                rightValue = seq [10 |> box]
                lexer      = fun buf -> Lexer_checker_on_glr.Lexer(buf):>ILexer
            })
        (28,
            {
                tables     = Tables_checker_on_glr.tables
                actionsMap = GNESCC.Actions_checker_on_glr.ruleToAction
                path       = "test_checker_on_glr\\test_checker_on_glr_3.yrd.in"
                rightValue = seq [11 |> box]
                lexer      = fun buf -> Lexer_checker_on_glr.Lexer(buf):>ILexer
            })
        (29,
            {
                tables     = Tables_checker_on_glr.tables
                actionsMap = GNESCC.Actions_checker_on_glr.ruleToAction
                path       = "test_checker_on_glr\\test_checker_on_glr_4.yrd.in"
                rightValue = seq [40 |> box]
                lexer      = fun buf -> Lexer_checker_on_glr.Lexer(buf):>ILexer
            })

        (30,
            {
                tables     = Tables_summator_1.tables
                actionsMap = GNESCC.Actions_summator_1.ruleToAction
                path       = "test_summator_1\\test_summator_1_1.yrd.in"
                rightValue = seq [1.0 |> box]
                lexer      = fun buf -> Lexer_summator_1.Lexer(buf):>ILexer
            })

        (31,
            {
                tables     = Tables_summator_1.tables
                actionsMap = GNESCC.Actions_summator_1.ruleToAction
                path       = "test_summator_1\\test_summator_1_2.yrd.in"
                rightValue = seq [3.0 |> box]
                lexer      = fun buf -> Lexer_summator_1.Lexer(buf):>ILexer
            })

        (32,
            {
                tables     = Tables_summator_1.tables
                actionsMap = GNESCC.Actions_summator_1.ruleToAction
                path       = "test_summator_1\\test_summator_1_3.yrd.in"
                rightValue = seq [6.0 |> box]
                lexer      = fun buf -> Lexer_summator_1.Lexer(buf):>ILexer
            })

        (33,
            {
                tables     = Tables_opt.tables
                actionsMap = GNESCC.Actions_opt.ruleToAction
                path       = "test_opt\\test_opt_1.yrd.in"
                rightValue = seq ["1 +" |> box]
                lexer      = fun buf -> Lexer_opt.Lexer(buf):>ILexer  
            })

        (333,
            {
                tables     = Tables_opt.tables
                actionsMap = GNESCC.Actions_opt.ruleToAction
                path       = "test_opt\\test_opt_2.yrd.in"
                rightValue = seq ["1" |> box]
                lexer      = fun buf -> Lexer_opt.Lexer(buf):>ILexer  
            })

        (34,
            {
                tables     = Tables_alt.tables
                actionsMap = GNESCC.Actions_alt.ruleToAction
                path       = "test_alt\\test_alt_3.yrd.in"
                rightValue = seq ["1 NUMBER 1"]
                lexer      = fun buf -> Lexer_alt.Lexer(buf):>ILexer  
            })

        (35,
            {
                tables     = Tables_alt_in_cls.tables
                actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
                path       = "test_alt_in_cls\\test_alt_in_cls_8.yrd.in"
                rightValue = seq ["5 NUMBER 2"]
                lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer  
            })

        (36,
            {
                tables     = Tables_summator_1.tables
                actionsMap = GNESCC.Actions_summator_1.ruleToAction
                path       = "test_summator_1\\test_summator_1_4.yrd.in"
                rightValue = seq ["5 PLUS +"]
                lexer      = fun buf -> Lexer_summator_1.Lexer(buf):>ILexer  
            })

        (37,
            {
                tables     = Tables_arithm_glr.tables
                actionsMap = GNESCC.Actions_arithm_glr.ruleToAction
                path       = "test_arithm_glr\\test_arithm_glr_6.yrd.in"
                rightValue = seq ["5 MINUS -"]
                lexer      = fun buf -> Lexer_arithm_glr.Lexer(buf):>ILexer  
            })

        (38,
            {
                tables     = Tables_reduce_reduce.tables
                actionsMap = GNESCC.Actions_reduce_reduce.ruleToAction
                path       = "test_reduce_reduce\\test_reduce_reduce_1.yrd.in"
                rightValue = seq ["A" |> box; "B" |> box]
                lexer      = fun buf -> Lexer_reduce_reduce.Lexer(buf):>ILexer  
            })

        (39,
            {
                tables     = Tables_simple_braces.tables
                actionsMap = GNESCC.Actions_simple_braces.ruleToAction
                path       = "\claret\\braces_1\\test_simple_braces_1.yrd.in"
                rightValue = seq [1 |> box]
                lexer      = fun buf -> Lexer_simple_braces.Lexer(buf):>ILexer  
            })

        (40,
            {
                tables     = Tables_simple_braces.tables
                actionsMap = GNESCC.Actions_simple_braces.ruleToAction
                path       = "\claret\\braces_1\\test_simple_braces_2.yrd.in"
                rightValue = seq [2 |> box]
                lexer      = fun buf -> Lexer_simple_braces.Lexer(buf):>ILexer  
            })

        (41,
            {
                tables     = Tables_simple_braces.tables
                actionsMap = GNESCC.Actions_simple_braces.ruleToAction
                path       = "\claret\\braces_1\\test_simple_braces_3.yrd.in"
                rightValue = seq [3 |> box]
                lexer      = fun buf -> Lexer_simple_braces.Lexer(buf):>ILexer  
            })

        (42,
            {
                tables     = Tables_simple_braces_2.tables
                actionsMap = GNESCC.Actions_simple_braces_2.ruleToAction
                path       = "\claret\\braces_2\\test_simple_braces_2_1.yrd.in"
                rightValue = seq [1 |> box]
                lexer      = fun buf -> Lexer_simple_braces_2.Lexer(buf):>ILexer  
            })

        (43,
            {
                tables     = Tables_simple_braces_2.tables
                actionsMap = GNESCC.Actions_simple_braces_2.ruleToAction
                path       = "\claret\\braces_2\\test_simple_braces_2_2.yrd.in"
                rightValue = seq [2 |> box]
                lexer      = fun buf -> Lexer_simple_braces_2.Lexer(buf):>ILexer  
            })

        (44,
            {
                tables     = Tables_simple_braces_2.tables
                actionsMap = GNESCC.Actions_simple_braces_2.ruleToAction
                path       = "\claret\\braces_2\\test_simple_braces_2_3.yrd.in"
                rightValue = seq [3 |> box]
                lexer      = fun buf -> Lexer_simple_braces_2.Lexer(buf):>ILexer  
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

let runMain path tables actions lexerF =
    let buf = run_common path 
    let l = lexerF buf
    let parseRes(*,cache,cc*) = 
        let ti = new TableInterpreter(tables)
        ti.Run l
    parseRes
//    let res  = 
//        match parseRes with
//        | PSuccess (forest) -> 
//            Seq.map (fun tree -> ASTInterpretator.interp actions cache tree) forest    
//            |> Seq.filter (function | Success _ -> true | _ -> false)
//            |> Seq.map (function | Success x -> x | _ -> failwith "Incorrect filter")
//            |> TSuccess
//        | PError (pos) -> 
//            TError((l:>ILexer).Get(pos) 
//            |> fun x -> (String.concat " " [pos.ToString(); string x.tag]))
//
//    res,cache,cc
    

let run path tables actions lexerF  = Seq.empty
//    match runMain path tables actions lexerF with
//    | (TSuccess(r),_,_) -> r
//    | _                 -> Seq.empty

let eRun path tables actions lexerF = Seq.empty
//    match runMain path tables actions lexerF with
//    | (TError(r),_,_) -> 
//    //#if debug
//        printf "\nError: %A \n" r
//    //#endif
//        Seq.singleton r
//    | _               -> Seq.empty

let pRun path tables actions lexerF = 0
//    match runMain path tables actions lexerF with
//    | (_,_,cc) -> cc

type GNESCCPerformanceTests () =
    static member Test1 = 
        let test = tests.[1] 
        fun x -> pRun x test.tables test.actionsMap
        , testPath + "/test_alt_in_cls/" + performanceFolder

    static member Test2 = 
        let test = tests.[30] 
        fun x -> pRun x test.tables test.actionsMap
        , testPath + "/test_summator_1/" + performanceFolder

     
[<TestFixture>]
type ``GNESCC parse error position tests`` () =

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


[<TestFixture>]
type ``GNESCC core tests`` () =
    [<Test>] 
    member test.``Alt in closure test 1`` () =
        let test = tests.[1]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer     
        Assert.AreEqual(test.rightValue,res) 

    [<Test>] 
    member test.``Alt in closure test 2`` () =
        let test = tests.[2]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt in closure test 3`` () =
        let test = tests.[3]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt in closure test 4`` () =
        let test = tests.[4]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt in closure test 5`` () =
        let test = tests.[5]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt in closure test 6`` () =
        let test = tests.[6]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt in closure test 7`` () =
        let test = tests.[7]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt test 1`` () =
        let test = tests.[8]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Alt test 2`` () =
        let test = tests.[9]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Seq test 1`` () =
        let test = tests.[10]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure with tail test 1`` () =
        let test = tests.[11]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure with tail test 2`` () =
        let test = tests.[12]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure with head test 1`` () =
        let test = tests.[13]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure with head test 2`` () =
        let test = tests.[14]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure test 1`` () =
        let test = tests.[15]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure test 2`` () =
        let test = tests.[16]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Closure test 3`` () =
        let test = tests.[17]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Arithm glr test 2`` () =
        let test = tests.[19]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Arithm glr test 3`` () =
        let test = tests.[20]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Arithm glr test 4`` () =
        let test = tests.[21]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Arithm glr test 5`` () =
        let test = tests.[22]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Arithm glr test 1`` () =
        let test = tests.[18]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``L attribute test 1`` () =
        let test = tests.[23]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Simple checker test 1`` () =
        let test = tests.[24]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Checker in glr arithmetic test 1`` () =
        let test = tests.[26]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Checker in glr arithmetic test 2`` () =
        let test = tests.[27]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Checker in glr arithmetic test 3`` () =
        let test = tests.[28]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Checker in glr arithmetic test 4`` () =
        let test = tests.[29]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Simple checker test 2`` () =
        let test = tests.[25]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Smmator_1 1`` () =
        let test = tests.[30]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Summator_1 2`` () =
        let test = tests.[31]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Summator_1 3`` () =
        let test = tests.[32]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Option 1`` () =
        let test = tests.[33]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``Option 2`` () =
        let test = tests.[333]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)
    
    [<Test>] 
    member test.``Reduce reduce conflict 1`` () =
        let test = tests.[38]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``claret b1 1`` () =
        let test = tests.[39]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``claret b1 2`` () =
        let test = tests.[40]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``claret b1 3`` () =
        let test = tests.[41]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``claret b2 1`` () =
        let test = tests.[42]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``claret b2 2`` () =
        let test = tests.[43]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)

    [<Test>] 
    member test.``claret b2 3`` () =
        let test = tests.[44]
        let res = run (testPath + test.path) test.tables test.actionsMap test.lexer
        Assert.AreEqual(test.rightValue,res)