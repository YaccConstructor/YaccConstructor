//  GNESCCCoreTests.fs contains unit tests for GNESCCCore
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
open Yard.Generators


let run path tables actions regexp lexerF =
    let buf =
        let content = System.IO.File.ReadAllText(path)    
        let reader = new System.IO.StringReader(content) 
        LexBuffer<_>.FromTextReader reader 

    let l = lexerF buf
    let parseRes = 
        let ti = new TableInterpreter(tables)
        ti.Run l
              
    Seq.map (fun tree -> ASTInterpretator.interp actions regexp tree) parseRes    
    |> Seq.filter (function | Success _ -> true | _ -> false)
    |> Seq.map (function | Success x -> x | _ -> failwith "Incorrect filter")
    |> List.ofSeq


[<TestFixture>]
type ``GNESCC core tests`` () =
    [<Test>]
    member test.checker_on_glr_1() =
        let tables     = GNESCCGenerator.Tables_checker_on_glr.tables
        let regexp     = GNESCC.Regexp_checker_on_glr.ruleToRegex
        let actionsMap = GNESCC.Actions_checker_on_glr.ruleToAction
        let rightValue = [3 |> box]
        let lexer      = fun buf -> Lexer_checker_on_glr.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/action_code/checkers/checker_on_glr/checker_on_glr_1.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.checker_on_glr_2() =
        let tables     = GNESCCGenerator.Tables_checker_on_glr.tables
        let regexp     = GNESCC.Regexp_checker_on_glr.ruleToRegex
        let actionsMap = GNESCC.Actions_checker_on_glr.ruleToAction
        let rightValue = [10 |> box]
        let lexer      = fun buf -> Lexer_checker_on_glr.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/action_code/checkers/checker_on_glr/checker_on_glr_2.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.checker_on_glr_3() =
        let tables     = GNESCCGenerator.Tables_checker_on_glr.tables
        let regexp     = GNESCC.Regexp_checker_on_glr.ruleToRegex
        let actionsMap = GNESCC.Actions_checker_on_glr.ruleToAction
        let rightValue = [11 |> box]
        let lexer      = fun buf -> Lexer_checker_on_glr.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/action_code/checkers/checker_on_glr/checker_on_glr_3.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.checker_on_glr_4() =
        let tables     = GNESCCGenerator.Tables_checker_on_glr.tables
        let regexp     = GNESCC.Regexp_checker_on_glr.ruleToRegex
        let actionsMap = GNESCC.Actions_checker_on_glr.ruleToAction
        let rightValue = [40 |> box]
        let lexer      = fun buf -> Lexer_checker_on_glr.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/action_code/checkers/checker_on_glr/checker_on_glr_4.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.simple_checker_1() =
        let tables     = GNESCCGenerator.Tables_simple_checker.tables
        let regexp     = GNESCC.Regexp_simple_checker.ruleToRegex
        let actionsMap = GNESCC.Actions_simple_checker.ruleToAction
        let rightValue = [7 |> box]
        let lexer      = fun buf -> Lexer_simple_checker.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/action_code/checkers/simple_checker/simple_checker_1.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.simple_checker_2() =
        let tables     = GNESCCGenerator.Tables_simple_checker.tables
        let regexp     = GNESCC.Regexp_simple_checker.ruleToRegex
        let actionsMap = GNESCC.Actions_simple_checker.ruleToAction
        let rightValue = [12 |> box; 12 |> box]
        let lexer      = fun buf -> Lexer_simple_checker.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/action_code/checkers/simple_checker/simple_checker_2.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.l_attr_1() =
        let tables     = GNESCCGenerator.Tables_l_attr.tables
        let regexp     = GNESCC.Regexp_l_attr.ruleToRegex
        let actionsMap = GNESCC.Actions_l_attr.ruleToAction
        let rightValue = [3 |> box]
        let lexer      = fun buf -> Lexer_l_attr.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/action_code/l_attrs/l_attr/l_attr_1.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.arithm_glr_1() =
        let tables     = GNESCCGenerator.Tables_arithm_glr.tables
        let regexp     = GNESCC.Regexp_arithm_glr.ruleToRegex
        let actionsMap = GNESCC.Actions_arithm_glr.ruleToAction
        let rightValue = [1 |> box]
        let lexer      = fun buf -> Lexer_arithm_glr.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/glr/arithm_glr/arithm_glr_1.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.arithm_glr_2() =
        let tables     = GNESCCGenerator.Tables_arithm_glr.tables
        let regexp     = GNESCC.Regexp_arithm_glr.ruleToRegex
        let actionsMap = GNESCC.Actions_arithm_glr.ruleToAction
        let rightValue = [3 |> box]
        let lexer      = fun buf -> Lexer_arithm_glr.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/glr/arithm_glr/arithm_glr_2.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.arithm_glr_3() =
        let tables     = GNESCCGenerator.Tables_arithm_glr.tables
        let regexp     = GNESCC.Regexp_arithm_glr.ruleToRegex
        let actionsMap = GNESCC.Actions_arithm_glr.ruleToAction
        let rightValue = seq [6 |> box]
        let lexer      = fun buf -> Lexer_arithm_glr.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/glr/arithm_glr/arithm_glr_3.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.arithm_glr_4() =
        let tables     = GNESCCGenerator.Tables_arithm_glr.tables
        let regexp     = GNESCC.Regexp_arithm_glr.ruleToRegex
        let actionsMap = GNESCC.Actions_arithm_glr.ruleToAction
        let rightValue = [2 |> box]
        let lexer      = fun buf -> Lexer_arithm_glr.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/glr/arithm_glr/arithm_glr_4.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.arithm_glr_5() =
        let tables     = GNESCCGenerator.Tables_arithm_glr.tables
        let regexp     = GNESCC.Regexp_arithm_glr.ruleToRegex
        let actionsMap = GNESCC.Actions_arithm_glr.ruleToAction
        let rightValue = [5|>box]
        let lexer      = fun buf -> Lexer_arithm_glr.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/glr/arithm_glr/arithm_glr_5.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.alt_in_cls_1() =
        let tables     = GNESCCGenerator.Tables_alt_in_cls.tables
        let regexp     = GNESCC.Regexp_alt_in_cls.ruleToRegex
        let actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
        let rightValue = seq ["-" |> box]
        let lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_1.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.alt_in_cls_2() =
        let tables     = GNESCCGenerator.Tables_alt_in_cls.tables
        let regexp     = GNESCC.Regexp_alt_in_cls.ruleToRegex
        let actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
        let rightValue = ["-;-" |> box]
        let lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_2.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.alt_in_cls_3() =
        let tables     = GNESCCGenerator.Tables_alt_in_cls.tables
        let regexp     = GNESCC.Regexp_alt_in_cls.ruleToRegex
        let actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
        let rightValue = ["+" |> box]
        let lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_3.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.alt_in_cls_4() =
        let tables     = GNESCCGenerator.Tables_alt_in_cls.tables
        let regexp     = GNESCC.Regexp_alt_in_cls.ruleToRegex
        let actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
        let rightValue = ["+;+" |> box]
        let lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_4.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.alt_in_cls_5() =
        let tables     = GNESCCGenerator.Tables_alt_in_cls.tables
        let regexp     = GNESCC.Regexp_alt_in_cls.ruleToRegex
        let actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
        let rightValue = ["-;+" |> box]
        let lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_5.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.alt_in_cls_6() =
        let tables     = GNESCCGenerator.Tables_alt_in_cls.tables
        let regexp     = GNESCC.Regexp_alt_in_cls.ruleToRegex
        let actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
        let rightValue = ["+;-" |> box]
        let lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_6.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.alt_in_cls_7() =
        let tables     = GNESCCGenerator.Tables_alt_in_cls.tables
        let regexp     = GNESCC.Regexp_alt_in_cls.ruleToRegex
        let actionsMap = GNESCC.Actions_alt_in_cls.ruleToAction
        let rightValue = ["-;+;+;-;+;-;-" |> box]
        let lexer      = fun buf -> Lexer_alt_in_cls.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/complex/alt_in_cls/alt_in_cls_7.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.cls_with_head_1() =
        let tables     = GNESCCGenerator.Tables_cls_with_head.tables
        let regexp     = GNESCC.Regexp_cls_with_head.ruleToRegex
        let actionsMap = GNESCC.Actions_cls_with_head.ruleToAction
        let rightValue = ["Head minus" |> box]
        let lexer      = fun buf -> Lexer_cls_with_head.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/complex/cls_with_head/cls_with_head_1.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.cls_with_head_2() =
        let tables     = GNESCCGenerator.Tables_cls_with_head.tables
        let regexp     = GNESCC.Regexp_cls_with_head.ruleToRegex
        let actionsMap = GNESCC.Actions_cls_with_head.ruleToAction
        let rightValue = ["Head minus;list minus" |> box]
        let lexer      = fun buf -> Lexer_cls_with_head.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/complex/cls_with_head/cls_with_head_2.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.cls_with_tail_1() =
        let tables     = GNESCCGenerator.Tables_cls_with_tail.tables
        let regexp     = GNESCC.Regexp_cls_with_tail.ruleToRegex
        let actionsMap = GNESCC.Actions_cls_with_tail.ruleToAction
        let rightValue = ["tail minus" |> box]
        let lexer      = fun buf -> Lexer_cls_with_tail.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/complex/cls_with_tail/cls_with_tail_1.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.cls_with_tail_2() =
        let tables     = GNESCCGenerator.Tables_cls_with_tail.tables
        let regexp     = GNESCC.Regexp_cls_with_tail.ruleToRegex
        let actionsMap = GNESCC.Actions_cls_with_tail.ruleToAction
        let rightValue = ["list minus;tail minus" |> box]
        let lexer      = fun buf -> Lexer_cls_with_tail.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/complex/cls_with_tail/cls_with_tail_2.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.seq_nonterms_1() =
        let tables     = GNESCCGenerator.Tables_seq_nonterms.tables
        let regexp     = GNESCC.Regexp_seq_nonterms.ruleToRegex
        let actionsMap = GNESCC.Actions_seq_nonterms.ruleToAction
        let rightValue = [3.0 |> box]
        let lexer      = fun buf -> Lexer_seq_nonterms.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/complex/seq_nonterms/seq_nonterms_1.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.alt_1() =
        let tables     = GNESCCGenerator.Tables_alt.tables
        let regexp     = GNESCC.Regexp_alt.ruleToRegex
        let actionsMap = GNESCC.Actions_alt.ruleToAction
        let rightValue = ["Detected: +" |> box]
        let lexer      = fun buf -> Lexer_alt.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/simple/alt/alt_1.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.alt_2() =
        let tables     = GNESCCGenerator.Tables_alt.tables
        let regexp     = GNESCC.Regexp_alt.ruleToRegex
        let actionsMap = GNESCC.Actions_alt.ruleToAction
        let rightValue = ["Detected: *" |> box]
        let lexer      = fun buf -> Lexer_alt.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/simple/alt/alt_2.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.cls_1() =
        let tables     = GNESCCGenerator.Tables_cls.tables
        let regexp     = GNESCC.Regexp_cls.ruleToRegex
        let actionsMap = GNESCC.Actions_cls.ruleToAction
        let rightValue = ["" |> box]
        let lexer      = fun buf -> Lexer_cls.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/simple/cls/cls_1.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.cls_2() =
        let tables     = GNESCCGenerator.Tables_cls.tables
        let regexp     = GNESCC.Regexp_cls.ruleToRegex
        let actionsMap = GNESCC.Actions_cls.ruleToAction
        let rightValue = ["*" |> box]
        let lexer      = fun buf -> Lexer_cls.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/simple/cls/cls_2.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.cls_3() =
        let tables     = GNESCCGenerator.Tables_cls.tables
        let regexp     = GNESCC.Regexp_cls.ruleToRegex
        let actionsMap = GNESCC.Actions_cls.ruleToAction
        let rightValue = ["*;*" |> box]
        let lexer      = fun buf -> Lexer_cls.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/simple/cls/cls_3.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.opt_1() =
        let tables     = GNESCCGenerator.Tables_opt.tables
        let regexp     = GNESCC.Regexp_opt.ruleToRegex
        let actionsMap = GNESCC.Actions_opt.ruleToAction
        let rightValue = ["1 +" |> box]
        let lexer      = fun buf -> Lexer_opt.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/simple/opt/opt_1.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.opt_2() =
        let tables     = GNESCCGenerator.Tables_opt.tables
        let regexp     = GNESCC.Regexp_opt.ruleToRegex
        let actionsMap = GNESCC.Actions_opt.ruleToAction
        let rightValue = ["1" |> box]
        let lexer      = fun buf -> Lexer_opt.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/simple/opt/opt_2.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

    [<Test>]
    member test.seq_1() =
        let tables     = GNESCCGenerator.Tables_seq.tables
        let regexp     = GNESCC.Regexp_seq.ruleToRegex
        let actionsMap = GNESCC.Actions_seq.ruleToAction
        let rightValue = [3.0 |> box]
        let lexer      = fun buf -> Lexer_seq.Lexer(buf):>ILexer
        let res = run @"../../../../Tests/GNESCC/regexp/simple/seq/seq_1.yrd.in" tables actionsMap regexp lexer
        Assert.AreEqual(rightValue,res)

