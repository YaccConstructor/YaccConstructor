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

