//  YardFrontendTests.fs contains unuit test for Yard frontend
//
//  Copyright 2010 Anastasia Nishnevich <Anastasia.Nishnevich@gmail.com>
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


module YardFrontendTester

open Microsoft.FSharp.Text.Lexing
open Yard.Frontends.YardFrontend
open Yard.Frontends.YardFrontend.GrammarParser
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open NUnit.Framework

module Lexer = Yard.Frontends.YardFrontend.GrammarLexer

let lexerTest str lexemsListCorrect =
    let buf = LexBuffer<_>.FromString str
    Lexer.currentFileContent := str
    let lexemsSeq = seq {
        while not buf.IsPastEndOfStream do
            yield Lexer.main buf  
    } 
    let lexemsList = Seq.toList lexemsSeq

    printfn "%A" lexemsList

    Assert.AreEqual(lexemsList, lexemsListCorrect)

let preprocessorTest path (expectedIL : t<Source.t,Source.t>) =
    let currentIL = {Main.ParseFile path with info = {fileName =""}}

    printfn "ilDef = %A" currentIL
    printfn "ilDefCorrect = %A" expectedIL

    Assert.AreEqual(currentIL, expectedIL)

let parserTest str (ilDefCorrect: t<Source.t,Source.t>) =
    let buf = LexBuffer<_>.FromString str
    Lexer.currentFileContent := str
    let ilDef = {GrammarParser.file Lexer.main buf with info = {fileName =""}}

    printfn "ilDef = %A" ilDef
    printfn "ilDefCorrect = %A" ilDefCorrect

    Assert.AreEqual(ilDef, ilDefCorrect)

let completeTest str lexemsListCorrect ilDefCorrect = 
    lexerTest str lexemsListCorrect
    parserTest str ilDefCorrect

[<TestFixture>]
type ``YardFrontend lexer tests`` () =    
    [<Test>]
    member test.``Lexer seq test`` () =
        lexerTest 
            "+s: NUMBER PLUS NUMBER;"
            [PLUS; LIDENT ("s", (1, 2)); COLON; UIDENT ("NUMBER", (4, 10))
            ; UIDENT ("PLUS", (11, 15)); UIDENT ("NUMBER", (16, 22)); SEMICOLON; EOF]

    [<Test>]
    member test.``Lexer cls test`` () =
        lexerTest 
            "+s: (MINUS|PLUS)*;"
            [PLUS; LIDENT ("s", (1, 2)); COLON; LPAREN (Range (Lexing.Position.Empty,Lexing.Position.Empty)); UIDENT ("MINUS", (5, 10)); BAR
            ; UIDENT ("PLUS", (11, 15)); RPAREN (Range (Lexing.Position.Empty,Lexing.Position.Empty)); STAR; SEMICOLON; EOF]

    [<Test>]            
    member test.``Include test`` () =
        lexerTest @"
include ""test_included.yrd""
+s:PLUS;"
            [INCLUDE; STRING ("test_included.yrd", (11, 28)); PLUS; LIDENT ("s", (32, 33))
            ; COLON; UIDENT ("PLUS", (34, 38)); SEMICOLON; EOF]

[<TestFixture>]
type ``Yard frontend preprocessor tests`` () =
    let basePath = "../../../../Tests/YardFrontend/Preprocessor"
    let cp file = System.IO.Path.Combine(basePath,file)
    [<Test>]
    member test.noUserDefs () =
        let expected = 
            {
                info = {fileName =""}
                head = None
                grammar = [{name = "e"
                            args = []
                            body = PSeq ([{omit = false
                                           rule = PToken ("R", (28, 29))
                                           binding = None
                                           checker = None}],None)
                            _public = true
                            metaArgs = []}]
                foot = None}
        preprocessorTest (cp "test_0.yrd") expected

    [<Test>]
    member test.if_endif () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = "e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken ("N", (16, 17))
                                        binding = None
                                        checker = None}; {omit = false
                                                          rule = PToken ("R", (28, 29))
                                                          binding = None
                                                          checker = None}],None)
                         _public = true
                         metaArgs = []}]
             foot = None}
        preprocessorTest ((cp "test_0.yrd")+"%ora") expected

    [<Test>]
    member test.``if_else_end. No user defs.`` () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = "e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken ("R", (29, 30))
                                        binding = None
                                        checker = None}],None)
                         _public = true
                         metaArgs = []}]
             foot = None}
        preprocessorTest (cp "test_1.yrd") expected

    [<Test>]
    member test.``if_else_end. User defs.`` () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = "e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken ("N", (17, 18))
                                        binding = None
                                        checker = None}],None)
                         _public = true
                         metaArgs = []}]
             foot = None}
        preprocessorTest ((cp "test_1.yrd")+"%ora") expected

    [<Test>]
    member test.``Inner if. No user defs.`` () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = "e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken ("Q", (57, 58))
                                        binding = None
                                        checker = None}],None)
                         _public = true
                         metaArgs = []}]
             foot = None}
        preprocessorTest (cp "test_2.yrd") expected

    [<Test>]
    member test.``Inner if. Inner user defs.`` () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = "e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken ("Q", (57, 58))
                                        binding = None
                                        checker = None}],None)
                         _public = true
                         metaArgs = []}]
             foot = None}
        preprocessorTest ((cp "test_2.yrd")+"%x") expected

    [<Test>]
    member test.``Inner if. Full user defs.`` () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = "e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken ("N", (16, 17))
                                        binding = None
                                        checker = None}; {omit = false
                                                          rule = PToken ("G", (27, 28))
                                                          binding = None
                                                          checker = None}],None)
                         _public = true
                         metaArgs = []}]
             foot = None}
        preprocessorTest ((cp "test_2.yrd")+"%ora;x") expected

    [<Test>]
    member test.``Inner if. Top user defs.`` () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = "e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken ("N", (16, 17))
                                        binding = None
                                        checker = None}; {omit = false
                                                          rule = PToken ("H", (38, 39))
                                                          binding = None
                                                          checker = None}],None)
                         _public = true
                         metaArgs = []}]
             foot = None}
        preprocessorTest ((cp "test_2.yrd")+"%ora") expected

    [<Test>]
    member test.``elif with no defs.`` () =
        let expected = 
            {
                info = {fileName =""}
                head = None
                grammar = [{name = "s"
                            args = []
                            body = PSeq ([{omit = false
                                           rule = PToken ("C", (40, 41))
                                           binding = None
                                           checker = None}],None)
                            _public = true
                            metaArgs = []}]
                foot = None}
        preprocessorTest (cp "test_3.yrd") expected

    [<Test>]
    member test.``elif with first def.`` () =
        let expected = 
            {
                info = {fileName =""}
                head = None
                grammar = [{name = "s"
                            args = []
                            body = PSeq ([{omit = false
                                           rule = PToken ("A", (15, 16))
                                           binding = None
                                           checker = None}],None)
                            _public = true
                            metaArgs = []}]
                foot = None}
        preprocessorTest ((cp "test_3.yrd")+"%first") expected

    [<Test>]
    member test.``elif with second def.`` () =
        let expected = 
            {
                info = {fileName =""}
                head = None
                grammar = [{name = "s"
                            args = []
                            body = PSeq ([{omit = false
                                           rule = PToken ("B", (31, 32))
                                           binding = None
                                           checker = None}],None)
                            _public = true
                            metaArgs = []}]
                foot = None}
        preprocessorTest ((cp "test_3.yrd")+"%second") expected

    [<Test>]
    member test.``elif with both defs.`` () =
        let expected = 
            {
                info = {fileName =""}
                head = None
                grammar = [{name = "s"
                            args = []
                            body = PSeq ([{omit = false
                                           rule = PToken ("A", (15, 16))
                                           binding = None
                                           checker = None}; {omit = false
                                                             rule = PToken ("B", (31, 32))
                                                             binding = None
                                                             checker = None}],None)
                            _public = true
                            metaArgs = []}]
                foot = None}
        preprocessorTest ((cp "test_3.yrd")+"%first;second") expected

[<TestFixture>]
type ``YardFrontend Parser tests`` () =    
    [<Test>]
    member test.``Seq test`` () =
        parserTest
            "+s: NUMBER PLUS NUMBER;" 
            { info = {fileName = ""} 
              head = None  
              grammar = 
                    [{ 
                        name = "s"
                        args = []
                        body =
                            PSeq(
                                [{ 
                                    omit = false
                                    rule = PToken ("NUMBER", (4, 10))
                                    binding = None
                                    checker = None
                                }; { 
                                    omit = false
                                    rule = PToken ("PLUS", (11, 15))
                                    binding = None
                                    checker = None
                                }; {
                                    omit = false
                                    rule = PToken ("NUMBER", (16, 22))
                                    binding = None
                                    checker = None
                                }],
                                None)
                        _public = true
                        metaArgs = []
                    }] 
              foot = None
            } 
                
[<TestFixture>]
type ``YardFrontend Complete tests`` () =    
    [<Test>]
    member test.``L_attr test`` () =
        completeTest @"
{
let value x = (x:>Lexeme<string>).value
}
+s: <res:int> = e[1] {res};
e[i]: n=NUMBER {(value n |> int) + i};"
            [ACTION (@"
let value x = (x:>Lexeme<string>).value
", 
                (3, 46)); PLUS;
                LIDENT ("s", (50, 51)); COLON; PATTERN ("res:int", (54, 61)); EQUAL;
                LIDENT ("e", (65, 66)); PARAM ("1", (67, 68)); ACTION ("res", (71, 74));
                SEMICOLON; LIDENT ("e", (78, 79)); PARAM ("i", (80, 81)); COLON;
                LIDENT ("n", (84, 85)); EQUAL; UIDENT ("NUMBER", (86, 92));
                ACTION ("(value n |> int) + i", (94, 114)); SEMICOLON; EOF]
            {
             info = { fileName = ""; }
             head = Some ("\r\nlet value x = (x:>Lexeme<string>).value\r\n", (3, 46))
             grammar = 
                    [{ 
                        name = "s"
                        args = []
                        body = 
                            PSeq (
                                [{
                                    omit = false
                                    rule = PRef (("e", (65, 66)),Some ("1", (67, 68)))
                                    binding = Some ("res:int", (54, 61))
                                    checker = None
                                }],
                                Some ("res", (71, 74)))
                        _public = true
                        metaArgs = []
                      }; { 
                        name = "e"
                        args = [("i", (80, 81))]
                        body = 
                            PSeq (
                                [{
                                    omit = false
                                    rule = PToken ("NUMBER", (86, 92))
                                    binding = Some ("n", (84, 85))
                                    checker = None
                                }],
                                Some ("(value n |> int) + i", (94, 114)))
                        _public = false
                        metaArgs = []
                    }]
             foot = None
            }