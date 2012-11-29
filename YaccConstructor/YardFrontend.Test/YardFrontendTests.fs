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

let dummyPos s = new Source.t(s)

let equalTokens x y =
    let getCtor arg = fst <| Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(arg, typeof<Token>)
    getCtor x = getCtor y

let lexerTest str lexemsListCorrect =
    let buf = LexBuffer<_>.FromString str
    Lexer.currentFile := ""
    Lexer.currentFileContent := str
    let lexemsSeq = seq {
        while not buf.IsPastEndOfStream do
            yield Lexer.main buf  
    } 
    let lexemsList = Seq.toList lexemsSeq

    printfn "%A" lexemsList
   
    printfn "%s" "*************************"

    printfn "%A" lexemsListCorrect

    let areEqual lexemsListCorrect lexemsList =
        try 
            List.map2
                (fun x y -> equalTokens x y)
                lexemsListCorrect lexemsList
            |> List.reduce (&&)
        with _ -> false
    Assert.IsTrue (areEqual lexemsListCorrect lexemsList)

let preprocessorTest path (expectedIL : t<Source.t,Source.t>) =
    let currentIL = {Main.ParseFile path with info = {fileName =""}}

    printfn "ilDef = %A" currentIL
    printfn "ilDefCorrect = %A" expectedIL

    Assert.IsTrue(Yard.Core.ILComparators.GrammarEqualsWithoutLineNumbers expectedIL.grammar currentIL.grammar)

let parserTest str (ilDefCorrect: t<Source.t,Source.t>) =
    let buf = LexBuffer<_>.FromString str
    Lexer.currentFileContent := str
    let ilDef = { Main.parse buf [||] with info = {fileName =""}}

    printfn "ilDef = %A" ilDef
    printfn "ilDefCorrect = %A" ilDefCorrect

    Assert.IsTrue(Yard.Core.ILComparators.GrammarEqualsWithoutLineNumbers ilDef.grammar ilDefCorrect.grammar)

let completeTest str lexemsListCorrect ilDefCorrect = 
    lexerTest str lexemsListCorrect
    parserTest str ilDefCorrect

let optionsTest path optionsCorrect =
    let definition = {Main.ParseFile path with info = {fileName =""}}
    let currentOptions = definition.options
    let optionsAreEq (m1:Map<Rule.t<_,_>,_>) (m2:Map<Rule.t<_,_>,_>) = 
        Assert.AreEqual (m1.Count,m2.Count)
        Assert.IsTrue (m2 |> Seq.forall2 (fun (x1:System.Collections.Generic.KeyValuePair<Rule.t<_,_>,_>) x2 
                                             -> x1.Key.name.text = x2.Key.name.text && x1.Value = x2.Value) m1)
    optionsAreEq currentOptions optionsCorrect
        
let getSource name b e = new Source.t (name, new Source.Position(b, 0, b), new Source.Position(e, 0, e), "")

[<TestFixture>]
type ``YardFrontend lexer tests`` () = 
    [<Test>]
    member test.``Lexer seq test`` () =
        lexerTest 
            "[<Start>]s: NUMBER PLUS NUMBER;"
            [START_RULE_SIGN (getSource "[<Start>]" 0 1); LIDENT (getSource "s" 1 2); COLON (getSource ":" 2 3); UIDENT (getSource "NUMBER" 4 10)
            ; UIDENT (getSource "PLUS" 11 15); UIDENT (getSource "NUMBER" 16 22); SEMICOLON (getSource ";" 22 23); EOF (getSource "" 23 23)]

    [<Test>]
    member test.``Lexer cls test`` () =
        lexerTest 
            "[<Start>]s: (MINUS|PLUS)*;"
            [START_RULE_SIGN (getSource "[<Start>]" 0 1); LIDENT (getSource "s" 1 2); COLON (getSource ":" 2 3); LPAREN (getSource "(" 4 5)
            ; UIDENT (getSource "MINUS" 5 10); BAR (getSource "|" 10 11); UIDENT (getSource "PLUS" 11 15);
            RPAREN (getSource ")" 15 16); STAR (getSource "*" 16 17); SEMICOLON (getSource ";" 17 18); EOF (getSource "" 18 18)]

    [<Test>]            
    member test.``Include test`` () =
        lexerTest @"  include ""test_included.yrd""  [<Start>]s:PLUS;"
            [INCLUDE (getSource "include" 2 9); STRING (getSource "test_included.yrd" 11 28); START_RULE_SIGN (getSource "[<Start>]" 2 3); LIDENT (getSource "s" 32 33)
            ; COLON (getSource ":" 33 34); UIDENT (getSource "PLUS" 34 38); SEMICOLON (getSource ":" 38 39); EOF (getSource ":" 39 39)]

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
                grammar = [{name = dummyPos "e"
                            args = []
                            body = PSeq ([{omit = false
                                           rule = PToken (getSource "R" 28 29)
                                           binding = None
                                           checker = None}],None,None)
                            _public = true
                            metaArgs = []}]
                foot = None
                options = Map.empty}
        preprocessorTest (cp "test_0.yrd") expected

    [<Test>]
    member test.if_endif () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = dummyPos"e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken (getSource "N" 16 17)
                                        binding = None
                                        checker = None}; {omit = false
                                                          rule = PToken (getSource "R" 28 29)
                                                          binding = None
                                                          checker = None}],None,None)
                         _public = true
                         metaArgs = []}]
             foot = None
             options = Map.empty}
        preprocessorTest ((cp "test_0.yrd")+"%ora") expected

    [<Test>]
    member test.``if_else_end. No user defs.`` () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = dummyPos"e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken (getSource "R" 29 30)
                                        binding = None
                                        checker = None}]
                                      ,None, None)
                         _public = true
                         metaArgs = []}]
             foot = None
             options = Map.empty}
        preprocessorTest (cp "test_1.yrd") expected

    [<Test>]
    member test.``if_else_end. User defs.`` () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = dummyPos"e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken (getSource "N" 17 18)
                                        binding = None
                                        checker = None}],None,None)
                         _public = true
                         metaArgs = []}]
             foot = None
             options = Map.empty}
        preprocessorTest ((cp "test_1.yrd")+"%ora") expected

    [<Test>]
    member test.``Inner if. No user defs.`` () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = dummyPos"e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken (getSource "Q" 57 58)
                                        binding = None
                                        checker = None}],None,None)
                         _public = true
                         metaArgs = []}]
             foot = None
             options = Map.empty}
        preprocessorTest (cp "test_2.yrd") expected

    [<Test>]
    member test.``Inner if. Inner user defs.`` () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = dummyPos"e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken (getSource "Q" 57 58)
                                        binding = None
                                        checker = None}],None,None)
                         _public = true
                         metaArgs = []}]
             foot = None
             options = Map.empty}
        preprocessorTest ((cp "test_2.yrd")+"%x") expected

    [<Test>]
    member test.``Inner if. Full user defs.`` () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = dummyPos"e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken (getSource "N" 16 17)
                                        binding = None
                                        checker = None}; {omit = false
                                                          rule = PToken (getSource "G" 27 28)
                                                          binding = None
                                                          checker = None}],None,None)
                         _public = true
                         metaArgs = []}]
             foot = None
             options = Map.empty}
        preprocessorTest ((cp "test_2.yrd")+"%ora;x") expected

    [<Test>]
    member test.``Inner if. Top user defs.`` () =
        let expected = 
            {info = {fileName = ""}
             head = None
             grammar = [{name = dummyPos "e"
                         args = []
                         body = PSeq ([{omit = false
                                        rule = PToken (getSource "N" 16 17)
                                        binding = None
                                        checker = None}; {omit = false
                                                          rule = PToken (getSource "H" 38 39)
                                                          binding = None
                                                          checker = None}],None,None)
                         _public = true
                         metaArgs = []}]
             foot = None
             options = Map.empty}
        preprocessorTest ((cp "test_2.yrd")+"%ora") expected

    [<Test>]
    member test.``elif with no defs.`` () =
        let expected = 
            {
                info = {fileName =""}
                head = None
                grammar = [{name = dummyPos"s"
                            args = []
                            body = PSeq ([{omit = false
                                           rule = PToken (getSource "C" 40 41)
                                           binding = None
                                           checker = None}],None,None)
                            _public = true
                            metaArgs = []}]
                foot = None
                options = Map.empty}
        preprocessorTest (cp "test_3.yrd") expected

    [<Test>]
    member test.``elif with first def.`` () =
        let expected = 
            {
                info = {fileName =""}
                head = None
                grammar = [{name = dummyPos"s"
                            args = []
                            body = PSeq ([{omit = false
                                           rule = PToken (getSource "A" 15 16)
                                           binding = None
                                           checker = None}],None,None)
                            _public = true
                            metaArgs = []}]
                foot = None
                options = Map.empty}
        preprocessorTest ((cp "test_3.yrd")+"%first") expected

    [<Test>]
    member test.``elif with second def.`` () =
        let expected = 
            {
                info = {fileName =""}
                head = None
                grammar = [{name = dummyPos"s"
                            args = []
                            body = PSeq ([{omit = false
                                           rule = PToken (getSource "B" 31 32)
                                           binding = None
                                           checker = None}],None,None)
                            _public = true
                            metaArgs = []}]
                foot = None
                options = Map.empty}
        preprocessorTest ((cp "test_3.yrd")+"%second") expected

    [<Test>]
    member test.``elif with both defs.`` () =
        let expected = 
            {
                info = {fileName =""}
                head = None
                grammar = [{name = dummyPos"s"
                            args = []
                            body = PSeq ([{omit = false
                                           rule = PToken (getSource "A" 15 16)
                                           binding = None
                                           checker = None}],None,None)
                            _public = true
                            metaArgs = []}]
                foot = None
                options = Map.empty}
        preprocessorTest ((cp "test_3.yrd")+"%first;second") expected
             
[<TestFixture>]
type ``YardFrontend Parser tests`` () =    
    [<Test>]
    member test.``Seq test`` () =
        parserTest
            "[<Start>]s: NUMBER PLUS NUMBER;" 
            { info = {fileName = ""} 
              head = None  
              grammar = 
                    [{ 
                        name = dummyPos"s"
                        args = []
                        body =
                            PSeq(
                                [{ 
                                    omit = false
                                    rule = PToken (getSource "NUMBER" 4 10)
                                    binding = None
                                    checker = None
                                }; { 
                                    omit = false
                                    rule = PToken (getSource "PLUS" 11 15)
                                    binding = None
                                    checker = None
                                }; {
                                    omit = false
                                    rule = PToken (getSource "NUMBER" 16 22)
                                    binding = None
                                    checker = None
                                }],
                                None,None)
                        _public = true
                        metaArgs = []
                    }] 
              foot = None
              options = Map.empty
            }
               

[<TestFixture>]
type ``YardFrontend options tests`` () =  
    let basePath = "../../../../Tests/YardFrontend/Options"
    let cp file = System.IO.Path.Combine(basePath,file)  

    [<Test>]
    member test.``Lexer test for options`` () =
        lexerTest 
            "[<Start>]s:  #set a = \"smth\"  A;"
            [START_RULE_SIGN (getSource "[<Start>]" 0 1); LIDENT (getSource "s" 1 2); COLON (getSource ":" 2 3); SET (getSource "#set" 5 9);
             LIDENT (getSource "a" 10 11) ; EQUAL (getSource "=" 12 13); STRING (getSource "smth" 15 19); UIDENT (getSource "A" 22 23);
             SEMICOLON (getSource ";" 23 24); EOF (getSource "" 24 24)]

    [<Test>]
    member test.``Basic options test`` () =
        let rule : Rule.t<Source.t, Source.t> = {
            Rule.name = dummyPos"s"
            Rule._public = true
            Rule.args = []
            Rule.body = PSeq ([{omit = false
                                rule = PToken (getSource "A" 22 23)
                                binding = None
                                checker = None}], None,None)
            Rule.metaArgs = []
            }
        let optionsForRule = Map.ofList [("a", "smth")]//[("dialect", "ora"), ("comment","smth")]

        optionsTest (cp "options_test_0.yrd") (Map.empty.Add (rule, optionsForRule))
    
                
[<TestFixture>]
type ``YardFrontend Complete tests`` () =    
    [<Test>]
    member test.``L_attr test`` () =
        completeTest
            "  {  let value x = (x:>Lexeme<string>).value  } \n[<Start>]s: <res:int> = e[1] {res};  e[i]: n=NUMBER {(value n |> int) + i};"
            [ACTION (getSource @"  let value x = (x:>Lexeme<string>).value  " 3 46); START_RULE_SIGN (getSource ":" 2 9);
                LIDENT (getSource "s" 50 51); COLON(getSource ":" 2 9); PATTERN (getSource "res:int" 54 61); EQUAL(getSource ":" 2 9);
                LIDENT (getSource "e" 65 66); PARAM (getSource "1" 67 68); ACTION (getSource "res" 71 74);
                SEMICOLON (getSource ":" 2 9); LIDENT (getSource "e" 78 79); PARAM (getSource "i" 80 81); COLON(getSource ":" 2 9);
                LIDENT (getSource "n" 84 85); EQUAL(getSource ":" 2 9); UIDENT (getSource "NUMBER" 86 92);
                ACTION (getSource "(value n |> int) + i" 94 114); SEMICOLON (getSource ":" 2 9); EOF(getSource ":" 2 9)]
            {
             info = { fileName = ""; }
             head = Some (getSource "  let value x = (x:>Lexeme<string>).value  " 3 46)
             grammar = 
                    [{ 
                        name = dummyPos"s"
                        args = []
                        body = 
                            PSeq (
                                [{
                                    omit = false
                                    rule = PRef ((getSource "e" 65 66),Some (getSource "1" 67 68))
                                    binding = Some (getSource "res:int" 54 61)
                                    checker = None
                                }],
                                Some (getSource "res" 71 74), None)
                        _public = true
                        metaArgs = []
                      }; { 
                        name = dummyPos"e"
                        args = [(getSource "i" 80 81)]
                        body = 
                            PSeq (
                                [{
                                    omit = false
                                    rule = PToken (getSource "NUMBER" 86 92)
                                    binding = Some (getSource "n" 84 85)
                                    checker = None
                                }],
                                Some (getSource "(value n |> int) + i" 94 114), None)
                        _public = false
                        metaArgs = []
                    }]
             foot = None
             options = Map.empty
            }
        
[<TestFixture>]
type ``Yardfrontend label tests`` () =
    let basePath = "../../../../Tests/YardFrontend/Label"
    let cp file = System.IO.Path.Combine(basePath,file)

    [<Test>]
    member test.``label test.`` () = 
        let expected = 
            {
                info = {fileName =""}
                head = None
                grammar = [{name = dummyPos"s"
                            args = []
                            body = PSeq ([{omit = false
                                           rule = PToken (getSource "A" 12 13)
                                           binding = None
                                           checker = None}],None,Some {label = "@label";
                                                         weight = None;});
                            _public = true
                            metaArgs = []}]
                foot = None
                options = Map.empty}
        preprocessorTest (cp "test_0.yrd") expected

    [<Test>]
    member test.``weight test correct input`` () =
        parserTest
            "[<Start>]s: @lbl[12.3](T);"
            {info = {fileName = "";};
            head = None;
            grammar = [{name = dummyPos"s";
                        args = [];
                        body = PSeq ([{omit = false;
                                       rule = PToken (getSource "T" 16 16);
                                       binding = None;
                                       checker = None;}],None,Some {label = "@lbl";
                                                                    weight = Some 12.3;});
                        _public = true;
                        metaArgs = [];
                      }];
            foot = None;
            options = Map.empty;
            }
    
    [<Test>]
    member test.``weight test incorrect input`` () =
        // must fail
        try
            let smth = parserTest
                        "[<Start>]s: @lbl[q](T);"
                        {info = {fileName = "";};
                        head = None;
                        grammar = [{name = dummyPos"s";
                                    args = [];
                                    body = PSeq ([{omit = false;
                                                   rule = PToken (getSource "T" 16 16);
                                                   binding = None;
                                                   checker = None;}],None,Some {label = "@lbl";
                                                                                weight = None;});
                                    _public = true;
                                    metaArgs = [];
                                  }];
                        foot = None;
                        options = Map.empty;
                        }
            Assert.IsTrue(false)
        with 
        | :? System.Exception as ex ->
            let expected = "Parse error on position (0,17) on token q: illegal weight. Number expected."
            let actual = ex.Message
            Assert.AreEqual(expected, actual)
