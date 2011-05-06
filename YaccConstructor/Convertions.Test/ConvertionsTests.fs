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
open NUnit.Framework

[<TestFixture>]
type ``ExpandBrackets tests`` () =    
    [<Test>]
    member test.``Lexer seq test`` () =
        let ilTree = { new Definition.t<Source.t, Source.t> with
            info = { fileName = "" } 
            and head = None 
            and foot = None 
            and grammar = [
                { new Rule.t<Source.t, Source.t> with 
                    name = "s"
                    and args = []
                    and metaArgs = []
                    and _public = true
                    and body = PAlt(
                        PSeq([
                            {new elem<Source.t, Source.t> with omit=false and binding=None and checker=None and rule=PToken("NUMBER",(0,0))};
                            {new elem<Source.t, Source.t> with omit=false and binding=None and checker=None and rule=PAlt(PToken("ALT1",(0,0)),PToken("ALT2",(0,0)))}
                            {new elem<Source.t, Source.t> with omit=false and binding=None and checker=None and rule=PToken("CHUMBER",(0,0))};
                            ], None),
                        PToken("OUTER",(0,0))
                    )
                }
            ]
        }
        let ilTreeConverted = ConvertionsManager.ApplyConvertion ilTree ("ExpandBrackets")
        printfn "%A" ilTreeConverted
        let correctConverted = { new Definition.t<Source.t, Source.t> with
            info = {fileName = "";}
            and head = None;
            and grammar = [
                {
                    name = "s";
                    args = [];
                    body =
                        PAlt(
                            PSeq([
                                {omit = false; rule = PToken ("NUMBER", (0, 0)); binding = None; checker = None;};
                                {omit = false; rule = PRef (("yard_exp_brackets_1", (0, 0)),None); binding = None; checker = None;}; 
                                {omit = false; rule = PToken ("CHUMBER", (0, 0)); binding = None; checker = None;}
                            ],None),
                            PToken ("OUTER", (0, 0)));
                    _public = true;
                    metaArgs = [];
                 };
                 {
                    name = "yard_exp_brackets_1";
                    args = [];
                    body = PAlt (PToken ("ALT1", (0, 0)),PToken ("ALT2", (0, 0)));
                    _public = false;
                    metaArgs = [];
                 }
             ];
            and foot = None;
        }
        Assert.AreEqual(ilTreeConverted, correctConverted)

