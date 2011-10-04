// GNESCCGeneratorTests.fs contains unit tests for GNESCCGenerator
//
//  Copyright 2011 Semen Grigorev <rsdpisuy@gmail.com>
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

module GNESCGeneratorTests

//open Microsoft.FSharp.Text.Lexing
open Yard.Generators.GNESCCGenerator
open NUnit.Framework
open Yard.Core.IL.Definition
open Yard.Core.IL.Production


[<TestFixture>]
type ``GNESCC generator test`` () =

    [<Test>] 
    member test.``Start rule not found`` () =
        let grammar =
            {info = {fileName = "test.yrd";};
             head = None;
             grammar =
              [{name = "s";
                args = [];
                body =
                 PSeq ([{omit = false;
                         rule = PToken ("N", (3, 4));
                         binding = None;
                         checker = None;}; {omit = false;
                                            rule = PToken ("P", (5, 6));
                                            binding = None;
                                            checker = None;}; {omit = false;
                                                               rule = PToken ("N", (7, 8));
                                                               binding = None;
                                                               checker = None;}],None);
                _public = false;
                metaArgs = [];}];
             foot = None;}
        let gen = new GNESCCGenerator()
        try
            let res = gen.Generate grammar
            ()
        with 
        | :? Yard.Generators.GNESCCGenerator.StartRuleNotFound -> 
            Assert.IsTrue (true)
        | e -> Assert.Fail ("Incorrect exceptio. Expected StartRuleNotFound, but given " + e.Message)
        
