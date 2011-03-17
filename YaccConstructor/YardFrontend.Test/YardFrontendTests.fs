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
open Yard.Core.IL.Definition
open NUnit.Framework

module Lexer = Yard.Frontends.YardFrontend.GrammarLexer


[<TestFixture>]
type ``YardFrontend lexer tests`` ()=    
    [<Test>]
    member test.``Lexer seq test 1`` () =
        let buf = LexBuffer<_>.FromString "+" //: NUMBER PLUS NUMBER;\n
        let lexemsSeq = seq {
            while not buf.IsPastEndOfStream do
                yield Lexer.main buf  
        } 
        let lexemsSeqCorrect = [ PLUS; EOF ] |> Seq.ofList
//        #if DEBUG
//        printfn "lexems seq:"
//        lexemsSeq |> Seq.iter (fun x -> printf "%A\n" x)
//        #endif
//        printfn "%A" (Seq.toList lexemsSeq)
        Assert.AreEqual(lexemsSeq, lexemsSeqCorrect)

[<TestFixture>]
type ``YardFrontend Parser tests`` ()=    
    [<Test>]
    member test.``Seq test 1`` () =
        let buf = LexBuffer<_>.FromString "+s: NUMBER PLUS NUMBER;\n" //
        let ilDef = GrammarParser.file Lexer.main buf
        let ilDefCorrect = {
            info = { fileName = "" };
            head = None;
            grammar = [
                
            ];
            foot = None;
        }
        #if DEBUG
        printfn "tree: %A" res
        #endif
        Assert.AreEqual(ilDef,ilDefCorrect)