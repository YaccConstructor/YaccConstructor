// Driver.fs contains main functions for test user application.
//
//  Copyright 2009,2010 Semen Grigorev <rsdpisuy@gmail.com>
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

//module m1

open Microsoft.FSharp.Text.Lexing
open Yard.Generators.RACCGenerator
open Yard.Generators.RACCGenerator.Tables
open Microsoft.FSharp.Text.Lexing
//UserLexer -- your lexer
//module Lexer = UserLexer

let smallBraces = seq{yield {name = "LBRACE"; value = "("};
                      yield {name = "RBRACE"; value = ")"};
                      yield {name = "LBRACE"; value = "("};
                      yield {name = "LBRACE"; value = "("};
                      yield {name = "RBRACE"; value = ")"};
                      yield {name = "RBRACE"; value = ")"};
                      yield {name = "LBRACE"; value = "("};
                      yield {name = "RBRACE"; value = ")"};
                      yield {name = "EOF"; value = "EOF"}}

type SeqLexer(seqTok:array<_>) = 
    class
        
        (*let locbuf _ = 
            printfn "%A\n" seqTok
            Seq.toArray seqTok*)
        let mutable ind = 0
        interface ILexer<string> with
            member self.Get pos =
                seqTok.[pos-1]
            member self.IsEnd () =
                true
    //            ind <- ind + 1
      //          printfn "Ind++ = %d" ind
        //        ind >= locbuf.Length
    end

//path -- path to input file
let run path =
    //Create lexer
    //let content = System.IO.File.ReadAllText(path)
    //let reader = new System.IO.StringReader(content)    
    //let buf = LexBuffer<_>.FromTextReader reader
    //let l = UserLexer.Lexer(buf)
    let l = SeqLexer (Seq.toArray smallBraces)

    //Create tables
    let tables =
        {
            gotoSet = dict[]//gotoSet
            automataDict = autumataDict
        }
    
    //Run parser
    // trees -- dirivation forest
    // cache -- trace cache
    // cc -- some additional debug info
    let parseRes,cache,cc = 
        let ti = new TableInterpreter<string>()
        ti.Run l tables

    let result = 
        match parseRes with
        //Parse success
        | PSuccess (forest) -> 
        //run forest interpretation (action code calculation)
            printf "\nForest %A\n" forest
            Seq.map 
             (fun tree -> ASTInterpretator.interp RACC.Actions.ruleToAction cache tree)
             forest
        //Error handling
        | PError (pos) -> 
            //Error handling
            //If you create lexeme with position in stream, you can not only provide error lexeme
            // but also navigate in error position
            let errLexeme = (l :> ILexer<string>).Get(pos)
            "Incorrect input. Unexpected lexeme: " + errLexeme.name + " with value = " + errLexeme.value
            |> failwith
            
    printf "\nResult %A\n" result
    

do 
    run @"D:\rec_ascent\trunk\Tests\RACC\test_alt_in_cls\test_alt_in_cls_7.yrd.in"
    |> ignore
    System.Console.ReadLine() |> ignore