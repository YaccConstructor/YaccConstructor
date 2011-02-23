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

module m1

open Microsoft.FSharp.Text.Lexing
open Yard.Generators.RACCGenerator
open Yard.Generators.RACCGenerator.Tables
//UserLexer -- your lexer
module Lexer = UserLexer

//path -- path to input file
let run path =
    //Create lexer
    let content = System.IO.File.ReadAllText(path)
    let reader = new System.IO.StringReader(content)    
    let buf = LexBuffer<_>.FromTextReader reader
    let l = UserLexer.Lexer(buf)

    //Create tables
    let tables =
        {
            gotoSet = gotoSet
            automataDict = autumataDict
        }
    
    //Run parser
    // trees -- dirivation forest
    // cache -- trace cache
    // cc -- some additional debug info        
    //Run parser
    // trees -- dirivation forest
    // cache -- trace cache
    // cc -- some additional debug info
    let parseRes,cache,cc = TableInterpreter.run l tables

    let forest = 
        match parseRes with
        | PSuccess (forest) -> forest
        | PError (pos) -> Set.empty
    //run forest interpretation (action code calculation)
    let res =
        Seq.map 
            (fun tree -> ASTInterpretator.interp RACC.Actions.ruleToAction cache tree) forest
            
    printf "\nResult %A\n" res
    forest

    
//let main path = run path
do 
    run @"D:\rec_ascent\trunk\Tests\RACC\test_alt_in_cls\Performance\test_9.in"
    |> ignore
    System.Console.ReadLine()
    |> ignore