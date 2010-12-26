// Driver.fs
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
module Lexer = UserLexer

let run_common path = 
    let content = System.IO.File.ReadAllText(path)    
    let reader = new System.IO.StringReader(content) in
    LexBuffer<_>.FromTextReader reader

type t<'buf,'a,'b> = 
    {
        buf   : 'buf
        lexer : ILexer<'a>
    }

let run path =
    let buf = run_common path 
    let l = UserLexer.Lexer(buf)
    let tables =
        {
            gotoSet = gotoSet
            automataDict = autumataDict
            //items = items
        }
    
    TableInterpreter.actions := RACC.Actions.ruleToAction
    let trees,cache,result,cc = TableInterpreter.run l tables
    printfn "\n %A \n" result
    let r =         
        Seq.map (fun tree -> ASTInterpretator.interp RACC.Actions.ruleToAction cache tree) trees        
    printf "\nResult %A\n" r
    trees        
    
let main path = run path

