// Driver.fs contains main functions for test user application.
//
//  Copyright 2009, 2010, 2011 Semen Grigorev <rsdpisuy@gmail.com>
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

open Microsoft.FSharp.Text.Lexing
open Yard.Generators.GNESCCGenerator
open Yard.Generators.GNESCCGenerator.Tables_seq
open Microsoft.FSharp.Text.Lexing


//path -- path to input file
let run path =
    //Create lexer
    let content = System.IO.File.ReadAllText(path)
    let reader = new System.IO.StringReader(content)    
    let buf = LexBuffer<_>.FromTextReader reader
    let l = Lexer_seq.Lexer(buf)

    //Create tables
    let tables = tables
    
    //Run parser
    // forest -- dirivation forest    
    let forest = 
        let ti = new TableInterpreter(tables)
        ti.Run l

    let result =
        //run forest interpretation (action code calculation)
        let r = 
            Seq.map 
                (ASTInterpretator.interp GNESCC.Actions.ruleToAction GNESCC.Regexp.ruleToRegex)
                forest
        r
                
    printfn "Result %A\n" result
    
do 
    run @"..\..\..\..\Tests\GNESCC\test_arithm_glr\test_arithm_glr_2.yrd.in"
    |> ignore
    System.Console.ReadLine() |> ignore