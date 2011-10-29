//  Copyright 2010-2011 Jake Kirilenko
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
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

#light
module Yard.Frontends.YardFrontend.Main

open Microsoft.FSharp.Text.Lexing
module Lexer = Yard.Frontends.YardFrontend.GrammarLexer

let private run_common path = 
    let content = System.IO.File.ReadAllText(path)
    Lexer.currentFileContent := content;
    let reader = new System.IO.StringReader(content) in
    LexBuffer<_>.FromTextReader reader

//let private run path =
//    let buf = run_common path in 
//    let example = seq {
//                       while not buf.IsPastEndOfStream do
//                             yield Lexer.main buf  
//                      } in
//    Seq.iter (printfn "%A ") example


let ParseFile path = 
    let buf = run_common path
    GrammarParser.currentFilename := path
    GrammarParser.file Lexer.main buf
    
let ParseString string = None
    //GrammarParser.file Lexer.main buf