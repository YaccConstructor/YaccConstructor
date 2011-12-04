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

module Yard.Frontends.YardFrontend.Main

open Microsoft.FSharp.Text.Lexing
module Lexer = Yard.Frontends.YardFrontend.GrammarLexer

let private run_common path = 
    let content = System.IO.File.ReadAllText(path)
    Lexer.currentFileContent := content;
    let reader = new System.IO.StringReader(content) in
    LexBuffer<_>.FromTextReader reader

let ParseFile path = 
    let buf = run_common path    
    GrammarParser.currentFilename := path
    let posTo2D pos =
        let source = System.IO.File.ReadAllText path
        source.ToCharArray(0, min (pos+1) (source.Length))
        |> Array.fold
            (fun (col,row) -> function
                | '\n' -> (col+1, 0)
                | '\r' -> (col, row)
                | _ -> (col, row+1)
            )
            (1,0)
    try
        GrammarParser.file Lexer.main buf
    with
    | Lexer.Lexical_error (msg, pos) ->
        let pos2D = posTo2D pos
        failwith <| sprintf "Lexical error in line %d position %d: %s" (fst pos2D) (snd pos2D) msg
    (*| GrammarParser.Parse_error msg ->
        let pos2D = posTo2D pos
        failwith <| sprintf "Lexical error in line %d position %d: %s" (fst pos2D) (snd pos2D) msg*)
    
let LexString string =
    Lexer.currentFileContent := string;
    let reader = new System.IO.StringReader(string)
    let buf = LexBuffer<_>.FromTextReader reader
    seq {
            while not buf.IsPastEndOfStream do
               yield Lexer.main buf  
        }
