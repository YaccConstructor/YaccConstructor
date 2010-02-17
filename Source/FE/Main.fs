// Main.fs
//
// Copyright 2009 Jake Kirilenko
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light
module Yard.Core.Main
open Microsoft.FSharp.Text.Lexing
module Lexer = Yard.Core.GrammarLexer

let run_common path = 
    let content = System.IO.File.ReadAllText(path)
    Lexer.currentFileContent := content;
    let reader = new System.IO.StringReader(content) in
    LexBuffer<_>.FromTextReader reader

let run path =
    let buf = run_common path in
    let example = seq {
                       while not buf.IsPastEndOfStream do
                             yield Lexer.main buf  
                      } in
    Seq.iter (printfn "%A ") example


let ParseFile path = 
    let buf = run_common path
    let res = GrammarParser.file Lexer.main buf
#if DEBUG
    printf "%A\n" <|res
#endif    
    {res with Yard.Core.IL.Definition.info = {fileName = path}}
    
#if DEBUG
let main =
    printf "%A\n" <| ParseFile @"..\..\..\..\Tests\test001.yrd"
#endif    

//(new Microsoft.FSharp.Text.Lexing.LexBuffer<_>()).