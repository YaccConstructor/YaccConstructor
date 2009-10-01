// Main.fs
//
// Copyright 2009 Jake Kirilenko
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light
open Microsoft.FSharp.Text.Lexing

let run_common path = 
    Lexer.currentFileContent := System.IO.File.ReadAllText(path);
    let reader = IO.binary_reader path in
    LexBuffer<_>.FromBinaryReader reader

let run path =
    let buf = run_common path in
    let example = seq {
                       while not buf.IsPastEndOfStream do
                             yield Lexer.main buf  
                      } in
    Seq.iter (printfn "%A ") example


let ParseFile path = 
    let buf = run_common path
    let res = Parser.file Lexer.main buf
#if DEBUG
    printf "%A\n" <|res
#endif    
    res
    
#if DEBUG
let main =
    printf "%A\n" <| ParseFile @"..\..\..\..\Tests\test001.yrd"
#endif    