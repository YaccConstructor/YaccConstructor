// Main.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.


#light

let run path = 
    use reader = IO.binary_reader path
    let p = reader.PeekChar()
    Parser.file Lexer.main (Microsoft.FSharp.Text.Lexing.LexBuffer.FromBinaryReader reader)
    
let main = 
 run "test001.yrd";//Sys.argv.[1];
 print_any "end"


