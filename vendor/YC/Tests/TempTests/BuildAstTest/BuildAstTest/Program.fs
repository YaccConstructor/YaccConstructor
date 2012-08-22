 
open Microsoft.FSharp.Text.Lexing

open Lexer
open Parser

let () =
    let filename = @"..\..\data.in"

    let content = System.IO.File.ReadAllText(filename)
    let reader = new System.IO.StringReader(content)    
    let lexbuf = LexBuffer<_>.FromTextReader reader

    let res = Parser.s Lexer.token lexbuf

    printf "%A" res

