open Lexer
open Parser

let test =
    "let x = 1; let y = fun d -> d + 23"

let res = Parser.s Lexer.token (Lexing.LexBuffer<_>.FromString test)

match res with
| AST.Prog es -> printfn "%i" es.Length
