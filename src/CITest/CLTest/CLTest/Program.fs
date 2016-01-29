// Learn more about F# at http://fsharp.net

open System.IO
open Microsoft.FSharp.Text.Lexing

let testLexerAndParserFromString text expectedCount = 
    let lexbuf = LexBuffer<char>.FromString text
    let mutable p = 0
    while Lexer.tokenstream lexbuf <> Parser.EOF do
        p <- p + 1
    System.Console.WriteLine(p)

testLexerAndParserFromString "sIn(E) + 23" 7
testLexerAndParserFromString "e" 1



