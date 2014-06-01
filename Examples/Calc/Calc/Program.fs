// Learn more about F# at http://fsharp.net

//let srcGraph = "..\..\inputGraph.dot"
let src = "..\..\inputSeq.txt"

let tokens = 
    let lexbuf = Lexing.LexBuffer<_>.FromTextReader <| new System.IO.StreamReader(src)
    seq { while not lexbuf.IsPastEndOfStream do
              yield Calc.Lexer.token lexbuf }

open System.IO
open System
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection


open Yard.Generators.GLL
open Yard.Generators.GLL.Parser    

open Yard.Generators.RNGLR.AST
open GLL.Calc2

match buildAst tokens with
//match buildAst tokens with
| Error _ ->
    printfn "Error"
| Success ast ->
    ast.PrintAst()
    
