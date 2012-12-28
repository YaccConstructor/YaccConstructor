// Learn more about F# at http://fsharp.net

let src = "..\..\input.txt"

let tokens = 
    let lexbuf = Lexing.LexBuffer<_>.FromTextReader <| new System.IO.StreamReader(src)
    seq { while not lexbuf.IsPastEndOfStream do
              yield Calc.Lexer.token lexbuf }

open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR.AST
open Calc.Parse

match buildAst tokens with
| Error (pos, token, msg, debugFuns) ->
    printfn "Error on position %d, token %A: %s" pos token msg
| Success ast ->
    ast.PrintAst()
    let args = {
        tokenToRange = fun _ -> Unchecked.defaultof<_>, Unchecked.defaultof<_>
        zeroPosition = Unchecked.defaultof<_>
        clearAST = false
        filterEpsilons = false
    }
    let result : double list = translate args ast
    printfn "%A" result