// Learn more about F# at http://fsharp.net

let srcGraph = "..\..\inputGraph.dot"
let src = "..\..\inputSeq.txt"

let tokens = 
    let lexbuf = Lexing.LexBuffer<_>.FromTextReader <| new System.IO.StreamReader(src)
    seq { while not lexbuf.IsPastEndOfStream do
              yield Calc.Lexer.token lexbuf }

open System.IO
open System
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection



open Yard.Generators.RNGLR.Parser    

open Yard.Generators.RNGLR.AST
open Calc.Parser

match buildAst tokens with
//match buildAst tokens with
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
    let result:List<double> = translate args ast
    //defaultAstToDot ast @"..\..\astFromSeq.dot"
    defaultAstToDot ast @"..\..\astFromDot.dot"
    printfn "%A" result
