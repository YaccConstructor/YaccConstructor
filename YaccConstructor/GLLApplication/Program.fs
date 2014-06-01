// Learn more about F# at http://fsharp.net

module GLLApplication
open System.IO
open System
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection


open Yard.Generators.GLL
open Yard.Generators.GLL.Parser    
open Microsoft.FSharp.Text.Lexing
open Yard.Generators.RNGLR.AST
open GLL.Calc2

open Yard.Generators.GLL
//open Yard.Generators.RNGLR.AST
open Yard.Generators
open Calc.Lexer

let src = @"..\..\input.txt"

let tokens = 
    let lexbuf = Lexing.LexBuffer<_>.FromTextReader <| new System.IO.StreamReader(src)
    seq { while not lexbuf.IsPastEndOfStream do
              yield Calc.Lexer.token lexbuf }


let run2 astBuilder =
    astBuilder tokens

let parser = GLL.Calc2.buildAst
//let path = @"..\..\input.txt"
//let rightValue = [ANode [ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf]]]]]

match run2 parser with
| Parser.Error str ->
    printfn "%s" str
| Parser.Success tree ->
    tree.PrintAst()
    GLL.Calc2.defaultAstToDot tree "ast.dot"
printfn "ff"

//
//    //printfn "Result: %A" (GLL.SimpleAmb.translate args tree)
//    //tree.ChooseSingleAst()
//    //tree.PrintAst()
//let src = @"..\..\input.txt"
//
//
//open Lexer2
//
//let run path astBuilder =
//    let tokens = Lexer2.tokens(path)
//    astBuilder tokens, tokens
//
//let parser = GLL.SimpleAmb.buildAst
//let path = @"..\..\input.txt"
////let rightValue = [ANode [ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf]]]]]
//
//
//match run path parser with
//| Parser.Error str, _ ->
//    printfn "%s" str
//| Parser.Success tree, tokens ->    
//    tree.PrintAst()
//    GLL.SimpleAmb.defaultAstToDot tree "ast.dot"
//
//
//printfn "ff"