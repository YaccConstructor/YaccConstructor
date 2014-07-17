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
open GLL.SimpleAmb

open Yard.Generators.GLL
open Yard.Generators
open Lexer2

let run path astBuilder =
    let tokens = Lexer2.tokens2(path)
    astBuilder tokens, tokens

let parser = GLL.SimpleAmb.buildAst
let r = run ((String.init (3) (fun i -> "B ")).Trim()) parser
//let r = run "A D B" parser
//for i in [1..100] do
//    let str = String.init (i * 100) (fun i -> "B ")
//    let start = System.DateTime.Now
//    let r = run (str.Trim()) parser
//    let t = System.DateTime.Now - start
//    printfn "%A" t.TotalSeconds
//    //printfn "%d" (i*100)
//

match r with
    | Parser.Error str, _ ->
        printfn "%s" str
    | Parser.Success tree, tokens ->
        GLL.SimpleAmb.defaultAstToDot tree "ast.dot"

printfn "ff"
