// Learn more about F# at http://fsharp.net

module GLLApplication
open System.IO
open System
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection


open Yard.Generators.GLL
open Yard.Generators.GLL.Parser    
open Microsoft.FSharp.Text.Lexing
open Yard.Generators.Common.AST2
open GLL.SimpleLeftRecursion
open GLL.BadLeftRecursion
open GLL.SimpleAmb
open GLL.SimpleRightRecursion

open Yard.Generators.GLL
open Yard.Generators
open Lexer2
//
let run1 path astBuilder =
    let tokens = Lexer2.tokens1(path)
    astBuilder tokens, tokens

let parser1 = GLL.BadLeftRecursion.buildAst
//let start = System.DateTime.Now
let r = run1 ((String.init (3) (fun i -> "B ")).Trim()) parser1
//let t = System.DateTime.Now - start
//printfn "%A" t.TotalSeconds
//match r with
//    | Parser.Error str, _ ->
//        printfn "%s" str
//    | Parser.Success tree, tokens ->
//        GLL.BadLeftRecursion.defaultAstToDot tree "ast.dot"
//printfn "Simple left recursion"
//for i in [1..10] do
//    let str = String.init (i * 5) (fun i -> "B ")
//    let start = System.DateTime.Now
//    let r = run1 (str.Trim()) parser1
//    let t = System.DateTime.Now - start
//    printfn "%A" t.TotalSeconds
////
//let run2 path astBuilder =
//    let tokens = Lexer2.tokens3(path)
//    astBuilder tokens, tokens
//
//let parser2 = GLL.Mixed.buildAst
//let str = String.init 5 (fun i -> "B ") + "A" 
//let r = run2 str parser2
//printfn "Mixed"
//for i in [1..100] do
//    let str = String.init (100) (fun i -> "B ") + "A "
//    let str2 = String.init (i) (fun i -> str)
//    let start = System.DateTime.Now
//    let r = run2 (str2.Trim()) parser2
//    let t = System.DateTime.Now - start
//    printfn "%A" t.TotalSeconds
//let run3 path astBuilder =
//    let tokens = Lexer2.tokens4(path)
//    astBuilder tokens, tokens
//
//let parser3 = GLL.SimpleRightRecursion.buildAst
//let r = run3 ((String.init (1000) (fun i -> "B ")).Trim()) parser3
//let r = run3 ((String.init (800) (fun i -> "B ")).Trim()) parser3
//let r = run "A D B" parser
//printfn "Simple right recursion"
//for i in [1..100] do
//    let str = "B " + String.init (i * 100) (fun i -> "A ") + "B"
//    let start = System.DateTime.Now
//    let r = run3 (str.Trim()) parser3
//    let t = System.DateTime.Now - start
//    printfn "%A" t.TotalSeconds

//match r with
//    | Parser.Error str, _ ->
//        printfn "%s" str
//    | Parser.Success tree, tokens ->
//        GLL.SimpleLeftRecursion.defaultAstToDot tree "ast.dot"
//
printfn "ff"
