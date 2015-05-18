// Learn more about F# at http://fsharp.net

module RNGLRApplication
open System.IO
open System
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection


open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.Parser 
open Yard.Generators.Common.AST2
open RNGLR.SimpleLeftRecursion
open RNGLR.BadLeftRecursion
open RNGLR.SimpleAmb
open RNGLR.SimpleRightRecursion

open Yard.Generators.RNGLR
open Yard.Generators
open LexCommon

let run1 path astBuilder =
    let tokens = LexCommon.tokens1(path)
    astBuilder tokens, tokens


let parser1 = RNGLR.BadLeftRecursion.buildAst
//let str = String.init (50) (fun i -> "B ")
//let start = System.DateTime.Now
//let r = run1 (str.Trim()) parser1
//let t = System.DateTime.Now - start
//printfn "%A" t.TotalSeconds
//printfn "Simple left recursion"
for i in [1..10] do
    let str = String.init (i * 5) (fun i -> "B ")
    let start = System.DateTime.Now
    let r = run1 (str.Trim()) parser1
    let t = System.DateTime.Now - start
    printfn "%A" t.TotalSeconds
////
//let run2 path astBuilder =
//    let tokens = LexCommon.tokens3(path)
//    astBuilder tokens, tokens
//
//let parser2 = RNGLR.Mixed.buildAst
//let str = String.init 5 (fun i -> "B ") + "A" 
//let r = run2 str parser2
printfn "Mixed"
//for i in [1..100] do
//    let str = String.init (100) (fun i -> "B ") + "A "
//    let str2 = String.init (i) (fun i -> str)
//    let start = System.DateTime.Now
//    let r = run2 (str2.Trim()) parser2
//    let t = System.DateTime.Now - start
//    printfn "%A" t.TotalSeconds
//
//let run3 path astBuilder =
//    let tokens = LexCommon.tokens4(path)
//    astBuilder tokens, tokens
//
//let parser3 = RNGLR.SimpleRightRecursion.buildAst
////let r = run ((String.init (10000) (fun i -> "B ")).Trim()) parser
////let r = run "A D B" parser
//printfn "Simple right recursion"
//for i in [1..100] do
//    let str = String.init (i * 100) (fun i -> "B ")
//    let start = System.DateTime.Now
//    let r = run3 (str.Trim()) parser3
//    let t = System.DateTime.Now - start
//    printfn "%A" t.TotalSeconds
//
////match r with
////    | Parser.Error str, _ ->
////        printfn "%s" str
////    | Parser.Success tree, tokens ->
////        RNGLR.SimpleLeftRecursion.defaultAstToDot tree "ast.dot"
////
////printfn "ff"
