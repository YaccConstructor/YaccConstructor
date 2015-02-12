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
open GLL.Parse.SimpleAmb
open GLL.SimpleRightRecursion
open Yard.Frontends.YardFrontend
open Yard.Generators.GLL
open Yard.Generators
open Lexer2


let run () =
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let gen = new Yard.Generators.GLL.GLL()
    let il = fe.ParseGrammar(@"C:\Users\User\recursive-ascent\src\GLLParser.SimpleTest\SimpleAmb.yrd")
    gen.Generate(il,"-pos int -token int -module GLL.Parse.SimpleAmb -o SimpleAmb.yrd.fs")

run () |> printfn "%A"

let run2 path astBuilder =
    let tokens = Lexer2.tokens2(path)
    astBuilder tokens, tokens

let parser2 = GLL.Parse.SimpleAmb.buildAst
let str = "A D B" //String.init 5 (fun i -> "B ") + "A" 
let r = run2 str parser2
printfn "simple amb"
//for i in [1..100] do
//    let str = String.init (100) (fun i -> "B ") + "A "
//    let str2 = String.init (i) (fun i -> str)
//    let start = System.DateTime.Now
//let str = "A D B"
//let r = run2 (str.Trim()) parser2
//    let t = System.DateTime.Now - start
//   printfn "%A" t.TotalSeconds



match r with
    | Parser.Error str, _ ->
        printfn "%s" str
    | Parser.Success tree, tokens ->
        printfn "%s" "sss"
        //GLL.SimpleLeftRecursion.defaultAstToDot tree "ast.dot"
//
printfn "ff"
