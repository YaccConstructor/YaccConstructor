// Learn more about F# at http://fsharp.net

module GLLApplication
open System.IO
open System
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
//packages/yc.tools/lib/net40/fslex.exe file.fsl --unicode

open Yard.Generators.GLL

open Yard.Generators.GLL.Parser    
open Microsoft.FSharp.Text.Lexing
open Yard.Generators.RNGLR
//open GLL.SimpleLeftRecursion

open Yard.Frontends.YardFrontend
open Yard.Generators.GLL
open Yard.Generators
open Lexer2

//let run () =
//    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
//    let gen = new Yard.Generators.GLL.GLL()
//    let il = ref <| fe.ParseGrammar(@"C:\Users\User\recursive-ascent\src\GLLApplication\SimpleAmb.yrd")
//    for constr in gen.Constraints do
//        let grammar = il.Value.grammar
//        if not <| constr.Check grammar then
//            eprintfn "Constraint %s: applying %s..." constr.Name constr.Conversion.Name
//            il := {!il with grammar = constr.Fix grammar}
//
//    gen.Generate(!il,"-pos int -token int -abstract false -module GLL.Parse.SimpleAmb -o SimpleAmb.yrd.fs -output C:\Users\User\recursive-ascent\src\GLLApplication")
//
//run () |> printfn "%A"

//let run1 astBuilder =
//    let lb = LexBuffer<_>.FromString "1 - 2"
//    let tokens = seq{while not lb.IsPastEndOfStream do yield Calc.Lexer.token lb}
//    astBuilder tokens, tokens

let outDir = @"../../../src/GLLApplication/"

//let parser1 = GLL.SimpleAmb.buildAst
//
//let run1 path astBuilder =
//    let tokens = Lexer2.tokens(path)
//    astBuilder tokens, tokens
//
//let str1 = String.init 500 (fun i -> "B ") + "B"
//   
//let start = System.DateTime.Now
//let r = run1 str1 parser1
//let t = System.DateTime.Now - start
//printfn "%A" t.TotalSeconds
//
//match r with
//    | Parser.Error str, _ ->
//        printfn "%s" "dddd" //str 
//    | Parser.Success tree, tokens ->
//        printfn "%s" "sss"
//        GLL.SimpleAmb.defaultAstToDot tree GLL.SimpleAmb.tokenToNumber GLL.SimpleAmb.tokenData  (outDir + "SimpleLeft.dot") 

let str2 = String.init 250 (fun i -> "A + ( A * B ) * A + ( A * B ) + ( A + ( A * B + A + ( A * B ) ) ) + A + ( A * B ) * A + ( A * B ) + ( A + ( A * B + A + ( A * B ) ) ) + ( A + ( A * B ) * A + ( A * B ) + ( A + ( A * B + A + ( A * B ) ) ) + A + ( A * B ) * A + ( A * B ) + ( A + ( A * B + A + ( A * B ) ) ) ) ; ") + "B ;"
let parser2 = GLL.Calc.buildAst

let run2 path astBuilder =
    let tokens = Lexer2.tokens3(path)   
    astBuilder tokens, tokens

    
let start2 = System.DateTime.Now
let r2 = run2 str2 parser2
let t2 = System.DateTime.Now - start2
printfn "%A" t2.TotalSeconds

match r2 with
    | Parser.Error str, _ ->
        printfn "%s" "dddd" //str 
    | Parser.Success tree, tokens ->
        printfn "%s" "sss"
        GLL.Calc.defaultAstToDot tree GLL.Calc.tokenToNumber GLL.Calc.tokenData  (outDir + "Calc.dot") 





