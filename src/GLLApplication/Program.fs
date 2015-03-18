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
    let il = ref <| fe.ParseGrammar(@"C:\Users\User\recursive-ascent\src\GLLApplication\SimpleAmb.yrd")
    for constr in gen.Constraints do
        let grammar = il.Value.grammar
        if not <| constr.Check grammar then
            eprintfn "Constraint %s: applying %s..." constr.Name constr.Conversion.Name
            il := {!il with grammar = constr.Fix grammar}

    gen.Generate(!il,"-pos int -token int -module GLL.Parse.SimpleAmb -o SimpleAmb.yrd.fs")

run () |> printfn "%A"

let run1 astBuilder =
    let lb = LexBuffer<_>.FromString "1 - 2"
    let tokens = seq{while not lb.IsPastEndOfStream do yield Calc.Lexer.token lb}
    astBuilder tokens, tokens


let parser1 = GLL.Parse.SimpleAmb.buildAst

//for i in [1..10] do
//    let str = String.init (i * 5) (fun i -> "B ")
//    let start = System.DateTime.Now
//    let r = run1 (str.Trim()) parser1
//    let t = System.DateTime.Now - start
//    printfn "%A" t.TotalSeconds

//////////////////////////////////////
//let run2 path astBuilder =
//    let tokens = Lexer2.tokens2(path)
//    astBuilder tokens, tokens
//
//let parser2 = GLL.Parse.SimpleAmb.buildAst
//let str = String.init 100 (fun i -> "B ") + "B"
//let str = "A B"
let r = run1 parser1
//printfn "simple amb"

match r with
    | Parser.Error str, _ ->
        printfn "%s" "dddd" //str 
    | Parser.Success tree, tokens ->
        printfn "%s" "sss"
        //GLL.SimpleLeftRecursion.defaultAstToDot tree "ast.dot"
////
//printfn "ff"
