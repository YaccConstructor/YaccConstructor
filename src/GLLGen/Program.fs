// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module GLLGen
open GLL.Parse.Test
let run () =
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let gen = new Yard.Generators.GLL2.GLL2()
    let il = fe.ParseGrammar(@"C:\Users\User\recursive-ascent\src\GLLParser.SimpleTest\SimpleAmb.yrd")
    gen.Generate(il,"-pos int -token int -module GLL.Parse.Test -o Test.yrd.fs")

run () |> printfn "%A"

let run1 input astBuilder =
    let tokens = Lexer2.tokens(input)
    astBuilder tokens

let parser1 = GLL.Parse.Test.buildAst
//let start = System.DateTime.Now
//let str = Lexer2.tokens((String.init (1) (fun i -> "A ")).Trim())
let str = Lexer2.tokens "A D B"
let r = GLL.Parse.Test.buildAst str 
r.Start

