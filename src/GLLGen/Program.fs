// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module GLLGen
let run () =
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let gen = new Yard.Generators.GLL2.GLL2()
    let il = fe.ParseGrammar(@"C:\Users\User\recursive-ascent\src\GLLParser.SimpleTest\SimpleAmb.yrd")
    gen.Generate(il,"-pos int -token int -module GLL.Parse.Test -o Test.yrd.fs")

run () |> printfn "%A"
