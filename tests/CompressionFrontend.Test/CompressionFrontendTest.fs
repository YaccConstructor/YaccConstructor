module CompressionFrontendTest

open NUnit.Framework
open Yard.Core.IL
open Production
open Yard.Frontends.CompressionFrontend
open Yard.Frontends.CompressionFrontend.Compression

let tester = CompressionFrontend()

[<Test>]
let ``When empty string expect empty grammar``() = 
    Assert.AreEqual(tester.ParseGrammar("").grammar.Head.rules, [])

[<Test>]
let ``When only one symbol expect one rule with token``() = 
    let x : t<Source.t, Source.t> = PToken(Source.t "a")
    Assert.AreEqual(x, tester.ParseGrammar("a").grammar.Head.rules.Head.body)

[<Test>]
let ``When only special symbols expect empty grammar``() = 
    let x : Rule.t<Source.t, Source.t> list = []
    Assert.AreEqual(x, tester.ParseGrammar("$$$$$", '$').grammar.Head.rules)

[<Test>]
let ``When a$b expect alternatives``() = 
    let x : t<Source.t, Source.t> = PAlt(PToken(Source.t "a"), PToken(Source.t "b"))
    Assert.AreEqual(x, tester.ParseGrammar("a$b", '$').grammar.Head.rules.Head.body)

[<Test>]
let ``When abcabc expect grammar with two rules``() =
    Assert.AreEqual(2, tester.ParseGrammar("abcabc").grammar.Head.rules.Length)

[<Test>]
let ``correctness of function``() = 
    Assert.AreEqual(tester.ParseGrammar("abra$cadabra", '$'), tester.ParseGrammar("abra$cadabra", '$'))

[<Test>]
let ``When two strings expect alternatives``() =
    let x : t<Source.t, Source.t> = PAlt(PToken(Source.t "a"), PToken(Source.t "b"))
    Assert.AreEqual(x, tester.ParseGrammar(["a"; "b"]).grammar.Head.rules.Head.body)
