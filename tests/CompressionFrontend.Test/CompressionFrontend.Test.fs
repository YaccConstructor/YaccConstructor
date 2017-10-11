module CompressionFrontendTest

open NUnit.Framework
open Yard.Core.IL
open Production
open Yard.Frontends.CompressionFrontend
open Yard.Frontends.CompressionFrontend.Compression

let tester = CompressionFrontend()

let mTok x = PToken(Source.t x)
let mRef x = PRef(Source.t ("sq_" + x.ToString()), None)
let mSeq x = PSeq([for e in x -> {omit = false; rule = e; binding= None; checker = None}], None, None)

[<Test>]
let ``When empty string expect empty grammar``() = 
    Assert.AreEqual(tester.ParseGrammar("").grammar.Head.rules, [])

[<Test>]
let ``When only special symbols expect empty grammar``() = 
    let x : Rule.t<Source.t, Source.t> list = []
    Assert.AreEqual(x, tester.ParseGrammar("$$$$$", '$').grammar.Head.rules)

[<Test>]
let ``When only one symbol expect one rule with token``() = 
    let x : t<Source.t, Source.t> = mTok "a"
    Assert.AreEqual(x, tester.ParseGrammar("a").grammar.Head.rules.Head.body)

[<Test>]
let ``When expect alternatives``() =
    let x : t<Source.t, Source.t> = PAlt(mTok "a", mTok "b")
    let y : t<Source.t, Source.t> = PAlt(mSeq [mTok "a"; mTok "b"], PAlt (mSeq [mTok "c"; mTok "b"], mSeq [mTok "a"; mTok "c"]))
    Assert.AreEqual(x, tester.ParseGrammar("a$b", '$').grammar.Head.rules.Head.body)
    Assert.AreEqual(y, tester.ParseGrammar(["ab"; "cb"; "ac"]).grammar.Head.rules.Head.body)

[<Test>] 
let ``if it doesnt compress specials with ordinary symbols``() =
    Assert.AreNotEqual(true, tester.ParseGrammar("a&a&a&a", '&').grammar.Head.rules.Length > 1)

[<Test>]
let ``if it expands useful rules``() =
    let x : t<Source.t, Source.t> = mSeq [mRef 1; mRef 1]
    let y : t<Source.t, Source.t> = mSeq [mTok "a"; mTok "b"; mTok "c"; mTok "d"]
    let res = tester.ParseGrammar("abcdabcd")
    Assert.AreEqual(x, res.grammar.Head.rules.Head.body)
    Assert.AreEqual(y, res.grammar.Head.rules.Tail.Head.body)

[<Test>]
let ``if it really compresses``() =
    let x : t<Source.t, Source.t> = mSeq [mRef 1; mRef 1]
    let y : t<Source.t, Source.t> = mSeq [mTok "a"; mRef 2; mRef 2]
    let z : t<Source.t, Source.t> = mSeq [mTok "b"; mTok "c"]
    let res = tester.ParseGrammar "abcbcabcbc"
    Assert.AreEqual(x, res.grammar.Head.rules.Head.body)
    Assert.AreEqual(z, res.grammar.Head.rules.Tail.Head.body)
    Assert.AreEqual(y, res.grammar.Head.rules.Tail.Tail.Head.body)    
