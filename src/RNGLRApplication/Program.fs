// Learn more about F# at http://fsharp.net

module RNGLRApplication
open System.IO
open System
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection


open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.Parser
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

for i in [1..12] do
    let str = String.init (i * 5) (fun i -> "B ")
    let start = System.DateTime.Now
    let r = run1 (str.Trim()) parser1
    let t = System.DateTime.Now - start
    printfn "%A" t.TotalSeconds
