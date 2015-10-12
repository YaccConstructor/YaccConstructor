// Learn more about F# at http://fsharp.net

module RNGLRApplication
open System.IO
open System
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection


open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.Parser
open RNGLR.SimpleAmb

open Yard.Generators.RNGLR
open Yard.Generators
open LexCommon

let run1 path astBuilder =
    let tokens = LexCommon.tokens2(path)
    astBuilder tokens, tokens


let parser1 = RNGLR.SimpleAmb.buildAst

for j = 1 to 5 do
    let str = String.init (j * 1000) (fun i -> "A + ( A * B ) ; ") + "B ;"
    let start = System.DateTime.Now
    let r = run1 (str.Trim()) parser1
    //printf "%A" r
    let t = System.DateTime.Now - start
    printfn "%A" t.TotalSeconds
