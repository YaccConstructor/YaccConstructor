// Learn more about F# at http://fsharp.net

module RNGLRApplication

open Yard.Generators.RNGLR
//open Yard.Generators.RNGLR.AST
open Yard.Generators
open LexCommon

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens, tokens

let parser = GLL.ParseCalc.parserSource
let path = @"..\..\input.txt"
//let rightValue = [ANode [ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf]]]]]


//|> (fun x -> Assert.IsTrue <| compareRes x rightValue)

