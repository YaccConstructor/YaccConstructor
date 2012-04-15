// Learn more about F# at http://fsharp.net

module RNGLRApplication

open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open Yard.Generators
open LexCommon

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens, tokens

let dir = @"../../../../Tests/RNGLR/"

let parser = RNGLR.ParseCalc.buildAst
let path = dir + "calc/input.txt"
//let rightValue = [ANode [ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf]]]]]

match run path parser with
| Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
| Parser.Success mAst,tokens ->
    mAst |> printAst 0
    RNGLR.ParseCalc.defaultAstToDot "ast.dot" mAst
    printfn "Result: %A" (RNGLR.ParseCalc.translate mAst)
//|> (fun x -> Assert.IsTrue <| compareRes x rightValue)
