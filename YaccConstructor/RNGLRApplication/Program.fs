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
let zeroPos = 0
let tokenToRange _ = 0,0
//let rightValue = [ANode [ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf]]]]]

match run path parser with
| Parser.Error (num, tok, message),_ -> printfn "Error in position %d on Token %A: %s" num tok message
| Parser.Success tree, tokens ->
    tree.PrintAst()
    RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
    //tree.Nodes |> Array.iteri (fun i x -> printfn "%2d: %A" i x)
    //printfn "%A" tree.Order
    printfn "Result: %A" (RNGLR.ParseCalc.translate tokenToRange zeroPos tree)
//|> (fun x -> Assert.IsTrue <| compareRes x rightValue)
