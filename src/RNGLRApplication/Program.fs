// Learn more about F# at http://fsharp.net

module RNGLRApplication

open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open Yard.Generators
open LexCommon

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens, tokens

let parser = RNGLR.ParseCalc.buildAst
let path = @"..\..\..\src\RNGLRApplication\input.txt"
//let rightValue = [ANode [ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf]]]]]

match run path parser with
| Parser.Error (num, tok, message, debug, _),_ ->
    printfn "Error in position %d on Token %A: %s" num tok message
    debug.drawGSSDot "out.dot"
| Parser.Success (tree, _, _), tokens ->
    tree.PrintAst()
    RNGLR.ParseCalc.defaultAstToDot tree "ast.dot"
    //tree.Nodes |> Array.iteri (fun i x -> printfn "%2d: %A" i x)
    //printfn "%A" tree.Order
    let args = {
        tokenToRange = fun _ -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }


    printfn "Result: %A" (RNGLR.ParseCalc.translate args tree)
    tree.ChooseSingleAst()
    tree.PrintAst()

//|> (fun x -> Assert.IsTrue <| compareRes x rightValue)
