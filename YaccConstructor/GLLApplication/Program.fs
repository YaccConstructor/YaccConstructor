// Learn more about F# at http://fsharp.net

module RNGLRApplication

open Yard.Generators.GLL
//open Yard.Generators.RNGLR.AST
open Yard.Generators
open Lexer2

let run path astBuilder =
    let tokens = Lexer2.tokens(path)
    astBuilder tokens, tokens

let parser = GLL.SimpleAmb.buildAst
let path = @"..\..\input.txt"
//let rightValue = [ANode [ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf]]]]]

match run path parser with
| Parser.Error _ , _ ->
    printfn "Error"
    //debug.drawGSSDot "out.dot"
| Parser.Success _, _ ->
    printfn "Success"
    //tree.Nodes |> Array.iteri (fun i x -> printfn "%2d: %A" i x)
    //printfn "%A" tree.Order
//    let args = {
//        tokenToRange = fun _ -> 0,0
//        zeroPosition = 0
//        clearAST = false
//        filterEpsilons = true
//    }


    //printfn "Result: %A" (RNGLR.ParseCalc.translate args tree)
    //tree.ChooseSingleAst()
    //tree.PrintAst()

//|> (fun x -> Assert.IsTrue <| compareRes x rightValue)
