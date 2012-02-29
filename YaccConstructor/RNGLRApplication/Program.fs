// Learn more about F# at http://fsharp.net

module RNGLRApplication

open Yard.Generators.RNGLR
open Yard.Generators
open LexCommon

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens

let dir = @"../../../../Tests/RNGLR/"

let rec printAst ind (ast : MultiAST) =
    let printInd num (x : 'a) =
        printf "%s" (String.replicate (num * 4) " ")
        printfn x

    match !ast with
    | [] -> ()
    | l -> l |> List.iter
                (fun x -> printInd ind "%d" x.number
                          x.children
                          |> Array.iter (printAst <| ind+1))

let parser = RNGLR.Parse.buildAst
let path = dir + "first/input.txt"
//let rightValue = [ANode [ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf]]]]]

match run path parser with
| Parser.Error (num, message) -> printfn "Error in position %d: %s" num message
| Parser.Success mAst -> mAst |> printAst 0
//|> (fun x -> Assert.IsTrue <| compareRes x rightValue)
