// Learn more about F# at http://fsharp.net

module RNGLRApplication

open Yard.Generators.RNGLR
open Yard.Generators
open LexCommon

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens, tokens

let dir = @"../../../../Tests/RNGLR/"

let rec printAst ind (ast : MultiAST) =
    let printInd num (x : 'a) =
        printf "%s" (String.replicate (num * 4) " ")
        printfn x
    let nodeTypeToStr = function
        | EpsTree -> "e"
        | NonTerm -> "n"
        | Term -> "t"
    match !ast with
    | [] -> ()
    | l ->  if l.Length > 1 then printInd ind "^^^^"
            l |> List.iteri 
                (fun i x -> if i > 0 then
                                printInd ind "----"
                            printInd ind "%s %d" (nodeTypeToStr x.nodeType) x.number
                            x.children
                            |> Array.iter (printAst <| ind+1))
            if l.Length > 1 then printInd ind "vvvv"


let parser = RNGLR.ParseCounter.buildAst
let path = dir + "counter/input.txt"
//let rightValue = [ANode [ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf]]]]]

match run path parser with
| Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
| Parser.Success mAst,tokens ->
    mAst |> printAst 0
    printfn "Result: %A" (RNGLR.ParseCounter.translate tokens mAst)
//|> (fun x -> Assert.IsTrue <| compareRes x rightValue)
