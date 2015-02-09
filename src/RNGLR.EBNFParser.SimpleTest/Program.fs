// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Yard.Generators.RNGLR.EBNF.Parser

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens

let dir = @"../../../Tests/RNGLR.EBNF/"
let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    //Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)

[<EntryPoint>]
let main argv = 
    let parser = RNGLR.EBNFParse.SimpleOpt.buildAst
    let path = dir + "simpleOneTerm.txt"
    match run path parser with
        | Error (num, tok, err, _) -> printErr (num, tok, err)
        | Success (tree, _) -> 
            printfn "Success"
    0 // return an integer exit code
