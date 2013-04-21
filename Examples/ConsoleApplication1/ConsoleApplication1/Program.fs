open RNGLR.Parse
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

let zero = Lexing.Position.Empty
let translateArgs = {
        tokenToRange = fun _ -> zero,zero
        zeroPosition = zero
        clearAST = false
        filterEpsilons = true
    }

//let test = fun (hd::tl) -> 1

[<EntryPoint>]
let main argv =
    match buildAst [X "x"; A "a"; B "b"; EOF "$"] with
        | Parser.Error _ -> printf "Parsing error"
        | Parser.Success tree -> translate translateArgs tree |> ignore
    
    0 // return an integer exit code
