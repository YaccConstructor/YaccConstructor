module LexCommon

open Microsoft.FSharp.Text
open Yard.Generators.RNGLR.Parser
open Microsoft.FSharp.Reflection
open RNGLR.SimpleLeftRecursion
 
let tokens(str : string) = 
    let toLexerTag (name:string) =
            //printfn "%s" name
            match name with
            //| "A" -> A (2,2)
            | "B" -> B (2)
            //| "+" -> genLiteral "+" 2 2
            //| "*" -> genLiteral "+" 2 2
            | x -> failwithf "Unexpected token %s" x
    str.Split([|' '|])
    |> Array.map toLexerTag
//    System.IO.File.ReadAllText(path)
//        .Split([|' '|])
//    |> Array.map toLexerTag