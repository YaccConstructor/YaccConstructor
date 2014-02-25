module HardLexer

open Microsoft.FSharp.Text
open Yard.Generators.RNGLR.Parser
open Microsoft.FSharp.Reflection
open RNGLR.ParseCalc
 
let tokens(path) = 
    let toLexerTag (name:string) =
            printfn "%s" name
            match name with
            | "A" -> A (2,2)
            | "B" -> B (2,2)
            | "+" -> genLiteral "+" 2 2
            | "*" -> genLiteral "+" 2 2
            | x -> failwithf "Unexpected token %s" x

    System.IO.File.ReadAllText(path)
        .Split([|' '|])
    |> Array.map toLexerTag