module Lexer2

open Microsoft.FSharp.Text
//open Yard.Generators.RNGLR.Parser
open Microsoft.FSharp.Reflection
open GLL.SimpleAmb

let tokens(path) = 
    let toLexerTag (name:string) =
            printfn "%s" name
            match name with
            | "A" -> A (2)
            | "B" -> B (2)
            | "D" -> D (2)
            //| "S" -> S (2)
            | x -> failwithf "Unexpected token %s" x

    System.IO.File.ReadAllText(path)
        .Split([|' '|])
    |> Array.map toLexerTag
