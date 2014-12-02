module Lexer2

open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
open GLL.Parse.Test

let tokens(str : string) = 
    let toLexerTag (name:string) =
            match name with
            | "A" -> GLL.Parse.Test.A (2)
            | "B" -> GLL.Parse.Test.B (2)
            | "D" -> GLL.Parse.Test.D (2)
            | x -> failwithf "Unexpected token %s" x

    str.Split([|' '|])
    |> Array.map toLexerTag


