module Lexer2

open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
    open GLL.Parse.SimpleAmb


let tokens(str : string) = 
    let toLexerTag (name:string) =
            match name with
            | "A" -> GLL.Parse.SimpleAmb.A (2)
            | "D" -> GLL.Parse.SimpleAmb.D (2)
            | "B" -> GLL.Parse.SimpleAmb.B (2)
            | x -> failwithf "Unexpected token %s" x

    str.Split([|' '|])
    |> Array.map toLexerTag
