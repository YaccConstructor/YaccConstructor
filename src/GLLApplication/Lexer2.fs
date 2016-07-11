module Lexer2

open Microsoft.FSharp.Text
open Yard.Generators.GLL.Parser
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection

let tokens(str : string) = 
    let toLexerTag (name:string) =
            match name with
            | "A" -> GLL.SimpleAmb.A (2)
            | x -> failwithf "Unexpected token %s" x

    str.Split([|' '|])
    |> Array.map toLexerTag


