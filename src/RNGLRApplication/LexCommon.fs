module LexCommon

open Microsoft.FSharp.Text
open Yard.Generators.RNGLR.Parser
open Microsoft.FSharp.Reflection
open RNGLR.SimpleAmb

let tokens2(str : string) = 
    let toLexerTag (name:string) =
            match name with
            | "/" -> RNGLR.SimpleAmb.DIV (2)
            | "(" -> RNGLR.SimpleAmb.LBRACE (2)
            | ")" -> RNGLR.SimpleAmb.RBRACE (2)
            | "-" -> RNGLR.SimpleAmb.MINUS (2)
            | "+" -> RNGLR.SimpleAmb.PLUS (2)
            | "*" -> RNGLR.SimpleAmb.MULT (2)
            | "A" -> RNGLR.SimpleAmb.NUMBER (2)
            | "B" -> RNGLR.SimpleAmb.NUMBER (2)
            | "**" -> RNGLR.SimpleAmb.POW (2)
            | ";" -> RNGLR.SimpleAmb.SEMI (2)
            | x -> failwithf "Unexpected token %s" x

    str.Split([|' '|])
    |> Array.map toLexerTag
