module LexCommon

open Microsoft.FSharp.Text
open Yard.Generators.RNGLR.Parser
open Microsoft.FSharp.Reflection
open RNGLR.SimpleLeftRecursion
open RNGLR.BadLeftRecursion
open RNGLR.SimpleRightRecursion
open RNGLR.SimpleAmb
open RNGLR.Mixed

let tokens1(str : string) = 
    let toLexerTag (name:string) =
            match name with
            | "B" -> RNGLR.BadLeftRecursion.B (2)
            | x -> failwithf "Unexpected token %s" x

    str.Split([|' '|])
    |> Array.map toLexerTag

let tokens2(str : string) = 
    let toLexerTag (name:string) =
            match name with
            | "A" -> RNGLR.SimpleAmb.A (2)
            | "B" -> RNGLR.SimpleAmb.B (2)
            | "D" -> RNGLR.SimpleAmb.D (2)
            | x -> failwithf "Unexpected token %s" x

    str.Split([|' '|])
    |> Array.map toLexerTag

let tokens3(str : string) = 
    let toLexerTag (name:string) =
            match name with
            | "A" -> RNGLR.Mixed.A (2)
            | "B" -> RNGLR.Mixed.B (2)
            | x -> failwithf "Unexpected token %s" x

    str.Split([|' '|])
    |> Array.map toLexerTag

let tokens4(str : string) = 
    let toLexerTag (name:string) =
            match name with
            | "B" -> RNGLR.SimpleRightRecursion.B (2)
            | x -> failwithf "Unexpected token %s" x

    str.Split([|' '|])
    |> Array.map toLexerTag
