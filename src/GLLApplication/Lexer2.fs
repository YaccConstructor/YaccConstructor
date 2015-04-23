module Lexer2

open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
open GLL.SimpleLeftRecursion
open GLL.BadLeftRecursion
open GLL.SimpleRightRecursion
open GLL.Parse.SimpleAmb
open GLL.Mixed

let tokens1(str : string) = 
    let toLexerTag (name:string) =
            match name with
            //| "A" -> GLL.Parse.SimpleAmb.A (2)
            | "A" -> GLL.Parse.SimpleAmb.A (2)
          //  | "D" -> GLL.Parse.SimpleAmb.D (2)
           // | "B" -> GLL.Parse.SimpleAmb.B (2)
            | x -> failwithf "Unexpected token %s" x

    str.Split([|' '|])
    |> Array.map toLexerTag

//let tokens2(str : string) = 
//    let toLexerTag (name:string) =
//            match name with
//            | "A" -> GLL.Parse.SimpleAmb.A (2)
//            | "B" -> GLL.Parse.SimpleAmb.B (2)
//            | "D" -> GLL.Parse.SimpleAmb.D (2)
//            | x -> failwithf "Unexpected token %s" x
//
//    str.Split([|' '|])
//    |> Array.map toLexerTag

let tokens3(str : string) = 
    let toLexerTag (name:string) =
            match name with
            | "A" -> GLL.Mixed.A (2)
            | "B" -> GLL.Mixed.B (2)
            | x -> failwithf "Unexpected token %s" x

    str.Split([|' '|])
    |> Array.map toLexerTag

let tokens4(str : string) = 
    let toLexerTag (name:string) =
            match name with
            | "B" -> GLL.SimpleRightRecursion.B (2)
            | "A" -> GLL.SimpleRightRecursion.A (2)
            | x -> failwithf "Unexpected token %s" x

    str.Split([|' '|])
    |> Array.map toLexerTag



