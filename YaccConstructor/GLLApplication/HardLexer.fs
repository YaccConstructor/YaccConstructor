module HardLexer

open Microsoft.FSharp.Text
//open Yard.Generators.RNGLR.Parser
open Microsoft.FSharp.Reflection
open GLL.Calc
 
let tokens(path) = 
    let toLexerTag (name:string) =
            printfn "%s" name
            match name with
            | "A" -> A (2)
            | "B" -> B (2)
            | "+" -> 
                let res = genLiteral "+" 2
                if res.IsSome then res.Value 
                else failwith "Unexpected token"
            | "*" -> 
                let res = genLiteral "*" 2
                if res.IsSome then res.Value 
                else failwith "Unexpected token" 
            | x -> failwithf "Unexpected token %s" x

    System.IO.File.ReadAllText(path)
        .Split([|' '|])
    |> Array.map toLexerTag