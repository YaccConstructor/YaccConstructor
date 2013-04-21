open Yard.Examples.MSParser
open Yard.Utils.SourceText

let parseFile (fileName:string) =
    let basePath = "../../../../../Tests/MSSqlParser"
    let reader = new System.IO.StreamReader (System.IO.Path.Combine (basePath,fileName))
    let lexbuf = Lexing.LexBuffer<_>.FromTextReader reader
    let tokens = seq { while not lexbuf.IsPastEndOfStream do yield Lexer.tokens lexbuf }
    parse tokens

[<EntryPoint>]
let main argv =
    parseFile "BeginTransaction.sql" |> printfn "%A"
    0 // return an integer exit code
