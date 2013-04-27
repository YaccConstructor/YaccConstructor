open Yard.Examples.MSParser
open Yard.Utils.SourceText

let basePath = "../../../../../Tests"

let parseFile (fileName:string) =
    let startTime = System.DateTime.Now
    try
        try
            let reader = new System.IO.StreamReader (System.IO.Path.Combine (basePath,fileName))
            let lexbuf = Lexing.LexBuffer<_>.FromTextReader reader
            let tokens = seq { while not lexbuf.IsPastEndOfStream do yield Lexer.tokens lexbuf }
            printf "Parsing %s: " fileName
            printf (if parse tokens then "OK" else "Parsing Error")
        with
           | e -> printfn "%s" (e.ToString ())
    finally
        printfn " %f s" (System.DateTime.Now - startTime).TotalSeconds

[<EntryPoint>]
let main argv =
    printf "(warmup) "
    //parseFile "MSSqlParser/BeginTransaction.sql"
    
    //parseFile "MSSqlParser/BeginTransaction.sql"
    //parseFile "MSSqlParser/DeclareLocalVars.sql"
    parseFile "materials/ms-sql/sysprocs/sp_addextendedproperty.sql"
    parseFile "materials/ms-sql/sysprocs/sp_help.sql"
    parseFile "materials/ms-sql/sysprocs/test.sql"

    0 // return an integer exit code
