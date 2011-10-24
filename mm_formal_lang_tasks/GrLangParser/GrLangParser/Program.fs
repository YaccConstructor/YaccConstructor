open Microsoft.FSharp.Text.Lexing
open Yard.Generators.GNESCCGenerator
open Yard.Generators.GNESCCGenerator.Tables
open Microsoft.FSharp.Text.Lexing

let run path =
    //Create lexer
    let content = System.IO.File.ReadAllText(path)
    let reader = new System.IO.StringReader(content)    
    let buf = LexBuffer<_>.FromTextReader reader
    let l = Lexer.Lexer(buf)

    //Create tables
    let tables = tables
    
    //Run parser
    // forest -- dirivation forest    
    let forest =    
        let ti = new TableInterpreter(tables)
        ti.Run l

    let result =
        //run forest interpretation (action code calculation)
        let r = 
            Seq.map 
                (ASTInterpretator.interp GNESCC.Actions.ruleToAction GNESCC.Regexp.ruleToRegex)
                forest
        r
                
    printfn "Result %A\n" result
    
do 
    run @"D:\YC\recursive-ascent\mm_formal_lang_tasks\GrLangParser\GrLangParser\tests\t1"
    |> ignore
    System.Console.ReadLine() |> ignore

