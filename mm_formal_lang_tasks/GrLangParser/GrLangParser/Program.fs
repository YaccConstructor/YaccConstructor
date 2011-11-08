open Microsoft.FSharp.Text.Lexing
open Yard.Generators.GNESCCGenerator
open Yard.Generators.GNESCCGenerator.Tables
open Microsoft.FSharp.Text.Lexing

let check grammar = 
    Checker.IsChomskyNormalForm grammar

let toFsYacc grammar =
    let fsYaccPrinter = new Yard.Generators.FsYaccPrinter.FsYaccPrinter()
    fsYaccPrinter.Generate grammar

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

    match List.ofSeq result with
    | (Success r)::tl ->  
       printfn "Is in normal form: %A" (check (r :?> _))
       toFsYacc  (r :?> _)
    | _ -> failwith "Attribte calculation problem"
    
do 
    run @"D:\projects\yc\recursive-ascent\mm_formal_lang_tasks\GrLangParser\GrLangParser\tests\t2"
    |> ignore
    System.Console.ReadLine() |> ignore

