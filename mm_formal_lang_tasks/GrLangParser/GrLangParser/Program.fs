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
        
    let forest =    
        let ti = new TableInterpreter(tables)
        ti.Run l

    let result =    
        let r = 
            Seq.map 
                (ASTInterpretator.interp GNESCC.Actions.ruleToAction GNESCC.Regexp.ruleToRegex)
                forest
        r |> List.ofSeq
    
    let toFsYacc =
        let fsYaccPrinter = Yard.Generators.FsYaccPrinter.FsYaccPrinter()
        result 
        |> List.map (function | Success r -> (let x = fsYaccPrinter.Generate(r:?> Yard.Core.IL.Definition.t<Yard.Core.IL.Source.t,Yard.Core.IL.Source.t>) in printfn "%A" x)  | _ ->())

    printfn "Result %A\n" result
   // toFsYacc()
    
do 
    run @"..\..\tests\t1"
    |> ignore
    System.Console.ReadLine() |> ignore
