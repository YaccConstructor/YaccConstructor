module AntlrToYard.Main

open Microsoft.FSharp.Text.Lexing

open AntlrToYard.Lexer
open AntlrToYard.Parser
open Yard.Core.IL

let () =
    let testPath = ref @"..\.."
    let testFile = ref "calc.g"

    let commandLineSpecs =
        ["--testpath", ArgType.String (fun s -> testPath := s), "Directory where test files are placed"
         "-t", ArgType.String (fun s -> testFile := s), "Name of test file"
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    let commandLineArgs = System.Environment.GetCommandLineArgs()
    ArgParser.Parse commandLineSpecs

    printfn "Start.."
    let content = System.IO.File.ReadAllText(!testPath + "\\" + !testFile)
    Lexer.source := content
    let reader = new System.IO.StringReader(content)
    let lexbuf = LexBuffer<_>.FromTextReader reader//LexBuffer<_>.FromChars  ("abc/* def */foo".ToCharArray())
//    let lexems = seq { 
//                       while not lexbuf.IsPastEndOfStream do
//                             yield Lexer.main lexbuf  
//                      }
//    lexems |> Seq.iter (fun x -> printf "%A\n" x)
    let c = ParseAntlr Lexer.main lexbuf
    printfn "%A" c
    ()
