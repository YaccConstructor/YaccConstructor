module Yard.Frontends.AntlrFrontend.Main

open Microsoft.FSharp.Text.Lexing

open Yard.Frontends.AntlrFrontend.Lexer
open Yard.Frontends.AntlrFrontend.Parser
open Yard.Core.IL



let ParseFile fileName =
    let content = System.IO.File.ReadAllText(fileName)
    Lexer.source := content
    let reader = new System.IO.StringReader(content)
    let lexbuf = LexBuffer<_>.FromTextReader reader
    let (grammar, terminals) = ParseAntlr Lexer.main lexbuf
    let terminalsDescr = (terminals |> Seq.fold (fun acc (KeyValue(k,v)) -> acc + (sprintf "%s :\n%s\n\n"  k v)) "(*\nYou need to describe following terminals in lexer:\n") + "*)"
    {new Definition.t<Source.t, Source.t> with info = {new Definition.info with fileName = ""} and head = Some(terminalsDescr, (0,0)) and grammar = grammar and foot = None}

let run =
    let testPath = ref @"..\..\..\..\Tests\ANTLR"
    let testFile = ref "c.g"

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
    let lexems = seq { 
                       while not lexbuf.IsPastEndOfStream do
                             yield Lexer.main lexbuf  
                      }
    lexems |> Seq.iter (fun x -> printf "%A\n" x)
//    let (a,b) = ParseAntlr Lexer.main lexbuf
//    b |> Seq.iter (fun x -> printf "%A\n" x)
//    printfn "%A" b
    ()
