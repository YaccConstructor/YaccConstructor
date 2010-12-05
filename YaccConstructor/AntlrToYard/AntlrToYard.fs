open ANTLR_expand_mlc 
open Lexer
open Microsoft.FSharp.Text
open FParsec.CharParsers

let testParser = function
    | Success (v, _, _)  -> printfn "%s" (v.ToString())
    | Failure (msg, err, _) -> printf "%s" msg
    | _ -> printf "???"


let () =

    let testPath = ref "."
    let testFile = ref "cgrammar.g"

    let commandLineSpecs =
        ["--testpath", ArgType.String (fun s -> testPath := s), "Directory where test files are placed"
         "-t", ArgType.String (fun s -> testFile := s), "Name of test file"
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    let commandLineArgs = System.Environment.GetCommandLineArgs()
    ArgParser.Parse commandLineSpecs

    let k = run multiline_comment @"/* sdf */"
    testParser k
    ()
