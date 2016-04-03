//  Copyright 2010-2011 Konstantin Ulitin
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Yard.Frontends.AntlrFrontend.Main

open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text
open Yard.Frontends.AntlrFrontend.Lexer
open Yard.Frontends.AntlrFrontend.Parser
open Yard.Core.IL
open Yard.Core.IL.Definition



let ParseFile fileName : t<Source.t, Source.t> =
    let content = System.IO.File.ReadAllText fileName
    Lexer.source := content
    let reader = new System.IO.StringReader(content)
    let lexbuf = LexBuffer<_>.FromTextReader reader
    let (grammar, terminals) = ParseAntlr Lexer.main lexbuf
    let terminalsDescr =
        terminals |> Seq.fold
            (fun acc (KeyValue(k,v)) -> acc + (sprintf "%s :\n%s\n\n"  k v))
            "(*\nYou need to describe following terminals in lexer:\n"
        |> (fun x -> x + "*)")
    {empty with head = Some <| new Source.t(terminalsDescr); grammar = grammar}

let run () =
    let testPath = ref @"..\..\..\..\Tests\ANTLR"
    let testFile = ref "calc.g4"

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
(*  let lexems = seq { 
                       while not lexbuf.IsPastEndOfStream do
                             yield Lexer.main lexbuf  
                      }
    lexems |> Seq.iter (fun x -> printf "%A\n" x) *) 
    let (a,b) = ParseAntlr Lexer.main lexbuf
    //printfn "%A" a
    b |> Seq.iter (fun x -> printf "%A\n" x)
    ()
