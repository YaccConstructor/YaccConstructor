module Yard.Frontends.FsYaccFrontend.Main

open Yard.Frontends.FsYaccFrontend.Lexer
open Yard.Frontends.FsYaccFrontend.Parser
open Yard.Core.IL

open Microsoft.FSharp.Text.Lexing

let ParseFile fileName =
    let content = System.IO.File.ReadAllText(fileName)
    Lexer.source := content
    let reader = new System.IO.StringReader(content)
    let lexbuf = LexBuffer<_>.FromTextReader reader
    let (grammar, terminals) = Parser.s Lexer.token lexbuf
    let terminalsDescr = (terminals |> Seq.fold (fun acc (KeyValue(k,v)) -> acc + (sprintf "%s :\n%s\n\n"  k v)) "(*\nYou need to describe following terminals in lexer:\n") + "*)"
    {new Definition.t<Source.t, Source.t> with info = {new Definition.info with fileName = ""} and head = Some(terminalsDescr, (0,0)) and grammar = grammar and foot = None}

