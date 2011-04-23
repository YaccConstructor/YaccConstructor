module Yard.Frontends.FsYaccFrontend.Main

open Yard.Frontends.FsYaccFrontend.Lexer
open Yard.Frontends.FsYaccFrontend.Parser
open Yard.Core.IL

open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Core

let addStarts starts (grammar: Grammar.t<Source.t, Source.t>) = 
    grammar |> List.map (fun rule -> if List.exists ((=) rule.name) starts then { rule with _public=true } else rule)

let ParseFile fileName =
    let content = System.IO.File.ReadAllText(fileName)
    Lexer.source := content
    let reader = new System.IO.StringReader(content)
    let lexbuf = LexBuffer<_>.FromTextReader reader
    let (res:System.Tuple<string option, string list, string list, Grammar.t<Source.t, Source.t>>) = Parser.s Lexer.token lexbuf
    
//    let a = fst res
//    let a = Parser.s Lexer.token lexbuf
//    let terminalsDescr = (terminals |> Seq.fold (fun acc (KeyValue(k,v)) -> acc + (sprintf "%s :\n%s\n\n"  k v)) "(*\nYou need to describe following terminals in lexer:\n") + "*)"
    let defHead = match res.Item1 with Some(str) -> Some(str, (0,0)) | _ -> None
    { new Definition.t<Source.t, Source.t> with info = {new Definition.info with fileName = ""} and head = defHead and grammar = addStarts res.Item3 res.Item4 and foot = None }
//    {new Definition.t<Source.t, Source.t> with info = {new Definition.info with fileName = ""} and head = None and grammar = [] and foot = None}

