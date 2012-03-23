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

module Yard.Frontends.FsYaccFrontend.Main

open Yard.Frontends.FsYaccFrontend.Lexer
open Yard.Frontends.FsYaccFrontend.Parser
open Yard.Core.IL
open Yard.Core.IL.Production

open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Core
open System.Text.RegularExpressions

let addStarts starts (grammar: Grammar.t<Source.t, Source.t>) = 
    grammar |> List.map (fun rule -> if List.exists ((=) rule.name) starts then { rule with _public=true } else rule)

let rec _addBindings = function
    | PSeq(elements, Some(ac,_)) -> 
        (elements
         |> List.mapi 
            (
                fun i elem -> 
                    if Regex.Match(ac, sprintf "\\$%d([^\\d]|$)" (i+1)).Success then 
                        { elem with rule=(_addBindings elem.rule) ; binding=Some((sprintf "_S%d" (i+1)),(0,0)) } 
                    else 
                        { elem with rule=_addBindings elem.rule} 
            ) 
        , Some(Regex.Replace(ac, "\\$(\\d+)", "_S$1"), (0,0)))
        |> PSeq
    | PSeq(elements, None) -> PSeq(List.map (fun elem -> { elem with rule=_addBindings elem.rule} ) elements, None)
    | PAlt(left, right) -> PAlt(_addBindings left, _addBindings right)
    | PSome(x) -> PSome(_addBindings x)
    | POpt(x) -> POpt(_addBindings x)
    | PMany(x) -> PMany(_addBindings x)
    | x -> x
let addBindings (grammar: Grammar.t<Source.t, Source.t>) = 
    grammar |> List.map (fun rule -> { rule with body=_addBindings rule.body } )

let ParseFile fileName =
    let content = System.IO.File.ReadAllText(fileName)
    Lexer.source := content
    let reader = new System.IO.StringReader(content)
    let lexbuf = LexBuffer<_>.FromTextReader reader
    try 
        let (res:System.Tuple<string option, string list, string list, Grammar.t<Source.t, Source.t>>) = Parser.s Lexer.token lexbuf
        let defHead = match res.Item1 with Some(str) -> Some(str, (0,0)) | _ -> None
        { new Definition.t<Source.t, Source.t> with info = {new Definition.info with fileName = ""} and head = defHead and grammar = addBindings (addStarts res.Item3 res.Item4) and foot = None and options = Map.empty}
    with e -> // when e.Message="parse error" -> 
        let pos = lexbuf.EndPos
        let extendedMessage = sprintf "error near line %d, character %d\nlast token: %s\n\n%s" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (new System.String(lexbuf.Lexeme)) (e.ToString())
        failwith extendedMessage