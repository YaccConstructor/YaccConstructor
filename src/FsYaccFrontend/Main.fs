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

module YC.Frontends.FsYaccFrontend.Main

open YC.Frontends.FsYaccFrontend.Lexer
open YC.Frontends.FsYaccFrontend.Parser
open YC.Core.IL

open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Core
open System.Text.RegularExpressions

let addStarts starts (grammar: Grammar<Source, Source>) = 
    grammar |> List.map (fun m ->
        {m with rules = m.rules |> List.map (fun rule ->
            if List.exists (fun (x : Source) -> x.text = rule.name.text) starts
            then { rule with isStart=true }
            else rule
        )}
    )

let rec _addBindings = function
    | PSeq(elements, Some (ac : Source), l) -> 
        (elements |> List.mapi (fun i elem -> 
            if Regex.Match(ac.text, sprintf "\\$%d([^\\d]|$)" (i+1)).Success then 
                { elem with rule=(_addBindings elem.rule) ; binding=Some <| new Source(sprintf "_S%d" (i+1), ac) } 
            else 
                { elem with rule=_addBindings elem.rule} 
            )
         , Some <| new Source(Regex.Replace(ac.text, "\\$(\\d+)", "_S$1"), ac)
         , l
        ) |> PSeq
    | PSeq(elements, None, l) -> PSeq(List.map (fun elem -> { elem with rule=_addBindings elem.rule} ) elements, None, l)
    | PAlt(left, right) -> PAlt(_addBindings left, _addBindings right)
    | PConj(left, right) -> PAlt(_addBindings left, _addBindings right)
    | PSome(x) -> PSome(_addBindings x)
    | POpt(x) -> POpt(_addBindings x)
    | PMany(x) -> PMany(_addBindings x)
    | x -> x

let addBindings (grammar: Grammar<Source, Source>) = 
    grammar |> YC.Core.Helpers.mapGrammar (List.map (fun rule -> { rule with body=_addBindings rule.body } ))

let LexBufferFromFile fileName = 
    let content = System.IO.File.ReadAllText fileName
    let reader = new System.IO.StringReader(content)
    Lexer.currentFile := fileName
    Lexer.source := content
    let lexbuf = LexBuffer<_>.FromTextReader reader
    lexbuf.EndPos <- lexbuf.EndPos.NextLine
    lexbuf

let LexBufferFromString grammarStr = 
    Lexer.currentFile := "Grammar from string"
    Lexer.source := grammarStr
    let lexbuf = LexBuffer<_>.FromString grammarStr
    lexbuf.EndPos <- lexbuf.EndPos.NextLine
    lexbuf

let Parse lexbuf ilInfo = 
    try 
        let (res : Source option * Source list * Source list * Grammar<Source, Source>) = Parser.s Lexer.token lexbuf
        let defHead, _, it3, it4 = res
        { emptyGrammarDefinition
            with info = {fileName = ilInfo}
                 head = defHead
                 grammar = addBindings <| addStarts it3 it4
            }
    with e -> // when e.Message="parse error" -> 
        fprintfn stderr "%A" e
        let pos = lexbuf.EndPos
        let extendedMessage =
            sprintf "error near line %d, character %d\nlast token: %s\n\n%s" pos.pos_lnum
                (pos.pos_cnum - pos.pos_bol) (new System.String(lexbuf.Lexeme)) (e.ToString())
        failwith extendedMessage

let ParseFile fileName =
    Parse (LexBufferFromFile fileName) fileName
    
let ParseString str = 
    Parse (LexBufferFromString str) "Grammar from string"