//  Copyright 2009-2011 Konstantin Ulitin
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
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

module Yard.Generators.FsYaccPrinter.Generator

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Microsoft.FSharp.Text.StructuredFormat

let findTokens (grammar:Rule.t<Source.t, Source.t> list) = 
    let rec _findTokens productions = 
        List.collect 
            (function
            | PSeq(elements, actionCode) -> _findTokens (List.map (fun elem -> elem.rule) elements)
            | PAlt(x, y) -> _findTokens [x] @ _findTokens [y]
            | PToken(source) -> [fst source]
            | _ -> []
            )
            productions
    _findTokens (grammar |> List.map (fun rule -> rule.body)) 

let findStartRules (grammar:Rule.t<Source.t, Source.t> list) = grammar |> List.choose (fun rule -> if rule._public then Some(rule.name) else None)

    
let fsYaccRule (yardRule:Rule.t<Source.t, Source.t>) = 
    yardRule.name + ": TODO\n"

let generate (ilDef:Definition.t<Source.t, Source.t>) = 
    let headerSection = if ilDef.head.IsSome then sprintf "%%{\n%s\n%%}\n" (fst ilDef.head.Value) else ""
    let tokens = findTokens ilDef.grammar
    let tokensSection = sprintf "%s\n" (List.fold (fun text token -> sprintf "%s%%token %s\n" text token) "" tokens)
    let startRules = findStartRules ilDef.grammar
    let startRulesSection = sprintf "%s\n" (List.fold (fun text start -> sprintf "%s%%start %s\n" text start) "" startRules)
    let typesSection = sprintf "%s\n" (List.fold (fun text start -> sprintf "%s%%type <string> %s\n" text start) "" startRules)
    let rulesSection  = sprintf "%%%%\n\n%s\n" (String.concat "\n" (List.map fsYaccRule ilDef.grammar))
    let footerSection = if ilDef.foot.IsSome then sprintf "%%%%\n%s\n" (fst ilDef.foot.Value) else ""
    headerSection + tokensSection + startRulesSection + typesSection + rulesSection + footerSection