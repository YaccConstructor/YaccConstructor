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
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

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
    let allTokens = _findTokens (grammar |> List.map (fun rule -> rule.body)) 
    List.fold (fun unique token -> if List.exists ((=) token) unique then unique else token::unique)
        [] allTokens

let findStartRules (grammar:Rule.t<Source.t, Source.t> list) =
    grammar |> List.choose (fun rule -> if rule._public then Some(fst rule.name) else None)
   
let indentedListL = List.reduce (---) 

let fsYaccRule (yardRule : Rule.t<Source.t, Source.t>) = 
    let lineNumber = ref 0
    let actionCodeFunPrefix = 
        if yardRule.args.IsEmpty then ""
        else  (String.concat "" (List.map (fst >> sprintf "fun %s -> ") yardRule.args))
    let rec layoutProduction isOnTop = function
        | PAlt(left, right) -> aboveL (layoutProduction isOnTop left) (layoutProduction isOnTop right)
        | PSeq(elements, actionCode) -> 
            let bindings =
                List.filter
                    (fun (_,elem) -> elem.binding.IsSome) <| List.mapi (fun i x -> (i+1,x))
                    elements
            let actionCodePrefix =
                bindings
                |> List.fold
                    (fun state (i, elem) ->
                        let args =
                            match elem.rule with
                            | PRef (_,Some args) -> fst args
                            | _ -> ""
                        sprintf "%slet %s=$%d %s in " state (fst elem.binding.Value) i args)
                    ""
            lineNumber := !lineNumber + 1
            indentedListL (
                if !lineNumber = 1 then wordL "" else wordL "|"
                ::(elements |> List.map (fun elem -> layoutProduction false elem.rule)) 
                @ (if actionCode = None then [wordL "{ }"]
                   else [wordL ("{ " + actionCodeFunPrefix + actionCodePrefix + fst actionCode.Value + "}")]))
        //| PRef(("empty",_),_) -> leftL ""
        | PToken(src)
        | PRef(src, _) ->
            if not isOnTop then wordL (fst src)
            else wordL <| (fst src) + "{ " + actionCodeFunPrefix + "$1 }"
        | PMany _ -> wordL "$UNEXPECTED MANY$"
        | PMetaRef _ -> wordL "$UNEXPECTED META_REF$"
        | PLiteral _ -> wordL "$UNEXPECTED LITERAL$"
        | PRepet _ -> wordL "$UNEXPECTED REPET$"
        | PPerm _ -> wordL "$UNEXPECTED PERM$"
        | PSome _ -> wordL "$UNEXPECTED SOME$"
        | POpt _ -> wordL "$UNEXPECTED OPT$"
//        | _ -> wordL "$UNEXPECTED$"
    let layout = (^^) (wordL (fst yardRule.name + " :")) (layoutProduction true yardRule.body)
    Display.layout_to_string FormatOptions.Default layout

let generate2 (ilDef:Definition.t<Source.t, Source.t>) tokenType =
    let headerSection = if ilDef.head.IsSome then sprintf "%%{\n%s\n%%}\n" (fst ilDef.head.Value) else ""
    let tokens = findTokens ilDef.grammar
    // TODO: use String.concat instead of fold+sprintf
    let tokensSection = sprintf "%s\n" (List.fold (fun text token -> sprintf "%s%%token <%s> %s\n" text (tokenType token) token) "" tokens)
    let startRules = findStartRules ilDef.grammar
    let startRulesSection = sprintf "%s\n" (List.fold (fun text start -> sprintf "%s%%start %s\n" text start) "" startRules)
    let typesSection = sprintf "%s\n" (List.fold (fun text start -> sprintf "%s%%type <'a> %s\n" text start) "" startRules)
    let rulesSection  = sprintf "%%%%\n\n%s\n" (String.concat "\n\n" (List.map fsYaccRule ilDef.grammar))
    let footerSection = if ilDef.foot.IsSome then sprintf "%%%%\n%s\n" (fst ilDef.foot.Value) else ""
    headerSection + tokensSection + startRulesSection + typesSection + rulesSection + footerSection

let generate ilDef tokenType =
  generate2 ilDef (fun _ -> tokenType)
