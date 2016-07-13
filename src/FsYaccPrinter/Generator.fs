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
open YC.PrettyPrinter.Pretty
open YC.PrettyPrinter.StructuredFormat

let findTokens (grammar:Rule.t<Source.t, Source.t> list) = 
    let rec _findTokens productions = 
        List.collect 
            (function
            | PSeq(elements, actionCode, _) -> _findTokens (List.map (fun elem -> elem.rule) elements)
            | PAlt(x, y) -> _findTokens [x] @ _findTokens [y]
            | PToken(source) -> [source.text]
            | _ -> []
            )
            productions
    let allTokens = _findTokens (grammar |> List.map (fun rule -> rule.body)) 
    List.fold (fun unique token -> if List.exists ((=) token) unique then unique else token::unique)
        [] allTokens

let findStartRules (rules : Rule.t<Source.t, Source.t> list) =
    rules |> List.choose (fun rule -> if rule.isStart then Some rule.name.text else None)
   
let indentedListL = List.reduce (---) 

let fsYaccRule (yardRule : Rule.t<Source.t, Source.t>) = 
    let lineNumber = ref 0
    let actionCodeFunPrefix = 
        if yardRule.args.IsEmpty then ""
        else  yardRule.args |> List.map (fun x -> sprintf "fun %s -> " x.text) |> String.concat ""
    let rec layoutProduction isOnTop = function
        | PAlt(left, right) ->
            let leftL = layoutProduction isOnTop left
            aboveL leftL (layoutProduction isOnTop right)
        | PSeq(elements, actionCode, _) -> 
            let bindings =
                elements
                |> List.mapi (fun i x -> (i+1,x))
                |> List.choose (fun (i,elem) -> elem.binding |> Option.map (fun x -> i, elem, x))
            let actionCodePrefix =
                bindings
                |> List.fold
                    (fun state (i, elem, (bind : Source.t)) ->
                        let args =
                            match elem.rule with
                            | PRef (_,Some (args : Source.t)) -> args.text
                            | _ -> ""
                        sprintf "%slet %s=$%d %s in " state bind.text i args)
                    ""
            lineNumber := !lineNumber + 1
            indentedListL (
                if !lineNumber = 1 then wordL "" else wordL "|"
                ::(elements |> List.map (fun elem -> layoutProduction false elem.rule)) 
                @ (if actionCode = None then [wordL "{ }"]
                   else [wordL ("{ " + actionCodeFunPrefix + actionCodePrefix + actionCode.Value.text + "}")]))
        //| PRef(("empty",_),_) -> leftL ""
        | PToken(src)
        | PRef(src, _) ->
            if not isOnTop then wordL src.text
            else wordL <| src.text + "{ " + actionCodeFunPrefix + "$1 }"
        | PMany _ -> wordL "$UNEXPECTED MANY$"
        | PMetaRef _ -> wordL "$UNEXPECTED META_REF$"
        | PLiteral _ -> wordL "$UNEXPECTED LITERAL$"
        | PRepet _ -> wordL "$UNEXPECTED REPET$"
        | PPerm _ -> wordL "$UNEXPECTED PERM$"
        | PSome _ -> wordL "$UNEXPECTED SOME$"
        | POpt _ -> wordL "$UNEXPECTED OPT$"
//        | _ -> wordL "$UNEXPECTED$"
    let layout = (^^) (wordL (yardRule.name.text + " :")) (layoutProduction true yardRule.body)
    print 80 layout

let generate2 (ilDef : Definition.t<Source.t, Source.t>) tokenType =
    let headerSection = match ilDef.head with Some v -> sprintf "%%{\n%s\n%%}\n" v.text | None -> ""
    let tokens = findTokens ilDef.grammar.Head.rules
    // TODO: use String.concat instead of fold+sprintf
    let tokensSection =
        tokens |> List.fold
            (fun text token -> sprintf "%s%%token <%s> %s\n" text (tokenType token) token)
            ""
        |> sprintf "%s\n"
    let startRules = findStartRules ilDef.grammar.Head.rules
    let startRulesSection = sprintf "%s\n" (List.fold (fun text start -> sprintf "%s%%start %s\n" text start) "" startRules)
    let typesSection = sprintf "%s\n" (List.fold (fun text start -> sprintf "%s%%type <'a> %s\n" text start) "" startRules)
    let rulesSection  = sprintf "%%%%\n\n%s\n" (String.concat "\n\n" (List.map fsYaccRule ilDef.grammar.Head.rules))
    let footerSection = match ilDef.foot with Some v -> sprintf "%%%%\n%s\n" v.text | None -> ""
    headerSection + tokensSection + startRulesSection + typesSection + rulesSection + footerSection

let generate ilDef tokenType =
  generate2 ilDef (fun _ -> tokenType)
