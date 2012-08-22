//  Module ReplaceInline replace nonterminals, from which only
//    Pref, PToken or PLiteral can be infered, whit their right sides.
//
//  Copyright 2011, 2012 Avdyukhin Dmitry
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

module Yard.Core.Convertions.ExpandInline

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic

let private replaceInline (rules : Rule.t<_,_> list) =
    let closure (inlines : (string * t<_,_>) list) = 
        let inlinesDict = inlines |> dict
        let getName = function
            | PRef((n,_),_) | PToken (n,_) | PLiteral (n,_) -> n
            | x -> ""
        [for (k,v) in inlines do
            let cur = ref v
            while getName !cur |> inlinesDict.ContainsKey do
                cur := inlinesDict.[getName !cur]
            yield (k,!cur)] |> dict
    let inlines = 
        rules
        |> List.fold
            (fun inlines cur ->
                match cur.body with
                | PRef (_, None) | PToken _ | PLiteral _ ->
                    (cur.name, cur.body)::inlines
                | _ -> inlines)
            []
        |> closure
    
    let rec modifyBody = function
        | PSeq (elems, ac, l) ->
            let newElems =
                elems |> List.map (fun x -> {x with rule = modifyBody x.rule})
            PSeq(newElems, ac, l)
        | PAlt (l,r) -> PAlt(modifyBody l, modifyBody r)
        | PRef ((name,_),_) as prev ->
            if inlines.ContainsKey name then inlines.[name]
            else prev
        | PMetaRef ((name,_),_,_) as prev ->
            if inlines.ContainsKey name then inlines.[name]
            else prev
        | PMany x -> PMany <| modifyBody x
        | PSome x -> PSome <| modifyBody x
        | POpt x -> POpt <| modifyBody x
        | x -> x
        
    rules
    |> List.choose
        (fun rule -> 
            if inlines.ContainsKey rule.name && not rule._public
            then None
            else Some <| {rule with body = modifyBody rule.body})
            
type ReplaceInline() = 
    inherit Convertion()
        override this.Name = "ReplaceInline"
        override this.ConvertList (ruleList,_) = replaceInline ruleList
        override this.EliminatedProductionTypes = [""]