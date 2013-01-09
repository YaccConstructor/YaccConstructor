//  Module Linearize.
//  Contains function, transforming multi-module grammar into single-module
//
//  Copyright 2013 Dmitry Avdyukhin
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

module Yard.Core.Conversions.Linearize

open Yard.Core
open Yard.Core.IL
open TransformAux
open Yard.Core.IL.Production
open System.Collections.Generic

let dummyPos s = new Source.t(s)

let private linearize (grammar: Grammar.t<_,_>) = 
    let rename =
        let decls = new Dictionary<_,_>()
        grammar |> List.iter (fun m ->
            let name = getModuleName m
            m.rules |> List.iter (fun rule ->
                let text = rule.name.text
                if not <| decls.ContainsKey text then
                    decls.[text] <- new HashSet<_>([name])
                else
                    decls.[text].Add name |> ignore
            )
        )

        let allNames = new HashSet<_>()

        let namesDict =
            grammar |> List.map (fun m ->
                let name = getModuleName m
                let map = new Dictionary<_,_>()
                m.rules |> List.iter (fun rule ->
                    let newName = 
                        let old = rule.name.text
                        if decls.[old].Count = 1 then name
                        else name + "_" + old
                    let result = 
                        let cur, curName = ref 2, ref newName
                        while allNames.Contains !curName do
                            curName := newName + string !cur
                            incr cur
                        !curName
                    allNames.Add result |> ignore
                    map.[rule.name.text] <- result
                )
                (name, map)
            )
            |> dict
        
        let rulesMap = getRulesMap grammar
        fun module' metaArgs (name : Source.t) ->
            if List.exists (fun (arg : Source.t) -> arg.text = name.text) metaArgs then name
            else
                let ruleModule = rulesMap.[module'].[name.text]
                new Source.t (namesDict.[ruleModule].[name.text], name)
    let renamebody module' metaArgs body =
        let inline rename name = rename module' metaArgs name
        let rec renamebody' = function
            | PRef (name, args) -> PRef (rename name, args)
            | PMetaRef (name, args, metas) ->
                PMetaRef (rename name, args, List.map renamebody' metas)
            | PAlt (left, right) -> PAlt (renamebody' left, renamebody' right)
            | PMany body -> PMany (renamebody' body)
            | PSome body -> PSome (renamebody' body)
            | POpt body -> POpt (renamebody' body)
            | PSeq (elems, ac, label) ->
                let newElems = 
                    elems |> List.map (fun elem -> {elem with rule = renamebody' elem.rule})
                PSeq (newElems, ac, label)
            | x -> x
        renamebody' body
    let newRules =
        grammar |> List.collect (fun m ->
            let name = getModuleName m
            m.rules |> List.map (fun rule ->
                {rule with
                    name = rename (getModuleName m) [] rule.name
                    body = renamebody (getModuleName m) rule.metaArgs rule.body
                }
            )
        )
    defaultModules newRules


type ExpandBrackets() = 
    inherit Conversion()
        override this.Name = "Linearize"
        override this.ConvertGrammar (grammar,_) = linearize grammar
        override this.EliminatedProductionTypes = [""]
