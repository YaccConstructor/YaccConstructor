//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

module Yard.Core.Conversions.Linearize

open Yard.Core
open Yard.Core.IL
open TransformAux
open Yard.Core.IL.Production
open System.Collections.Generic
open Mono.Addins

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
                        if decls.[old].Count = 1 || name = "" then old
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
            elif name.text = errorToken then name
            else
                try
                    let ruleModule = rulesMap.[module'].[name.text]
                    new Source.t (namesDict.[ruleModule].[name.text], name)
                with
                | :? KeyNotFoundException ->
                    if not <| rulesMap.ContainsKey module' then
                        failwithf "There is no module %s" module'
                    elif not <| rulesMap.[module'].ContainsKey name.text then
                        failwithf "Rule %s is not accessable from module %s" name.text module'
                    else failwith "Something was not found"
                

    let renamebody module' metaArgs body =
        let inline rename name = rename module' metaArgs name
        let rec renamebody' = function
            | PRef (name, args) -> PRef (rename name, args)
            | PMetaRef (name, args, metas) ->
                PMetaRef (rename name, args, List.map renamebody' metas)
            | PAlt (left, right) -> PAlt (renamebody' left, renamebody' right)
            | PConj (left, right) -> PConj (renamebody' left, renamebody' right)
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



[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type Linearize() = 
    inherit Conversion()
        override this.Name = "Linearize"
        override this.ConvertGrammar (grammar,_) = linearize grammar
