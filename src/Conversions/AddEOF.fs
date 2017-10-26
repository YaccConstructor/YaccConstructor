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

module Yard.Core.Conversions.AddEOF

open Yard.Core
open Yard.Core.IL


open System.Collections.Generic

let dummyPos s = new Source(s)
let dummyToken s = PToken <| new Source(s)

let lastName = ref ""
let createName() =
    lastName := Namer.newName "start"
    !lastName

let rec eachProduction f productionList =
    List.iter (function    
        | PSeq(elements, actionCode, l) ->
            f <| PSeq(elements, actionCode, l)
            elements |> List.map (fun elem -> elem.rule) |> eachProduction f
        | PAlt(left, right) ->
            f <| PAlt(left, right)
            eachProduction f [left; right]
        | PConj(left, right) ->
            f <| PConj(left, right)
            eachProduction f [left; right]
        | PMany x ->
            f <| PMany x
            eachProduction f [x]
        | PSome x ->
            f <| PSome x
            eachProduction f [x]
        | POpt x ->
            f <| POpt x
            eachProduction f [x]
        | x -> f x
    ) productionList 

let addEOFToProduction = function
    | PSeq(elements, actionCode, l) -> 
        (
            elements 
            @ [{omit=true; rule=dummyToken "EOF"; binding=None; checker=None}]
            ,actionCode, l
        ) |> PSeq
    | x -> (
                [
                    {omit=false; rule=x; binding=None; checker=None}; 
                    {omit=true; rule=dummyToken "EOF"; binding=None; checker=None}
                ]
                ,None, None
           ) |> PSeq

let addEOF (ruleList: Rule<Source, Source> list) = 
    let startRules = new HashSet<string>()
    ruleList |> List.iter
        (fun rule -> if rule.isStart then startRules.Add rule.name.text |>ignore )
    let usedRules = new HashSet<string>()
    ruleList |> List.map (fun rule -> rule.body) 
    |> eachProduction (function
        | PRef(name,_) -> usedRules.Add name.text |>ignore
        | _ -> ()
    )
    let usedStartRules = new HashSet<string>(startRules)
    usedStartRules.IntersectWith(usedRules)
    ruleList |> List.collect (fun rule -> 
        if rule.isStart then
            if usedStartRules.Contains rule.name.text then
                [{rule with isStart=false
                }; {
                    name= dummyPos <| createName()
                    args=[]
                    isStart=true
                    isPublic=false
                    isInline = false
                    metaArgs=[] 
                    body=   [{
                                omit=false
                                rule=PRef (dummyPos rule.name.text, None)
                                binding= !lastName |> dummyPos |> Some
                                checker=None
                            }; {
                                omit=false
                                rule=dummyToken "EOF"
                                binding=None
                                checker=None
                            }]
                            |> fun elems -> PSeq (elems, !lastName |> dummyPos |> Some, None)
                }]
            else
                [{rule with body=(addEOFToProduction rule.body)}]
        else
            [rule]
    )  

type AddEOF() = 
    inherit Conversion()
        override this.Name = "AddEOF"
        override this.ConvertGrammar (grammar,_) = mapGrammar addEOF grammar
