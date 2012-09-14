//  Module AddEOF contains:
//  - function, which adds EOF terminal to the end of start rule.
//  May add brackets.
//
//  Copyright 2009, 2011 Konstantin Ulitin
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

module Yard.Core.Convertions.AddEOF

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

open System.Collections.Generic

let dummyPos s = new Source.t(s)
let dummyToken s = PToken <| new Source.t(s)

let nameIndex = ref 0
let createName() = nameIndex := !nameIndex + 1 ;sprintf "yard_start_%d" !nameIndex
let getLastName() = sprintf "yard_start_%d" !nameIndex

let rec eachProduction f productionList =
    List.iter 
        (function    
        | PSeq(elements, actionCode) -> f(PSeq(elements, actionCode)); eachProduction f (List.map (fun elem -> elem.rule) elements)
        | PAlt(left, right) -> f(PAlt(left, right)); eachProduction f [left; right]
        | PMany(x) -> f(PMany(x)); eachProduction f [x]
        | PSome(x) -> f(PSome(x)); eachProduction f [x]
        | POpt(x) -> f(POpt(x)); eachProduction f [x]
        | x -> f(x)
        )
        productionList 

let addEOFToProduction = function
    | PSeq(elements, actionCode) -> 
        (
            elements 
            @ [{omit=true; rule=dummyToken "EOF"; binding=None; checker=None}]
            ,actionCode
        ) 
        |> PSeq
    | x -> (
                [
                    {omit=false; rule=x; binding=None; checker=None}; 
                    {omit=true; rule=dummyToken "EOF"; binding=None; checker=None}
                ]
                ,None
           )
           |> PSeq

let addEOF (ruleList: Rule.t<Source.t, Source.t> list) = 
    let startRules = new HashSet<string>()
    ruleList |> List.iter
        (fun rule -> if rule._public then startRules.Add rule.name.text |>ignore )
    let usedRules = new HashSet<string>()
    eachProduction 
        (function
        | PRef(name,_) -> usedRules.Add name.text |>ignore
        | _ -> ()
        )
        (ruleList |> List.map (fun rule -> rule.body) )
    let usedStartRules = new HashSet<string>(startRules)
    usedStartRules.IntersectWith(usedRules)
    ruleList |> List.collect 
        (fun rule -> 
            if rule._public then
                if usedStartRules.Contains rule.name.text then
                    [{rule with _public=false}; 
                    {
                        name= dummyPos (createName())
                        args=[]
                        _public=true
                        metaArgs=[] 
                        body=PSeq([
                                    {omit=false; rule=PRef(dummyPos rule.name.text, None); binding=Some(dummyPos <| getLastName()); checker=None}; 
                                    {omit=false; rule=dummyToken "EOF"; binding=None; checker=None}
                                  ]
                                  ,Some(dummyPos <| getLastName())
                        )
                    }]
                else
                    [{rule with body=(addEOFToProduction rule.body)}]
            else
                [rule]
        )  

type AddEOF() = 
    inherit Convertion()
        override this.Name = "AddEOF"
        override this.ConvertList (ruleList,_) = addEOF ruleList
        override this.EliminatedProductionTypes = [""]