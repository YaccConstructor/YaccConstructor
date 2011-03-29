//  Module AddEOF contains:
//  - function, which adds EOF terminal to the end of start rule.
//  May add brackets.
//
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

module Yard.Core.Convertions.AddEOF

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

open System.Collections.Generic

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
    | PSeq(elements, actionCode) -> PSeq(
        elements @ [{new elem<Source.t, Source.t> with omit=true and rule=PToken("EOF",(0,0)) and binding=None and checker=None}], 
        actionCode)
    | x -> PSeq([
            {new elem<Source.t, Source.t> with omit=false and rule=x and binding=None and checker=None}; 
            {new elem<Source.t, Source.t> with omit=true and rule=PToken("EOF",(0,0)) and binding=None and checker=None}],
        None)

let addEOF (ruleList: Rule.t<Source.t, Source.t> list) = 
    let startRules = new HashSet<string>()
    ruleList |> List.iter (fun rule -> if rule._public then startRules.Add(rule.name);() )
    let usedRules = new HashSet<string>()
    eachProduction 
        (function
        | PRef((name,_),_) -> usedRules.Add(name);()
        | _ -> ()
        )
        (ruleList |> List.map (fun rule -> rule.body) )
    let usedStartRules = new HashSet<string>(startRules)
    usedStartRules.IntersectWith(usedRules)
    ruleList |> List.collect 
        (fun rule -> 
            if rule._public then
                if usedStartRules.Contains(rule.name) then
                    [{rule with _public=false}; 
                    {new Rule.t<Source.t, Source.t> with 
                        name=createName() and args=[] and _public=true and metaArgs=[] 
                        and body=PSeq([
                            {new elem<Source.t, Source.t> with omit=false and rule=PRef((rule.name, (0,0)), None) and binding=Some(getLastName(), (0,0)) and checker=None}; 
                            {new elem<Source.t, Source.t> with omit=false and rule=PToken("EOF", (0,0)) and binding=None and checker=None}
                            ],
                            Some(getLastName(),(0,0))
                        )
                    }]
                else
                    [{rule with body=(addEOFToProduction rule.body)}]
            else
                [rule]
        )  

type AddEOF() = 
    interface IConvertion with
        member this.Name = "AddEOF"
        member this.ConvertList ruleList = addEOF ruleList
        member this.EliminatedProductionTypes = [""]
    end