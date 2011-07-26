//  Module AddDefaultAC contains:
//  - function, which adds action code to used productions which haven't any.
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

module Yard.Core.Convertions.AddDefaultAC

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

open System.Collections.Generic

/// Adds action code to production considering it is used somewhere
let rec addAcToProduction neededRules = function
    | PSeq(elements, Some(ac)) -> 
        PSeq(
            elements 
            |> List.map 
                (fun elem ->
                    match elem.binding with
                    | Some(binding) -> { elem with rule=addAcToProduction neededRules elem.rule }
                    | None -> elem
                )
            , Some(ac)
        )
    | PSeq(elements, None) -> 
        let getBinding i elem =
            if elem.omit 
            then None
            else Some(sprintf "S%d" (i+1), (0,0))
        PSeq(
            elements |> List.mapi (fun i elem ->  {elem with binding=getBinding i elem; rule=addAcToProduction neededRules elem.rule} )
            , Some(elements |> List.mapi getBinding |> List.choose id |> List.map fst |> String.concat ", ", (0,0))       
        )
    | PAlt(left, right) -> PAlt(addAcToProduction neededRules left, addAcToProduction neededRules right)
    | PRef((ref,(_,_)), _) as x -> neededRules := ref::!neededRules; x
    | PLiteral _ as x -> x
    | PToken _ as x -> x
    | PSome _ as x -> x
    | PMany _ as x -> x
    | POpt _ as x -> x
    | _ -> failwith "EORRR"


let addDefaultAC (ruleList: Rule.t<Source.t, Source.t> list)  = 
    let updatedRules = new HashSet<string>()
    let rulesQueueBfs = new System.Collections.Generic.Queue<string>()
    let rulesMap = new Dictionary<string, Rule.t<Source.t, Source.t>>()
    ruleList |> List.iter 
        (fun rule -> 
            rulesMap.Add(rule.name, rule); 
            if rule._public then (rulesQueueBfs.Enqueue(rule.name) |> ignore)
        ) 
    while rulesQueueBfs.Count > 0 do
        let bfsFor = rulesQueueBfs.Dequeue()
        if not(updatedRules.Contains(bfsFor)) then 
            let ruleFor = rulesMap.[bfsFor]
            let neededRules = ref []
            let updatedBody = addAcToProduction neededRules (ruleFor.body)
            !neededRules |> List.iter (fun r -> if not (updatedRules.Contains(r)) then rulesQueueBfs.Enqueue(r))
            rulesMap.[bfsFor] <- { ruleFor with body=updatedBody}
    ruleList |> List.map (fun rule -> let ruleRef = ref rule in rulesMap.TryGetValue(rule.name ,ruleRef) |> ignore; !ruleRef)

type AddDefaultAC() = 
    inherit Convertion()
        override this.Name = "AddDefaultAC"
        override this.ConvertList ruleList = addDefaultAC ruleList 
        override this.EliminatedProductionTypes = [""]

