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
open Yard.Core.Convertions.TransformAux

/// Adds action code to production considering it is used somewhere
let rec addAcToProduction neededRules ruleBody = 
    match ruleBody with
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
    | PSome p -> PSome(addAcToProduction neededRules p)
    | PMany p -> PMany(addAcToProduction neededRules p)
    | POpt p -> POpt(addAcToProduction neededRules p)
    //| PMetaRef(_,_,_) -> failwith "ERROR: PMetaRef unexpected in AddDefaultAC"
    | x -> failwithf "ERROR: %A unexpected in AddDefaultAC" x

let addDefaultAC (ruleList: Rule.t<Source.t, Source.t> list)  = 
    let updatedRules = new HashSet<string>()
    let rulesQueueBfs = new System.Collections.Generic.Queue<string>()
    let rulesMap = new Dictionary<string, Rule.t<Source.t, Source.t>>()
    ruleList |> List.iter 
        (fun rule -> 
            rulesMap.Add(rule.name, rule); 
            //if rule._public then (rulesQueueBfs.Enqueue(rule.name) |> ignore)
            rulesQueueBfs.Enqueue(rule.name) |> ignore
        ) 
    while rulesQueueBfs.Count > 0 do
        let bfsFor = rulesQueueBfs.Dequeue()
        if not <| updatedRules.Contains bfsFor then    
            //printfn "u: %s" bfsFor
            updatedRules.Add bfsFor |> ignore        
            let emptyRule = {Rule.t.name=""; Rule.t.args=[]; Rule.t.body=PSeq([], None);
                                Rule.t._public=false; Rule.t.metaArgs=[]}
            let ruleFor = ref emptyRule
            if rulesMap.TryGetValue(bfsFor, ruleFor) then
                // Some generators need to have a sequence on the top of body tree
                (*
                let rec bodyToSeq = function
                    | PSeq (_,_) as p -> p
                    | PAlt (l,r) -> PAlt(bodyToSeq l, bodyToSeq r)
                    | x -> PSeq([{rule = x; binding = createSource "yard_bind" |> Some;
                                    omit = false; checker = None}]
                                , Some(createSource "yard_bind"))
                                *)
                let neededRules = ref []
                let updatedBody =
                    addAcToProduction neededRules ((!ruleFor).body)
                    //|> bodyToSeq
                !neededRules |> List.iter (fun r -> if not (updatedRules.Contains(r)) then rulesQueueBfs.Enqueue(r))
                rulesMap.[bfsFor] <- { !ruleFor with body=updatedBody}
    ruleList
    |> List.map (fun rule ->
                    let ruleRef = ref rule in
                    rulesMap.TryGetValue(rule.name ,ruleRef)
                    |> ignore;
                    !ruleRef)

type AddDefaultAC() = 
    inherit Convertion()
        override this.Name = "AddDefaultAC"
        override this.ConvertList (ruleList,_) = addDefaultAC ruleList 
        override this.EliminatedProductionTypes = [""]

