//  Module MergeAlter contains:
//  - function, which merges though alternative sign all rules with same name. It can be useful
//  for rule's merging when one file with grammar includes other.
//
//  Copyright 2009, 2010, 2011 Konstantin Ulitin
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

module Yard.Core.Convertions.MergeAlter

open Yard.Core
open Yard.Core.IL

open System.Collections.Generic

let mergeAlter (ruleList: Rule.t<Source.t, Source.t> list) = 
    let buildAlt (ruleList:Rule.t<Source.t, Source.t> list) = 
        //TODO add check that all merged rules contains same attributes and metaattributes
        let rec buildProduction productionList =
            match productionList with
            | [production] -> production
            | production::t -> Production.PAlt(production,  buildProduction t)
            | [] -> failwith "buildProduction: expected one or more elements"
        match ruleList with
        | rule::t -> { rule with body = ruleList |> List.map (fun ruleInner -> ruleInner.body) |> buildProduction }
        | [] -> failwith "buildAlt: expected one or more elements"
    let findedRules = new HashSet<string>()
    ruleList |>
        List.choose 
            (fun rule ->
                if findedRules.Contains(rule.name) then
                    None
                else
                    findedRules.Add(rule.name) |> ignore
                    Some <| buildAlt (ruleList |> List.filter (fun ruleInner -> rule.name = ruleInner.name))
            )
    

type MergeAlter() = 
    inherit Convertion()
        override this.Name = "MergeAlter"
        override this.ConvertList (ruleList,_) = mergeAlter ruleList
        override this.EliminatedProductionTypes = [""]