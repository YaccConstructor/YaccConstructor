//  Contains common functions for some constraints (like production traversal)
//
//  Copyright 2013 Avdyukhin Dmitry<dimonbv@gmail.com>
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

module Yard.Core.ConstraintsImpl.Common

open Yard.Core.IL
open Production

let existsRules pred (grammar : Grammar.t<_,_>) =
    grammar |> List.exists (fun m ->
        m.rules |> List.exists pred
    )

let existsSubProd pred =
    let rec exists node = 
        if pred node then true
        else
            match node with
            | PAlt (l, r) -> exists l || exists r
            | PSeq (elems,_,_) ->
                elems |> List.exists (fun e -> exists e.rule)
            | PSome r | POpt r | PMany r | PRepet (r,_,_) -> exists r
            | PMetaRef (_,_,metas) ->
                metas |> List.exists exists
            | PPerm elems -> elems |> List.exists exists
            | PLiteral _ | PToken _ | PRef _ -> false
    exists

let existsProd pred =
    let exists = existsSubProd pred
    existsRules (fun r -> exists r.body)
