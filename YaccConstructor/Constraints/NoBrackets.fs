//  Describe constraint - grammar cannot contain inner alternatives.
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

module Yard.Core.ConstraintsImpl.NoBrackets

open Yard.Core
open IL
open Production
open Yard.Core.ConstraintsImpl.Common

let private checker grammar =
    let isAlt = function PAlt _ -> true | _ -> false
    let rec inner = function
        | PSeq (elems,_,_) ->
            elems |> List.exists (fun e -> match e.rule with PSeq _ -> true | _ -> false)
        | x -> false
    existsRules (fun r -> inner r.body) grammar
    |> not
    
let noBrackets = new Constraint("NoBrackets", checker, Conversions.ExpandBrackets.ExpandBrackets())