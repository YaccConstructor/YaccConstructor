//  Describe constraint - grammar must be in CNF.
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

module Yard.Core.ConstraintsImpl.InCNF

open Yard.Core
open IL
open Production
open Yard.Core.ConstraintsImpl.Common

let private checker (grammar : Grammar.t<_,_>) =
    if grammar.Length > 1 then false
    else
        let notStart = 
            match grammar.Head.rules |> List.tryPick (fun r -> if r.isStart then Some r.name.text else None) with
            | None -> fun _ -> true
            | Some start -> fun (x : Source.t) -> x.text <> start
        existsRules (fun r ->
            match r.body with
            | PSeq (elems, _, _) ->
                match elems |> List.map (fun e -> e.rule) with
                | [] -> not r.isStart
                | [PToken _] -> false
                | [PRef (a,_); PRef (b,_)] when notStart a && notStart b -> false
                | _ -> true
            | _ -> true
        ) grammar |> not
    
let inCNF = new Constraint("InCNF", checker, Conversions.ToCNF.ToCNF())