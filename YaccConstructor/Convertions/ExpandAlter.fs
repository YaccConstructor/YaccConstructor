//  Copyright 2009 by Ilia Chemodanov
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
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

module Yard.Core.Convertions.ExpandAlter

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

open System

let extract_one_rule (rule:Rule.t<'a,'b>) = 
    let rec expand = function
    |PAlt     (a,b) -> expand a  @ expand b
    |PSeq     (a,b) -> let wrap = List.map (fun x -> (x.rule, fun r -> {x with rule = r})) a
                                  |> List.unzip
                       in
                       let rec gen = function
                           | hd::tl -> [for x in hd -> x :: ( gen tl |> List.concat)]
                           | []     -> []
                       in 
                       fst wrap |> List.map expand |> gen 
                       |> List.map (fun x -> PSeq ((List.map2 ( |> ) x (snd wrap)),b))
    |PRef   _ 
    |PLiteral _
    |PToken   _ as t   -> [t]
    | _             -> (System.Console.WriteLine("incorrect tree for alternative expanding!")
                        ; failwith "incorrect tree for alternative expanding!")
    in 
    expand rule.body |> List.map (fun x -> {rule with body = x})

type ExpandAlter() = 
    inherit Convertion()
        override this.Name = "ExpandAlter"
        override this.ConvertList ruleList = List.collect extract_one_rule ruleList
        override this.EliminatedProductionTypes = ["PAlt"]