//  Module EliminateLeftRecursion contains:
//  - function, which delete left recursion from grammar
//
//  Copyright 2009,2010 Ilia Chemodanov
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

module Yard.Core.Convertions.EliminateLeftRecursion

open Yard.Core
open Yard.Core.IL
open Production
open Yard.Core.Namer
open TransformAux

    
let eliminateLeftRecursion (ruleList : Rule.t<_,_> list) =
    let rules = ruleList |> Array.ofList
    let nonTermToIdx, idxToNonTerm =
        rules
        |> Array.map (fun x -> x.name)
        |> Set.ofArray |> Array.ofSeq
        |> (fun x ->
                let dict = x |> Array.mapi (fun i n -> n, i) |> dict
                (fun nTerm -> dict.Item nTerm)
                , (fun i -> x.[i])
            )
    ruleList

type EliminateLeftRecursion() = 
    inherit Convertion()
        override this.Name = "EliminateLeftRecursion"
        override this.ConvertList (ruleList,_) =
            let prevConvertions = ["ExpandEbnf"; "ExpandMeta"; "ExpandAlter"]
            eliminateLeftRecursion ruleList
        override this.EliminatedProductionTypes = [""]

