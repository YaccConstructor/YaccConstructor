//  Module RemoveAST contains:
//  - function, which remove all action code.
//
//  Copyright 2012 Semen Grigorev
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

module Yard.Core.Convertions.RemoveAST

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

let private removeAC (ruleList: Rule.t<Source.t, Source.t> list) =
    let rec inner (production: t<Source.t, Source.t>) =
        match production with
        | PSeq (x,ac,l) -> PSeq(x |> List.map (fun b -> {b with rule = b.rule |> inner}),None,l)
        | PAlt (l,r) -> PAlt (inner l, inner r)
        | PSome p -> inner p |> PSome
        | PMany p -> inner p |> PMany
        | PLiteral _ 
        | PToken _ 
        | PRef _
        | PRepet _
        | PPerm _
        | PMetaRef _ as p -> p
        | POpt p -> inner p |> POpt        

    ruleList |> List.map (fun rule -> {rule with body=(inner rule.body) } )

type RemoveAC() =
    inherit Convertion()
    override this.Name = "RemoveAC"
    override this.ConvertList(ruleList,_) = removeAC ruleList
    override this.EliminatedProductionTypes = [""]
