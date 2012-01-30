//  Module ExpandBrackets contains:
//  - function, which moves grammar constructions in brackets to
// separate rules. Basically it is needed for FsYaccPrinter.
// ExpandMeta and ExpandEBNF should be applied firstly.
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

module Yard.Core.Convertions.ExpandBrackets

open Yard.Core
open Yard.Core.IL
open TransformAux
open Yard.Core.IL.Production

let private newName () = (Namer.Names.brackets,(0,0)) |> Namer.createNewName |> fst
    
let private expandBrackets (ruleList: Rule.t<'patt, 'expr> list) = 
    let toExpand = new System.Collections.Generic.Queue<Rule.t<'patt, 'expr>>(List.toArray ruleList)
    let expanded = ref []
    while toExpand.Count > 0 do
        let toExpandRule = toExpand.Dequeue()
        let rec expandBody attrs = function
            | PSeq(elements, actionCode) -> 
                (elements
                 |>List.fold
                    (fun (res, attrs) elem ->
                        let newElem =
                            match elem.rule with 
                            | PSeq(subelements, None) when List.length subelements = 1 -> 
                                { elem with rule = (List.head subelements).rule }
                            | PSeq(subelements, subActionCode) when List.length subelements > 1 || subActionCode <> None ->
                                let newName = newName()
                                toExpand.Enqueue({name = newName; args=attrs; body=elem.rule; _public=false; metaArgs=[]})
                                { elem with rule = PRef((newName,(0,0)), list2opt <| createParams attrs) }
                            | PAlt(_,_) -> 
                                let newName = newName()
                                toExpand.Enqueue({name=newName; args=attrs; body=elem.rule; _public=false; metaArgs=[]})
                                { elem with rule = PRef((newName,(0,0)), list2opt <| createParams attrs) }
                            | _ -> elem
                        newElem::res, if elem.binding.IsSome then elem.binding.Value::attrs else attrs
                    )
                    ([], attrs)
                 |> fst |> List.rev
                 ,actionCode)
                |> PSeq
            | PAlt(left, right) -> PAlt(expandBody attrs left, expandBody attrs right)
            | x -> x
        
        let expandedRule = expandBody toExpandRule.args toExpandRule.body
        expanded := { toExpandRule with body=expandedRule } :: !expanded
        ()
    List.rev !expanded

type ExpandBrackets() = 
    inherit Convertion()
        override this.Name = "ExpandBrackets"
        override this.ConvertList (ruleList,_) = expandBrackets ruleList
        override this.EliminatedProductionTypes = [""]