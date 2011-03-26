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
open Yard.Core.IL.Production

let nameIndex = ref 0
let createName() = nameIndex := !nameIndex + 1 ;sprintf "yard_exp_brackets_%d" !nameIndex
let getLastName() = sprintf "yard_exp_brackets_%d" !nameIndex
    
let expandBrackets (ruleList: Rule.t<'patt, 'expr> list) = 
    let toExpand = new System.Collections.Generic.Queue<Rule.t<'patt, 'expr>>(List.toArray ruleList)
    let expanded = ref []
    while toExpand.Count > 0 do
        let toExpandRule = toExpand.Dequeue()
        let rec expandBody = function
            | PSeq(elements, actionCode) -> PSeq((List.map (fun elem ->
                match elem.rule with 
                | PSeq(subelements, subActionCode) when List.length subelements > 1 || subActionCode <> None -> 
                    toExpand.Enqueue({new Rule.t<'patt,'expr> with name=createName() and args=[] and body=elem.rule and _public=false and metaArgs=[]})
                    { elem with rule = PRef((getLastName(),(0,0)), None) }
                | PAlt(_,_) -> 
                    toExpand.Enqueue({new Rule.t<'patt,'expr> with name=createName() and args=[] and body=elem.rule and _public=false and metaArgs=[]})
                    { elem with rule = PRef((getLastName(),(0,0)), None) }
                | x -> elem
                )
                elements), actionCode)
            | PAlt(left, right) -> PAlt(expandBody left, expandBody right)
            | x -> x
        
        let expandedRule = expandBody toExpandRule.body
        expanded := { toExpandRule with body=expandedRule } :: !expanded
        ()
    List.rev !expanded

type ExpandBrackets() = 
    interface IConvertion with
        member this.Name = "ExpandBrackets"
        member this.ConvertList ruleList = expandBrackets ruleList
        member this.EliminatedProductionTypes = [""]
    end