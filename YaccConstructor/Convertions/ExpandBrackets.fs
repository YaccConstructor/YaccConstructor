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

let expandBrackets (ruleList: Rule.t<'patt, 'expr> list) = 
    let toExpand = new System.Collections.Generic.Queue<Rule.t<'patt, 'expr>>(List.toArray ruleList)
    while toExpand.Count > 0 do
        let expandedRule = 
            match toExpand.Dequeue().body with
            | PSeq(elements, actionCode) -> PSeq((List.map (fun elem ->
                match elem.rule with 
                | PSeq(subelements, subActionCode) when List.length subelements > 1 || subActionCode <> None -> 
                    toExpand.Enqueue({new Rule.t<'patt,'expr> with name="seq" and args=[] and body=elem.rule and _public=false and metaArgs=[]})
                    { elem with rule = PRef(("seq",(0,0)), None) }
                | PAlt(_,_) -> 
                    toExpand.Enqueue({new Rule.t<'patt,'expr> with name="alt" and args=[] and body=elem.rule and _public=false and metaArgs=[]})
                    { elem with rule = PRef(("alt",(0,0)), None) }
                | x -> elem
                )
                elements), actionCode)
            | x -> x
        printf "sdf"
    (*
    List.collect (fun (rule:Rule.t<'patt, 'expr>) ->
        match rule.body with
        | PSeq(elements, actionCode) -> List.map (function 
            | PSeq(subelements, subActionCode) -> elements
//             | PAlt(x,y) -> [{rule with body=PRef(("aa",(0,0)), None)}; {new Rule.t<'patt, 'expr> with name="aa" and args=[] and body=PAlt(x,y) and _public=false and metaArgs=[]}]
             | _ -> []
        )
        ruleList*)
    []

type ExpandBrackets() = 
    interface IConvertion with
        member this.Name = "ExpandBrackets"
        member this.ConvertList ruleList = expandBrackets ruleList
        member this.EliminatedProductionTypes = [""]
    end