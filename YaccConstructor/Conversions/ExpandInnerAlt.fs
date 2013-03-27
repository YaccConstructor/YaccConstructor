//  Module ExpandBrackets contains:
//  - function, which moves inner alternatives to
// separate rules. Basically it is needed for RNGLR.
//
//  Copyright 2009, 2010, 2011 Konstantin Ulitin,
//            2011, 2012 Dmitry Avdyukhin
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

module Yard.Core.Conversions.ExpandInnerAlt

open Yard.Core
open Yard.Core.IL
open TransformAux
open Yard.Core.IL.Production

let dummyPos s = new Source.t(s)

let private expandInnerAlts (ruleList: Rule.t<_,_> list) = 
    let toExpand = new System.Collections.Generic.Queue<Rule.t<_,_>>(List.toArray ruleList)
    let expanded = ref []
    while toExpand.Count > 0 do
        let toExpandRule = toExpand.Dequeue()
        let rec expandBody attrs = function
            | PSeq(elements, actionCode, l) -> 
                elements |> List.fold (fun (res, attrs) elem ->
                    match elem.rule with 
                    | PSeq(subelements, None, l) when subelements.Length = 1 -> 
                        { elem with rule = (List.head subelements).rule }
                    | PSeq(subelements, subActionCode, l) when subelements.Length > 1 || subActionCode <> None ->
                        let newName = Namer.newName Namer.Names.brackets
                        toExpand.Enqueue({name = dummyPos newName; args=attrs; body=elem.rule;
                                            isStart=false; isPublic=false; metaArgs=[]})
                        { elem with rule = PRef(dummyPos newName, list2opt <| createParams attrs) }
                    | PAlt(_,_) -> 
                        let newName = Namer.newName Namer.Names.brackets
                        toExpand.Enqueue({name = dummyPos newName; args=attrs; body=elem.rule;
                                            isStart=false; isPublic=false; metaArgs=[]})
                        { elem with rule = PRef(dummyPos newName, list2opt <| createParams attrs) }
                    | _ -> elem
                    |> fun newElem -> newElem::res, if elem.binding.IsSome then attrs@[elem.binding.Value] else attrs
                ) ([], attrs)
                |> fst |> List.rev
                |> fun elems -> PSeq (elems, actionCode, l)
            | PAlt(left, right) -> PAlt(expandBody attrs left, expandBody attrs right)
            | x -> x
        
        let expandedRule = expandBody toExpandRule.args toExpandRule.body
        expanded := { toExpandRule with body=expandedRule } :: !expanded
        ()
    List.rev !expanded

type ExpandInnerAlt() = 
    inherit Conversion()
        override this.Name = "ExpandInnerAlt"
        override this.ConvertGrammar (grammar,_) = mapGrammar expandInnerAlts grammar
