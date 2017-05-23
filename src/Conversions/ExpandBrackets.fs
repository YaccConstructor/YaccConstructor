//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

module Yard.Core.Conversions.ExpandBrackets

open Yard.Core
open Yard.Core.IL
open Namer
open TransformAux
open Yard.Core.IL.Production


//let dummyPos s = new Source.t(s)

let private newName () = Namer.newName Namer.Names.brackets
    
let private expandBrackets (ruleList: Rule.t<_, _> list) = 
    let toExpand = new System.Collections.Generic.Queue<Rule.t<_, _>>(List.toArray ruleList)
    let expanded = ref []
    while toExpand.Count > 0 do
        let toExpandRule = toExpand.Dequeue()
        let rec expandBody attrs = function
            | PSeq(elements, actionCode, l) ->
                elements
                |> List.fold (fun (res, attrs) elem ->
                    let newElem =
                        match elem.rule with 
                        | PSeq(subelements, None, l) as s when List.length subelements = 1  ->
                            let body = 
                                match subelements.Head.rule with
                                | PSeq(x,y,_) -> PSeq(x,y,l)
                                | _ -> s 
                            { elem with rule = body }
                        | PSeq(subelements, subActionCode, l) when List.length subelements > 1 || subActionCode <> None ->
                            let newName = newName()
                            toExpand.Enqueue({name= genNewSourceWithRange newName elem.rule; args=attrs;
                                                body=elem.rule; isStart=false; isPublic=false; isInline = false; metaArgs=[]})
                            { elem with rule = PRef(genNewSourceWithRange newName elem.rule, list2opt <| createParams attrs) }
                        | _ -> elem
                    newElem::res, if elem.binding.IsSome then attrs@[elem.binding.Value] else attrs
                ) ([], attrs)
                |> fst |> List.rev
                |> fun elems -> PSeq (elems,actionCode, l)
            | PAlt(left, right) -> PAlt(expandBody attrs left, expandBody attrs right)
            | x -> x
        
        let expandedRule = expandBody toExpandRule.args toExpandRule.body
        expanded := { toExpandRule with body=expandedRule } :: !expanded
        ()
    List.rev !expanded







type ExpandBrackets() = 
    inherit Conversion()
        override this.Name = "ExpandBrackets"
        override this.ConvertGrammar (grammar,_) =
            mapGrammar expandBrackets grammar
