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

module YC.Core.Conversions.ExpandInline

open YC.Core
open IL


let private replaceInline (rules : Rule<_,_> list) =
    let inlines = 
        rules
        |> List.choose
            (fun cur ->
                if cur.isInline   
                then Some (cur.name.text, cur.body)
                else None)
        |> dict
    
    let rec modifyBody = function
        | PSeq (elems, ac, l) ->
            let newElems =
                elems |> List.map (fun x -> {x with rule = modifyBody x.rule})
            PSeq(newElems, ac, l)
        | PAlt (l,r) -> PAlt(modifyBody l, modifyBody r)
        | PConj (l,r) -> PConj(modifyBody l, modifyBody r)
        | PNeg x -> PNeg(modifyBody x)
        | PRef (name,_) as prev ->
            if inlines.ContainsKey name.text then modifyBody inlines.[name.text]
            else prev
        | PMetaRef (name,x,args) ->
            if inlines.ContainsKey name.text then modifyBody inlines.[name.text]
            else PMetaRef(name,x, List.map modifyBody args)
        | PMany x -> PMany <| modifyBody x
        | PSome x -> PSome <| modifyBody x
        | POpt x -> POpt <| modifyBody x
        | PRepet (x,i,j) -> PRepet (modifyBody x, i, j)
        | PLiteral _ 
        | PPerm _ 
        | PToken _ as x -> x
        | PShuff _ -> failwith "Unsupported"
        //| x -> x
        
    rules
    |> List.choose
        (fun rule -> 
            if inlines.ContainsKey rule.name.text && not rule.isStart
            then None
            else Some <| {rule with body = modifyBody rule.body})
           
type ReplaceInline() = 
    inherit Conversion()
        override this.Name = "ReplaceInline"
        override this.ConvertGrammar (grammar,_) = mapGrammar replaceInline grammar
