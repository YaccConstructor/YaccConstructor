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

module YC.Core.ConstraintsImpl.Common

open YC.Core.IL

let existsRules pred (grammar : Grammar<_,_>) =
    grammar |> List.exists (fun m ->
        m.rules |> List.exists pred
    )

let existsSubProd pred =
    let rec exists node = 
        if pred node then true
        else
            match node with
            | PAlt (l, r) -> exists l || exists r
            | PConj (l, r) -> exists l || exists r
            | PSeq (elems,_,_) ->
                elems |> List.exists (fun e -> exists e.rule)
            | PSome r | POpt r | PMany r | PRepet (r,_,_) -> exists r
            | PMetaRef (_,_,metas) ->
                metas |> List.exists exists
            | PPerm elems -> elems |> List.exists exists
            | PLiteral _ | PToken _ | PRef _ -> false
            | PShuff _ -> failwith "Unsupported"
            | PNeg _ -> failwith "Unsupported"
    exists

let existsProd pred =
    let exists = existsSubProd pred
    existsRules (fun r -> exists r.body)
