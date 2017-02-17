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

module Yard.Core.Conversions.AddDefaultAC

open Yard.Core
open Namer
open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Core.Conversions.TransformAux
open Mono.Addins

//let dummyPos s = new Source.t(s)

/// Adds action code to production considering it is used somewhere
let rec addAcToProduction neededRules ruleBody = 
    match ruleBody with
    | PSeq(elements, Some ac, l) -> 
        PSeq(
            elements 
            |> List.map 
                (fun elem ->
                    { elem with rule=addAcToProduction neededRules elem.rule }
                    (*match elem.binding with
                    | Some binding -> { elem with rule=addAcToProduction neededRules elem.rule }
                    | None -> elem*)
                )
            , Some ac, l
        )
    | PSeq(elements, None, l) -> 
        let getBinding i elem =
            if elem.omit 
            then None
            else Some <| genNewSourceWithRange (sprintf "S%d" (i+1)) elem.rule
        PSeq(
            elements |> List.mapi (fun i elem ->
                {elem with binding=getBinding i elem; rule=addAcToProduction neededRules elem.rule} )
            , Some(elements |> List.mapi getBinding |> List.choose id
                |> List.map (fun x -> x.text) |> String.concat ", "|> (fun n -> genNewSourceWithRange n ruleBody))       
            , l)
    | PAlt(left, right) -> PAlt(addAcToProduction neededRules left, addAcToProduction neededRules right)
    | PConj(left, right) -> PConj(addAcToProduction neededRules left, addAcToProduction neededRules right)
    | PRef(ref, _) as x -> neededRules := ref.text::!neededRules; x
    | PLiteral _ as x -> x
    | PToken _ as x -> x
    | PSome p -> PSome(addAcToProduction neededRules p)
    | PMany p -> PMany(addAcToProduction neededRules p)
    | POpt p -> POpt(addAcToProduction neededRules p)
    //| PMetaRef(_,_,_) -> failwith "ERROR: PMetaRef unexpected in AddDefaultAC"
    | x -> failwithf "ERROR: %A unexpected in AddDefaultAC" x

let addDefaultAC (ruleList: Rule.t<Source.t, Source.t> list)  = 
    let updatedRules = new HashSet<string>()
    let rulesQueueBfs = new System.Collections.Generic.Queue<string>()
    let rulesMap = new Dictionary<string, Rule.t<Source.t, Source.t>>()
    for rule in ruleList do
            rulesMap.Add(rule.name.text, rule) 
            //if rule._public then (rulesQueueBfs.Enqueue(rule.name) |> ignore)
            rulesQueueBfs.Enqueue rule.name.text
    while rulesQueueBfs.Count > 0 do
        let bfsFor = rulesQueueBfs.Dequeue()
        if not <| updatedRules.Contains bfsFor then    
            //printfn "u: %s" bfsFor
            updatedRules.Add bfsFor |> ignore        
            let emptyRule = {Rule.t.name = new Source.t(""); Rule.t.args = []; Rule.t.body = PSeq([], None, None)
                            ;Rule.t.isPublic = false; Rule.t.isInline = false; Rule.t.metaArgs = []; Rule.isStart = false
                            }
            let ruleFor = ref emptyRule
            if rulesMap.TryGetValue(bfsFor, ruleFor) then
                // Some generators need to have a sequence on the top of body tree
                (*
                let rec bodyToSeq = function
                    | PSeq (_,_) as p -> p
                    | PAlt (l,r) -> PAlt(bodyToSeq l, bodyToSeq r)
                    | PConj (l,r) -> PConj(bodyToSeq l, bodyToSeq r)
                    | x -> PSeq([{rule = x; binding = createSource "yard_bind" |> Some;
                                    omit = false; checker = None}]
                                , Some(createSource "yard_bind"))
                                *)
                let neededRules = ref []
                let updatedBody =
                    addAcToProduction neededRules (ruleFor.Value.body)
                    //|> bodyToSeq
                !neededRules |> List.iter (fun r -> if not <| updatedRules.Contains r then rulesQueueBfs.Enqueue r)
                rulesMap.[bfsFor] <- { !ruleFor with body=updatedBody}
    ruleList
    |> List.map (fun rule ->
                    let ruleRef = ref rule
                    rulesMap.TryGetValue(rule.name.text, ruleRef)
                    |> ignore
                    !ruleRef)


[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type AddDefaultAC() = 
    inherit Conversion()
        override this.Name = "AddDefaultAC"
        override this.ConvertGrammar (grammar,_) = mapGrammar addDefaultAC grammar

