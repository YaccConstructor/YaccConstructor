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

module Yard.Core.Conversions.ExpandEbnfStrict

open Yard.Core
open Yard.Core.IL
open Production
open Namer
open TransformAux
open Yard.Core.IL.Rule


//let dummyPos s = new Source.t(s)

//let s2source = TransformAux.createSource
let generatedSomesCount = ref 0
let genSomeName () =
    generatedSomesCount := !generatedSomesCount + 1
    sprintf "yard_some_%d" !generatedSomesCount

let generatedManysCount = ref 0
let genManyName () =
    generatedManysCount := !generatedManysCount + 1
    sprintf "yard_many_%d" !generatedManysCount

let generatedOptsCount = ref 0
let genOptName () =
    generatedOptsCount := !generatedOptsCount + 1
    sprintf "yard_opt_%d" !generatedOptsCount

let default_elem = {omit=false; rule=PRef(new Source.t "dummy", None); binding=None; checker=None}

let convertToBnf (rule:(Rule.t<Source.t,Source.t>)) = 
    let factList list = list |> List.map fst
    let formList list = list |> List.map snd
    let reduceMeta = function 
        | PMetaRef (n,a,[]) -> PRef(n,a)
        | x -> x
    let addedBnfRules = ref []
    //let sourceIf cond s = if cond then Some(s2source s) else None
    let genAction ac oldBody = genNewSourceWithRange ac oldBody |> Some
    let genBinding = genAction
    // if production is not binded then don't add semantic action in generated rules
    let rec replaceEbnf production attrs metaArgs = 
        let insideMetaArgs =
            metaArgs
            |> List.map (fun x -> PRef (x, None))
        let genRule generatedName src =
            let inner attrsHandler =
                reduceMeta <| PMetaRef(genNewSourceWithRange generatedName src, list2opt <| createParams (attrsHandler attrs), insideMetaArgs)
            inner factList, inner formList
        match production with
        | PSeq(elem_list, ac, l) ->
            elem_list
            |> List.fold (fun (res,curAttrs) elem ->
                {elem with rule = replaceEbnf elem.rule curAttrs metaArgs}::res
                , if elem.binding.IsNone then curAttrs
                    else curAttrs@[elem.binding.Value, elem.binding.Value(*createNewName <| createSource "arg"*)]
            ) ([],attrs)
            |> fst
            |> List.rev
            |> fun elems -> PSeq(elems , ac, l)
        | PAlt(left, right) -> PAlt(replaceEbnf left attrs metaArgs, replaceEbnf right attrs metaArgs)
        | PConj(left, right) -> PConj(replaceEbnf left attrs metaArgs, replaceEbnf right attrs metaArgs)
        | PSome p ->
            let generatedName = genSomeName()
            let expandedBody = replaceEbnf p attrs metaArgs
            let newRule, insideNewRule = genRule generatedName p
            let newBody =
                PAlt(
                    PSeq([{default_elem with rule = expandedBody; binding=genBinding "yard_elem" p}], genAction "[yard_elem]" p, None) ,
                    PSeq([{
                            omit=false;
                            rule = expandedBody;
                            binding=genBinding "yard_head" p;
                            checker=None
                        }; {
                            omit=false;
                            rule = insideNewRule;
                            binding=genBinding "yard_tail" p;
                            checker=None
                        }]
                        , genAction "yard_head::yard_tail" p, None)
                ) 
            addedBnfRules := (
                {
                    name = genNewSourceWithRange generatedName p
                    args = formList attrs 
                    body = newBody
                    isStart=false
                    isPublic=false
                    isInline = false
                    metaArgs = metaArgs
                }) :: !addedBnfRules
            newRule
        | PMany p -> 
            let generatedName = genManyName()
            let expandedBody = replaceEbnf p attrs metaArgs
            let newRule, insideNewRule = genRule generatedName p
            let newBody = 
                PAlt(
                    PSeq([], genAction "[]" p, None) ,
                    PSeq([
                            {omit=false;
                                rule=expandedBody;
                                binding=genBinding "yard_head" p;
                                checker=None};
                            {omit=false;
                                rule=insideNewRule;
                                binding=genBinding "yard_tail" p;
                                checker=None}
                            ]
                            , genAction "yard_head::yard_tail" p, None)
                ) 
            addedBnfRules := (
                {
                 name= genNewSourceWithRange generatedName p
                 args = formList attrs
                 body= newBody
                 isStart=false
                 isPublic=false
                 isInline = false
                 metaArgs = metaArgs
                }) :: !addedBnfRules
            newRule
        | POpt p -> 
            let generatedName = genOptName()
            let expandedBody = replaceEbnf p attrs metaArgs
            let newRule, insideNewRule = genRule generatedName p
            let newBody =
                PAlt(
                    PSeq([], genAction "None" p, None),
                    PSeq([{default_elem with rule=expandedBody; binding=genBinding "yard_elem" p}], genAction "Some(yard_elem)" p, None)
                ) 
            addedBnfRules := (
                {
                 name= genNewSourceWithRange generatedName p
                 args = formList attrs
                 body= newBody
                 isStart=false
                 isPublic=false
                 isInline = false
                 metaArgs = metaArgs
                }) :: !addedBnfRules
            newRule
         | PMetaRef (src, args, metas) as x ->
            metas |> List.map (fun prod -> replaceEbnf prod attrs metaArgs)
            |> fun m -> PMetaRef (src, args, m)
         | PLiteral _ | PPerm _ | PRef _ | PRepet _ | PToken _ as x -> x
        //| x -> x
    {rule with body=replaceEbnf rule.body (List.zip rule.args rule.args) rule.metaArgs}::(List.rev !addedBnfRules)

type ExpandEbnf() = 
    inherit Conversion()
        override this.Name = "ExpandEbnf"
        override this.ConvertGrammar (grammar,_) = mapGrammar (fun rules -> rules |> List.map (convertToBnf) |> List.concat) grammar
