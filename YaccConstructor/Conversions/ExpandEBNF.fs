//  Module ExpandEbnf contains:
//  - functions for rules Conversion from EBNF to BNF 
//
//  Copyright 2011 by Konstantin Ulitin
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
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
    let genAction ac oldBody = genNewSource ac oldBody |> Some
    let genBinding = genAction
    // if production is not binded then don't add semantic action in generated rules
    let rec replaceEbnf production attrs metaArgs = 
        let insideMetaArgs =
            metaArgs
            |> List.map (fun x -> PRef (x, None))
        let genRule generatedName src =
            let inner attrsHandler =
                reduceMeta <| PMetaRef(genNewSource generatedName src, list2opt <| createParams (attrsHandler attrs), insideMetaArgs)
            inner factList, inner formList
        match production with
        | PSeq(elem_list, ac, l) ->
            PSeq(elem_list
                 |> List.fold
                    (fun (res,curAttrs) elem ->
                        {elem with rule = replaceEbnf elem.rule curAttrs metaArgs}::res
                        , if elem.binding.IsNone then curAttrs
                           else curAttrs@[elem.binding.Value, elem.binding.Value(*createNewName <| createSource "arg"*)]
                    )
                    ([],attrs)
                 |> fst
                 |> List.rev
                 , ac, l)
        | PAlt(left, right) -> PAlt(replaceEbnf left attrs metaArgs, replaceEbnf right attrs metaArgs)
        | PSome p ->
            let generatedName = genSomeName()
            let expandedBody = replaceEbnf p attrs metaArgs
            let newRule, insideNewRule = genRule generatedName p
            let newBody =
                PAlt(
                    PSeq([{default_elem with rule = expandedBody; binding=genBinding "yard_elem" p}], genAction "[yard_elem]" p, None) ,
                    PSeq([
                            {omit=false;
                                rule = expandedBody;
                                binding=genBinding "yard_head" p;
                                checker=None};
                            {omit=false;
                                rule = insideNewRule;
                                binding=genBinding "yard_tail" p;
                                checker=None}
                            ]
                            , genAction "yard_head::yard_tail" p, None)
                ) 
            addedBnfRules := (
                {
                 name = genNewSource generatedName p
                 args = formList attrs 
                 body = newBody
                 _public=false
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
                 name= genNewSource generatedName p
                 args = formList attrs
                 body= newBody
                 _public=false
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
                 name= genNewSource generatedName p
                 args = formList attrs
                 body= newBody
                 _public=false
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
        override this.ConvertList (ruleList,_) = ruleList |> List.map (convertToBnf) |> List.concat
        override this.EliminatedProductionTypes = ["POpt"; "PSome"; "PMany"]