//  Module ExpandEbnf contains:
//  - functions for rules convertion from EBNF to BNF 
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

module Yard.Core.Convertions.ExpandEbnfStrict

open Yard.Core
open Yard.Core.IL
open Production

let s2source s = (s, (0,0))
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

let default_elem = {new elem<Source.t, Source.t> with omit=false and rule=PRef(s2source "empty", None) and binding=None and checker=None}

let convertToBnf (rule:(Rule.t<Source.t,Source.t>)) = 
    let addedBnfRules = ref []
    let sourceIf cond s = if cond then Some(s2source s) else None
    // if production is not binded then don't add semantic action in generated rules
    let rec replaceEbnf production binded = 
        match production with
        | PSeq(elem_list, ac) -> PSeq(elem_list |> List.map (fun elem -> {elem with rule=replaceEbnf elem.rule (elem.binding.IsSome)}), ac) 
        | PAlt(left, right) -> PAlt(replaceEbnf left binded, replaceEbnf right binded)
        | PSome(p) -> 
            let generatedName = genSomeName()
            let expandedBody = replaceEbnf p binded
            addedBnfRules := (
                {new Rule.t<Source.t,Source.t> 
                 with name=generatedName 
                 and args=[] 
                 and body=
                    PAlt(
                        PSeq([{default_elem with rule=expandedBody; binding=sourceIf binded "elem"}], sourceIf binded "[elem]") ,
                        PSeq([
                            {new elem<Source.t, Source.t> with omit=false and rule=expandedBody                       and binding=sourceIf binded "head" and checker=None};
                            {new elem<Source.t, Source.t> with omit=false and rule=PRef(s2source generatedName, None) and binding=sourceIf binded "tail" and checker=None}
                        ], sourceIf binded "head::tail")
                    ) 
                 and _public=false
                 and metaArgs=[]
                }) :: !addedBnfRules
            PRef(s2source generatedName, None)
        | PMany(p) -> 
            let generatedName = genManyName()
            let expandedBody = replaceEbnf p binded
            addedBnfRules := (
                {new Rule.t<Source.t,Source.t> 
                 with name=generatedName 
                 and args=[] 
                 and body=
                    PAlt(
                        PSeq([{default_elem with rule=PRef(s2source "empty", None)}], sourceIf binded "[]") ,
                        PSeq([
                            {new elem<Source.t, Source.t> with omit=false and rule=expandedBody                       and binding=sourceIf binded "head" and checker=None};
                            {new elem<Source.t, Source.t> with omit=false and rule=PRef(s2source generatedName, None) and binding=sourceIf binded "tail" and checker=None}
                        ], sourceIf binded "head::tail")
                    ) 
                 and _public=false
                 and metaArgs=[]
                }) :: !addedBnfRules
            PRef(s2source generatedName, None)
        | POpt(p) -> 
            let generatedName = genOptName()
            let expandedBody = replaceEbnf p binded
            addedBnfRules := (
                {new Rule.t<Source.t,Source.t> 
                 with name=generatedName 
                 and args=[] 
                 and body=
                    PAlt(
                        PSeq([{default_elem with rule=PRef(s2source "empty", None)}], sourceIf binded "None"),
                        PSeq([{default_elem with rule=expandedBody; binding=sourceIf binded "elem"}], sourceIf binded "Some(elem)")
                    ) 
                 and _public=false
                 and metaArgs=[]
                }) :: !addedBnfRules
            PRef(s2source generatedName, None)
        | x -> x
    {rule with body=replaceEbnf rule.body false}::(List.rev !addedBnfRules)

type ExpandEbnfStrict() = 
    inherit Convertion()
        override this.Name = "ExpandEbnfStrict"
        override this.ConvertList ruleList = ruleList |> List.map (convertToBnf) |> List.concat
        override this.EliminatedProductionTypes = ["POpt"; "PSome"; "PMany"]
