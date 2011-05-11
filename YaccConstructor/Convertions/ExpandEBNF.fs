//  Module ExpandEbnf contains:
//  - functions for rules convertion from EBNF to BNF 
//
//  Copyright 2009-2011 by Konstantin Ulitin
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

module Yard.Core.Convertions.ExpandEbnf

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
//    let addedBnfRules = new System.Collections.Generic.Queue<Rule.t<'patt, 'expr>>()
    let addedBnfRules = ref []
    let rec replaceEbnf production = 
        match production with
        | PSeq(elem_list, ac) -> PSeq(elem_list |> List.map (fun elem -> {elem with rule=replaceEbnf elem.rule}), ac) 
        | PAlt(left, right) -> PAlt(replaceEbnf left, replaceEbnf right)
        | PSome(p) -> 
            let generatedName = genSomeName()
            let expandedBody = replaceEbnf p
            addedBnfRules := (
                {new Rule.t<Source.t,Source.t> 
                 with name=generatedName 
                 and args=[] 
                 and body=
                    PAlt(expandedBody, PSeq([
                            {new elem<Source.t, Source.t> with omit=false and rule=PRef(s2source generatedName, None) and binding=None and checker=None};
                            {new elem<Source.t, Source.t> with omit=false and rule=expandedBody                       and binding=None and checker=None}
                    ], None)) 
                 and _public=false
                 and metaArgs=[]
                }) :: !addedBnfRules
            PRef(s2source generatedName, None)
        | PMany(p) -> 
            let generatedName = genManyName()
            let expandedBody = replaceEbnf p
            addedBnfRules := (
                {new Rule.t<Source.t,Source.t> 
                 with name=generatedName 
                 and args=[] 
                 and body=
                    PAlt(
                        PSeq([{default_elem with rule=PRef(s2source "empty", None)}], Some(s2source "[]")) ,
                        PSeq([
                            {new elem<Source.t, Source.t> with omit=false and rule=PRef(s2source generatedName, None) and binding=None and checker=None};
                            {new elem<Source.t, Source.t> with omit=false and rule=expandedBody                       and binding=None and checker=None}
                        ], Some(s2source )
                    ) 
                 and _public=false
                 and metaArgs=[]
                }) :: !addedBnfRules
            PRef(s2source generatedName, None)
        | POpt(p) -> 
            let generatedName = genOptName()
            let expandedBody = replaceEbnf p
            addedBnfRules := (
                {new Rule.t<Source.t,Source.t> 
                 with name=generatedName 
                 and args=[] 
                 and body=
                    PAlt(
                        PSeqPRef(s2source "empty", None), PSeq([
                            {new elem<Source.t, Source.t> with omit=false and rule=expandedBody and binding=None and checker=None}
                    ], None)) 
                 and _public=false
                 and metaArgs=[]
                }) :: !addedBnfRules
            PRef(s2source generatedName, None)
        | x -> x
    {rule with body=replaceEbnf rule.body}::!addedBnfRules

type ExpandEbnf() = 
    inherit Convertion()
        override this.Name = "ExpandEbnf"
        override this.ConvertList ruleList = ruleList |> List.map (convertToBnf) |> List.concat
        override this.EliminatedProductionTypes = ["POpt"; "PSome"; "PMany"]
