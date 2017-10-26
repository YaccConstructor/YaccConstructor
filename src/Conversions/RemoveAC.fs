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

module Yard.Core.Conversions.RemoveAST

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production


let private removeAC (ruleList: Rule<Source.t, Source.t> list) =
    let rec inner (production: t<Source.t, Source.t>) =
        match production with
        | PSeq (x,ac,l) -> PSeq(x |> List.map (fun b -> {b with rule = b.rule |> inner}),None,l)
        | PAlt (l,r) -> PAlt (inner l, inner r)
        | PConj (l,r) -> PConj (inner l, inner r)
        | PSome p -> inner p |> PSome
        | PMany p -> inner p |> PMany
        | PLiteral _ 
        | PToken _ 
        | PRef _
        | PRepet _
        | PPerm _
        | PMetaRef _ as p -> p
        | POpt p -> inner p |> POpt

    ruleList |> List.map (fun rule -> {rule with body=(inner rule.body) } )

type RemoveAC() =
    inherit Conversion()
    override this.Name = "RemoveAC"
    override this.ConvertGrammar(grammar,_) = mapGrammar removeAC grammar
