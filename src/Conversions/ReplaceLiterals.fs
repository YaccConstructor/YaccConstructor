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

module Yard.Core.Conversions.ReplaceLiterals

open Yard.Core
open Yard.Core.IL

open System.Collections.Generic

let tokenName literal token_format =
    let upper = 
        String.collect 
            (function
            | '>' -> "GREATER"
            | '=' -> "EQUAL"
            | '<' -> "LESS"
            | ';' -> "SEMICOLON"
            | ':' -> "COLON"
            | '_' | ' ' -> "_"
            | c when System.Char.IsLetterOrDigit c -> string (System.Char.ToUpper c) 
            | _ -> ""
            )
            literal
    if upper.Length=0
    then "EMPTY" 
    else 
        let format = Printf.StringFormat<string->string>(token_format)
        sprintf format upper

let rec eachProduction f productionList =
    List.iter 
        (function    
        | PSeq(elements, actionCode, l) -> 
            PSeq(elements, actionCode, l) |> f
            List.map (fun elem -> elem.rule) elements |> eachProduction f
        | PAlt(left, right) -> 
            PAlt(left, right) |> f
            eachProduction f [left; right]
        | PConj(left, right) -> 
            PConj(left, right) |> f
            eachProduction f [left; right]
        | PMany x ->
            PMany x |> f
            eachProduction f [x]
        | PSome x ->
            PSome x |> f
            eachProduction f [x]
        | POpt x  ->
            POpt x |> f
            eachProduction f [x]
        | x -> f x
        )
        productionList 

let replaceLiteralsInProduction production (replacedLiterals:Dictionary<_,_>) (grammarTokens:HashSet<_>) token_format= 
    let rec _replaceLiterals = function
        | PSeq(elements, actionCode, l) -> 
            (
                elements 
                |> List.map (fun elem -> { elem with rule = _replaceLiterals elem.rule })
                ,actionCode, l
            )
            |> PSeq
        | PAlt(left, right) -> PAlt(_replaceLiterals left, _replaceLiterals right)
        | PConj(left, right) -> PConj(_replaceLiterals left, _replaceLiterals right)
        | PMany x -> PMany(_replaceLiterals x)
        | PSome x -> PSome(_replaceLiterals x)
        | POpt x  -> POpt(_replaceLiterals x)
        | PMetaRef (nt, s, args) -> PMetaRef(nt,s,args |> List.map _replaceLiterals)
        | PLiteral src -> 
            let str = src.text
            if replacedLiterals.ContainsKey str
            then PToken <| new Source(replacedLiterals.[str], src)
            else
                let token = ref(tokenName str token_format)
                while grammarTokens.Contains !token do
                    eprintfn 
                        "WARNING. ReplaceLiterals. Token with name %s is already exists. Seems, that you use token %s and literal %s. Please, resolve this ambiguity."
                        !token !token str
                    token := "YARD_" + !token
                replacedLiterals.Add(str, !token) 
                PToken <| new Source(!token, src) 
        | x -> x
    _replaceLiterals production

let replaceLiterals (ruleList: Rule<Source, Source> list) token_format = 
    
    let grammarTokens = new HashSet<_>()
    eachProduction
        (function
        | PToken name -> grammarTokens.Add name.text |> ignore
        | _ -> ()
        )
        (ruleList |> List.map (fun rule -> rule.body) )
    let replacedLiterals = new Dictionary<string, string>() // <literal text, token>
    ruleList |> List.map (fun rule -> {rule with body=replaceLiteralsInProduction rule.body replacedLiterals grammarTokens token_format} )  

type ReplaceLiterals() = 
    inherit Conversion()
        override this.Name = "ReplaceLiterals"
        override this.ConvertGrammar grammar = this.ConvertGrammar(grammar, [|"%s"|])
        override this.ConvertGrammar (grammar, token_format) = mapGrammar (fun rules -> replaceLiterals rules token_format.[0]) grammar
