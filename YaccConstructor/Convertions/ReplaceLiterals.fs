//  Module ReplaceLiterals contains:
//  - function, which replaces all literals in grammar by tokens and
//  writes a comment to header about them.
//
//  Copyright 2009, 2011 Konstantin Ulitin
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
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

module Yard.Core.Convertions.ReplaceLiterals

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

open System.Collections.Generic

let tokenName literal token_format=
    let upper = 
        String.collect 
            (function
            | '>' -> "GREATER"
            | '=' -> "EQUAL"
            | '<' -> "LESS"
            | ';' -> "SEMICOLON"
            | ':' -> "COLON"
            | '_' | ' ' -> "_"
            | c when System.Char.IsLetterOrDigit c   -> string (System.Char.ToUpper c) 
            | _ -> ""
            )
            literal
    if upper.Length=0 then 
        "EMPTY" 
    else 
        let format = Printf.StringFormat<string->string>(token_format)
        sprintf format upper

let rec eachProduction f productionList =
    List.iter 
        (function    
        | PSeq(elements, actionCode) -> f(PSeq(elements, actionCode)); eachProduction f (List.map (fun elem -> elem.rule) elements)
        | PAlt(left, right) -> f(PAlt(left, right)); eachProduction f [left; right]
        | PMany(x) -> f(PMany(x)); eachProduction f [x]
        | PSome(x) -> f(PSome(x)); eachProduction f [x]
        | POpt(x) -> f(POpt(x)); eachProduction f [x]
        | x -> f(x)
        )
        productionList 

let replaceLiteralsInProduction production (replacedLiterals:Dictionary<string, string>) (grammarTokens:HashSet<string>) token_format= 
    let rec _replaceLiterals = function
        | PSeq(elements, actionCode) -> 
            (
                elements 
                |> List.map (fun elem -> {elem with rule=(_replaceLiterals elem.rule)})
                ,actionCode
            )
            |> PSeq
        | PAlt(left, right) -> PAlt(_replaceLiterals left, _replaceLiterals right)
        | PMany(x) -> PMany(_replaceLiterals x)
        | PSome(x) -> PSome(_replaceLiterals x)
        | POpt(x) -> POpt(_replaceLiterals x)
        | PLiteral(str, _) -> 
            if (replacedLiterals.ContainsKey str) then
                PToken(replacedLiterals.[str], (0,0))
            else
                let token = ref(tokenName str token_format)
                while grammarTokens.Contains(!token) do
                    token := "YARD_" + !token
                replacedLiterals.Add(str, !token) 
                PToken(!token, (0,0)) 
        | x -> x
    _replaceLiterals production
    

let replaceLiterals (ruleList: Rule.t<Source.t, Source.t> list) (token_format:string) = 
    
    let grammarTokens = new HashSet<string>()
    eachProduction 
        (function
        | PToken(name,_) -> grammarTokens.Add(name) |> ignore ; ()
        | _ -> ()
        )
        (ruleList |> List.map (fun rule -> rule.body) )
    let replacedLiterals = new Dictionary<string, string>() // <literal text, token>
    ruleList |> List.map (fun rule -> {rule with body=replaceLiteralsInProduction rule.body replacedLiterals grammarTokens token_format} )  

type ReplaceLiterals() = 
    inherit Convertion()
        override this.Name = "ReplaceLiterals"
        override this.ConvertList ruleList = this.ConvertList(ruleList, [|"%s"|])
        override this.ConvertList(ruleList, token_format) =  replaceLiterals ruleList token_format.[0]
        override this.EliminatedProductionTypes = [""]