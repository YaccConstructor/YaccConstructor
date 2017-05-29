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

module Yard.Core.Conversions.BuildAST

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production


open System.Collections.Generic

(* You need to add following code in grammar header 
type AST<'token> =
    | Node of string * AST<'token> list
    | Leaf of string * 'token

OR

type AST =
    | Node of string * AST list
    | Leaf of string 
    
Type is defined by tokenType parameter of rule application
*)


// different logic for untyped and typed AST
let isTyped = ref false
let leafConstr = ref (fun token binding -> sprintf "Leaf(\"%s\")" token)

let seqify = function
    | PSeq(x, y,l) -> PSeq(x, y,l)
    | production -> PSeq([{new elem<Source.t, Source.t> with omit=false and rule=production and binding=None and checker=None}], None,None)

let printSeqProduction binding = function
    | POpt x -> sprintf "(match %s with None -> Node(\"opt\", []) | Some(ast) -> ast)" binding 
    | PToken s | PLiteral s -> !leafConstr s.text binding
    | PSome p -> sprintf "Node(\"some\", %s)" binding
    | PMany p -> sprintf "Node(\"many\", %s)" binding    
    | _ -> binding

let rec _buildAST ruleName (production: t<Source.t, Source.t>) = 
    let isRef (elem:Production.elem<Source.t, Source.t>) = match elem.rule with PRef(_,_) -> true | _ -> false
    let isTopLevelAlt elem = 
        match elem.rule with
        | PAlt _ -> true
        | _ -> false
    match production with
    | PSeq(elements, _,l) ->
        (*if elements.Length = 1 && (match elements.Head.rule with PRef(("empty",_),_) -> true | _ -> false) then
            PSeq(elements, Some("Node(\"empty\", [])", (0,0,"")))
        else*)
            PSeq(
                elements
                |> List.mapi
                    (fun i elem ->
                //Don't add bindings to omit or tokens or literals
                //TODO add omit check
                        match elem.rule with
                        | PToken _ | PLiteral _ -> { elem with binding=if !isTyped then Some(new Source.t(sprintf "S%d" (i+1))) else None }
                        | PRef _ ->  { elem with binding=Some(new Source.t(sprintf "S%d" (i+1))) }
                        | PAlt(left,right) -> { elem with binding=Some(new Source.t(sprintf "S%d" (i+1)));
                                                          rule=PAlt(_buildAST (sprintf "%s_Alt%dL" ruleName (i+1)) left,
                                                                    _buildAST (sprintf "%s_Alt%dR" ruleName (i+1)) right) }
                        | PConj(left,right) -> { elem with binding=Some(new Source.t(sprintf "S%d" (i+1)));
                                                          rule=PConj(_buildAST (sprintf "%s_Conj%dL" ruleName (i+1)) left,
                                                                    _buildAST (sprintf "%s_Conj%dR" ruleName (i+1)) right) }
                        | PMany p -> { elem with binding=Some(new Source.t(sprintf "S%d" (i+1)));
                                                 rule=PMany(_buildAST (sprintf "%s_Many%d" ruleName (i+1)) p) }
                        | PSome p -> { elem with binding=Some(new Source.t(sprintf "S%d" (i+1)));
                                                 rule=PSome(_buildAST (sprintf "%s_Some%d" ruleName (i+1)) p) }
                        | POpt p  -> { elem with binding=Some(new Source.t(sprintf "S%d" (i+1)));
                                                 rule=POpt (_buildAST (sprintf "%s_Opt%d"  ruleName (i+1)) p) }
                        | x -> { elem with binding=Some(new Source.t(sprintf "S%d" (i+1)));
                                           rule=_buildAST (sprintf "%s_INNER%d" ruleName (i+1)) elem.rule }
                    )
                ,
                    elements
                    |> List.mapi (fun i elem -> i,elem) 
                    |> List.choose
                        (fun (i, elem) ->
                            if elem.omit || isTopLevelAlt elem
                            then None
                            else Some(printSeqProduction (sprintf "S%d" (i+1)) elem.rule))
                    |> String.concat "; "
                    |> sprintf "Node(\"%s\", [%s])" ruleName
                    |> (fun x -> Some <| new Source.t(x))
                ,l)
    | PAlt(left,right) -> 
        PAlt(_buildAST (sprintf "%s_Alt%dL" ruleName 1) left,_buildAST (sprintf "%s_Alt%dR" ruleName 1) right)
    | PConj(left,right) -> 
        PConj(_buildAST (sprintf "%s_Conj%dL" ruleName 1) left,_buildAST (sprintf "%s_Conj%dR" ruleName 1) right)
    | x -> seqify x |> _buildAST ruleName

let buildAST (ruleList: Rule.t<Source.t, Source.t> list) tokenType =
    if System.String.IsNullOrEmpty tokenType then
        leafConstr := (sprintf "Leaf(\"%s\", %s)")
        isTyped := true
    ruleList |> List.map (fun rule -> {rule with body=(_buildAST rule.name.text rule.body) } )


type BuildAST() =
    inherit Conversion()
        override this.Name = "BuildAST"
        override this.ConvertGrammar(grammar, tokenType) = 
            let token = 
                if tokenType.Length > 0
                then tokenType.[0]
                else ""
            mapGrammar (fun rules -> buildAST rules token) grammar

