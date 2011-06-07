//  Module BuildAST contains:
//  - function, which replaces all action code with code that constructs AST in own format.
//
//  Copyright 2009-2011 Konstantin Ulitin
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

module BuildAST

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
    | PSeq(x, y) -> PSeq(x, y)
    | production -> PSeq([{new elem<Source.t, Source.t> with omit=false and rule=production and binding=None and checker=None}], None)

let printSeqProduction binding = function
    | POpt(x) -> sprintf "(match %s with None -> Node(\"opt\", []) | Some(ast) -> ast)" binding 
    | PToken(s,_) | PLiteral(s,_) -> !leafConstr s binding
    | PSome(p) -> sprintf "Node(\"some\", %s)" binding
    | PMany(p) -> sprintf "Node(\"many\", %s)" binding
    | _ -> binding

let rec _buildAST ruleName (production: t<Source.t, Source.t>) = 
    let isRef (elem:Production.elem<Source.t, Source.t>) = match elem.rule with PRef(_,_) -> true | _ -> false
    match production with
    | PSeq(elements, _) -> 
        if elements.Length = 1 && (match elements.Head.rule with PRef(("empty",_),_) -> true | _ -> false) then
            PSeq(elements, Some("Node(\"empty\", [])", (0,0)))
        else
            PSeq(
                elements 
                |> List.mapi 
                    (fun i elem -> 
                //Don't add bindings to omit or tokens or literals
                //TODO add omit check
                        match elem.rule with
                        | PToken _ | PLiteral _ -> { elem with binding=if !isTyped then Some((sprintf "S%d" (i+1)), (0,0)) else None }
                        | PRef _ ->  { elem with binding=Some((sprintf "S%d" (i+1)), (0,0)) }
                        | PAlt(left,right) -> { elem with binding=Some((sprintf "S%d" (i+1)), (0,0)); rule=PAlt(_buildAST (sprintf "%s_Alt%dL" ruleName (i+1)) left,_buildAST (sprintf "%s_Alt%dR" ruleName (i+1)) right) }
                        | PMany(p) -> { elem with binding=Some((sprintf "S%d" (i+1)), (0,0)); rule=PMany(_buildAST (sprintf "%s_Many%d" ruleName (i+1)) p) }
                        | PSome(p) -> { elem with binding=Some((sprintf "S%d" (i+1)), (0,0)); rule=PSome(_buildAST (sprintf "%s_Some%d" ruleName (i+1)) p) }
                        | POpt(p)  -> { elem with binding=Some((sprintf "S%d" (i+1)), (0,0)); rule=POpt (_buildAST (sprintf "%s_Opt%d"  ruleName (i+1)) p) }
                        | x -> { elem with binding=Some((sprintf "S%d" (i+1)), (0,0)); rule=_buildAST (sprintf "%s_INNER%d" ruleName (i+1)) elem.rule }
                    )
                ,(                    
                    elements
                    |> List.mapi (fun i elem -> (i, elem)) 
                    |> List.choose 
                        (fun (i, elem) -> 
                            if elem.omit
                            then None 
                            else Some(printSeqProduction (sprintf "S%d" (i+1)) elem.rule))
                    |> String.concat "; "
                    |> sprintf "Node(\"%s\", [%s])" ruleName
                    , (0,0)
                )|> Some)
    | x -> _buildAST ruleName (seqify x)

let buildAST (ruleList: Rule.t<Source.t, Source.t> list) tokenType = 
    if tokenType <> "" then 
        leafConstr := (sprintf "Leaf(\"%s\", %s)");
        isTyped := true
    ruleList |> List.map (fun rule -> {rule with body=(_buildAST rule.name rule.body) } )

type BuildAST() = 
    inherit Convertion()
        override this.Name = "BuildAST"
        override this.ConvertList ruleList = buildAST ruleList "" 
        override this.ConvertList(ruleList, tokenType) = buildAST ruleList tokenType
        override this.EliminatedProductionTypes = [""]

