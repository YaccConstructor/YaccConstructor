//  Module BuildAstSimple contains:
//  - function, which replaces all action code with code that constructs AST in own format.
//
//  Copyright 2009, 2010, 2011 Konstantin Ulitin
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

module Yard.Core.Conversions.BuildAstSimple

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production

open System.Collections.Generic

(* You need to add following code in grammar header 
type AST<'token> =
    | Node of string * AST<'token> list
    | Leaf of string * 'token
*)

let leafConstr = sprintf "Leaf(\"%s\", %s)"
let seqify = function
    | PSeq(x, y, l) -> PSeq(x, y, l)
    | production -> PSeq([{new elem<Source.t, Source.t> with omit=false and rule=production and binding=None and checker=None}], None, None)

let printSeqProduction binding = function
    | POpt(x) -> sprintf "(match %s with None -> [] | Some(ast) -> ast)" binding 
    | PToken s | PLiteral s -> leafConstr s.text binding
    | PSome(p) -> sprintf "List.concat %s" binding
    | PMany(p) -> sprintf "List.concat %s" binding
    | _ -> binding

/// ruleName is empty when production is inner and action code returns list of nodes
let rec _buildAstSimple ruleName (production: t<Source.t, Source.t>) = 
    match production with
    | PSeq(elements, _, l) -> 
        //if elements.Length = 1 && (match elements.Head.rule with PRef(("empty",_),_) -> true | _ -> false) then
        //    PSeq(elements, Some <| new Source.t("Node(\"empty\", [])"))
        //else
        if elements.Length = 1 && (match elements.Head.rule with PRef(n,_) when n.text = "error" -> true | _ -> false) then
            PSeq(elements, Some <| new Source.t("Node(\"error\", [])"), l)
        else
            PSeq(
                elements 
                |> List.mapi 
                    (fun i elem -> 
                        if elem.omit then 
                            elem
                        else
                            let binding = Some <| new Source.t(sprintf "S%d" (i+1))
                            match elem.rule, elem.binding with
                            | PToken _, None | PLiteral _, None | PRef _, None -> { elem with binding=binding }
                            | PToken _, Some _ ->  { elem with binding=binding }
                            | PAlt(left,right), _ -> { elem with binding=binding; rule=PAlt(_buildAstSimple "" left,_buildAstSimple "" right) }
                            | PMany(p), _ -> { elem with binding=binding; rule=PMany(_buildAstSimple "" p) }
                            | PSome(p), _ -> { elem with binding=binding; rule=PSome(_buildAstSimple "" p) }
                            | POpt(p), _  -> { elem with binding=binding; rule=POpt (_buildAstSimple "" p) }
                            | PSeq([elem_inner],None, l), _ -> { elem_inner with binding=binding; rule=_buildAstSimple ruleName elem_inner.rule }
                            | x, _ -> { elem with binding= Some <| new Source.t(sprintf "FAIL(%A)_S%d" x (i+1))
                                                  rule=_buildAstSimple ruleName elem.rule }
                    )
                ,
                    elements
                    |> List.mapi (fun i elem -> (i, elem)) 
                    |> List.choose 
                        (fun (i, elem) -> 
                            if elem.omit then 
                                None
                            else
                                match elem.rule with
                                | PToken _ | PLiteral _ | PRef _ -> Some("["+ printSeqProduction (sprintf "S%d" (i+1)) elem.rule + "]") //returns one element
                                | _ -> Some(printSeqProduction (sprintf "S%d" (i+1)) elem.rule) //returns list
                        )
                    |> String.concat "; "
                    |> if ruleName="" then sprintf "List.concat [%s]" else sprintf "Node(\"%s\", List.concat [%s])" ruleName
                    |> (fun x -> Some <| new Source.t(x))
                , l)

    | PAlt(left, right) -> PAlt(_buildAstSimple ruleName left, _buildAstSimple ruleName right)
    | x -> _buildAstSimple ruleName (seqify x)

let buildAstSimple (ruleList: Rule.t<Source.t, Source.t> list)  = 
    ruleList |> List.map (fun rule -> {rule with body=(_buildAstSimple rule.name.text rule.body) } )

type BuildAstSimple() = 
    inherit Conversion()
        override this.Name = "BuildAstSimple"
        override this.ConvertList (ruleList,_) = buildAstSimple ruleList 
        override this.EliminatedProductionTypes = [""]

