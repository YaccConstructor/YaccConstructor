// Module TransformAux contains:
//  - common functions for grammar transformations
//
//  Copyright 2009,2010 Ilia Chemodanov
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

module Yard.Core.Convertions.TransformAux

open Yard.Core.IL
open Production
open Yard.Core.Namer

let getText = Source.toString
let rec getTextIL = function
    //| PRef(s,None) -> getText s
    | PRef(s,_) -> getText s
    | PToken(s) -> getText s
    | PLiteral(s) -> getText s
    | POpt(p) -> "(" + getTextIL p + ")?"
    | PSome(p) -> "(" + getTextIL p + ")*"
    | PMany(p) -> "(" + getTextIL p + ")+"
    | PAlt(l,r) -> "(" + getTextIL l + ")|(" + getTextIL r + ")"
    | PMetaRef(s,_,_) -> getText s
    | PSeq(elements, ac) ->
        "(" + (elements |> List.map (fun elem -> getTextIL elem.rule) |> String.concat " ") + "){" + 
        (match ac with Some(t) -> getText t | None -> "") + "}"
    | _ -> failwith "Unsupported meta param construct"

let createSimpleElem rulProd bind = 
    { omit = false; rule = rulProd; binding = bind; checker = None }

let createDefaultElem rulProd = createSimpleElem rulProd None

let createRule name args body _public mArgs = 
    { Rule.name = name     ; Rule.args = args      ; Rule.body = body
    ; Rule._public = _public ; Rule.metaArgs = mArgs
    }

/// Non-start rule with empty meta-arguments list
let createSimpleRule name args body =
    createRule name args body false []

/// Replace first (name) field in Source.t with new source 
let getNewSource (_, (l, c)) n = (n, (l, c))

let createSource n = (n, (0, 0))

let createRef ruleName _params = PRef (createSource ruleName, _params)

let addBinding _params = function None -> _params | Some x -> x::_params

/// Reduce param list to string. Arguments are separated by spaces
let createParams = 
    function
    | []         -> []
    | [_] as e   -> e
    | h ::_ as l -> let _params = String.concat " " (List.map getText l) 
                    in [getNewSource h _params]

/// Create option from empty or one element list
let list2opt = function
    | []     -> None
    | [h]    -> Some h (** list contains only one element!!! *)
    | _other -> invalidArg "list2opt" "Inpul list cannot contains more than one element." 

(* end of TransformAux *)
