#light "off"

(** Module TransformAux contains:
 *  - common functions for grammar transformations
 *
 *  Author: Ilia
 *)

module Yard.Core.TransformAux

open IL
open Production
open Namer

let getText = IL.Source.toString

let createSimpleElem rulProd bind = 
    { omit = false; rule = rulProd; binding = bind; checker = None }

let createDefaultElem rulProd = createSimpleElem rulProd None

let createRule name args body _public mArgs = 
    { Rule.name = name     ; Rule.args = args      ; Rule.body = body
    ; Rule._public = _public ; Rule.metaArgs = mArgs
    }

let createSimpleRule name args body =
    createRule name args body false []

let getNewSource (_, (l, c)) n = (n, (l, c))

let createSource n = (n, (0, 0))

let createRef ruleName _params = PRef (createSource ruleName, _params)

let addBinding _params = function None -> _params | Some x -> x::_params

let createParams = 
    function
    | []         -> []
    | [_] as e   -> e
    | h ::_ as l -> let _params = String.concat " " (List.map getText l) 
                    in [getNewSource h _params]

(** create option from empty or one element list *)
let list2opt = function
    | []     -> None
    | [h]    -> Some h (** list contains only one element!!! *)
    | _other -> invalidArg "list2opt" "Inpul list cannot contains more than one element." 

(* end of TransformAux *)
