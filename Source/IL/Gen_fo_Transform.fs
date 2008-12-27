#light "off"

(** Module TransformAux contains:
 *  - common functions for grammar transformations
 *
 *  Author: Ilia
 *)

module Gen_fo_Transform

open IL
//open Misc.Generation
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

//let createEmptyRef = createRef getEmptyAltName None

//let createItemRef = createRef getItem None

let addBinding _params = function None -> _params | Some x -> x::_params

let createParams = 
    function
    | []        -> []
    | h::t as l -> match t 
                   with
                   | [] -> [h]
                   | _  -> let _params = String.concat " " (List.map getText l) 
                           in [getNewSource h _params]

(** create option from empty or one element list *)
let createOpt = 
    function
    | []   -> None
    | h::_ -> Some h (* list contains only one element!!! *)

(* end of TransformAux *)
