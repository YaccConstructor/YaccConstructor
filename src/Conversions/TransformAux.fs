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

module Yard.Core.Conversions.TransformAux

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
    | PConj(l,r) -> "(" + getTextIL l + ")&(" + getTextIL r + ")"
    | PMetaRef(s,_,_) -> getText s
    | PSeq(elements, ac, _) ->
        "(" + (elements |> List.map (fun elem -> getTextIL elem.rule) |> String.concat " ") + "){" + 
        (match ac with Some(t) -> getText t | None -> "") + "}"
    | _ -> failwith "Unsupported meta param construct"

let createSimpleElem rulProd bind = 
    { omit = false; rule = rulProd; binding = bind; checker = None }

let createDefaultElem rulProd = createSimpleElem rulProd None

let createRule name args body _public mArgs = 
    { Rule.name = name     ; Rule.args = args      ; Rule.body = body
    ; Rule.isStart = _public ; Rule.metaArgs = mArgs; Rule.isInline = false; Rule.isPublic = false
    }

/// Non-start rule with empty meta-arguments list
let createSimpleRule name args body =
    createRule name args body false []

/// Replace first (name) field in Source.t with new source 
let getNewSource (old : Source.t) n =
    new Source.t(n, old.startPos, old.endPos, old.file)

//let createSource n = new Source.t(n)

//let createRef ruleName _params = PRef (createSource ruleName, _params)

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
    | _other -> invalidArg "list2opt" "Input list cannot contains more than one element." 

let opt2list = function
    | None -> []
    | Some x -> [x]

(* end of TransformAux *)
