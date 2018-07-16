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

module Yard.Core.Namer

open IL
open System.Collections.Generic

(** prefix for all items created by Yard *)
let private withPrefix s = "yard_" + s

(** global variable for number of current generated rule *)
let private curNum = ref 0

//let private genYardName n (b, e, f) = new Source(sprintf "%s_%d" (withPrefix n) !curNum, b, e, f)

module Names =
 begin
//sig 
//  (** metarule name for option: "item?" *)
//  val opt:string 
//  (** metarule name for some: "item+" *)
//  val some:string 
//  (** metarule name for many: "item*" *)
//  val many:string 
//  (** rule prefix for sequence *)
//  val seq:string 
//  (** rule prefix for alternative *)
//  val alt:string 
//  (** rule prefix for predicate *)
//  val predicate:string
//end 

   let x = withPrefix

   let opt  = x "option"
   let some = x "nlist"
   let many = x "list"
   let seq  = x "seq"
   let alt  = x "alt"
   let predicate = x "predicate"
   let brackets = x "exp_brackets"
   let leftRec = x "lr_" 
   let repeat = x "repeat"
   let conjunction = x "conjunction"
end 

let usedNames = new HashSet<_>()

let initNamer (grammar : Grammar<_,_>) =
    curNum := 0
    usedNames.Clear()
    let add s = usedNames.Add s |> ignore
    let addSrc (s : Source) = usedNames.Add s.text |> ignore
    let acceptable c = System.Char.IsLetterOrDigit c || c = '_'
    let addAC (s : Source) =
        let len = ref 0
        s.text |> String.iteri (fun i c ->
            if acceptable c
            then incr len
            elif !len <> 0
            then add <| s.text.Substring(i - !len, !len)
        )
        if !len <> 0
        then add <| s.text.Substring(s.text.Length - !len, !len)

    let rec walk = function
        | PRef (n,args) ->
            addSrc n
            args |> Option.iter addAC
        | PMetaRef (n, args, metas) ->
            addSrc n
            args |> Option.iter addAC
            metas |> List.iter walk
        | PToken n -> addSrc n
        | PLiteral n -> addSrc n
        | PAlt (l,r) ->
            walk l
            walk r
        | PConj (l,r) ->
            walk l
            walk r
        | PNeg x -> walk x
        | PMany x | PSome x | POpt x | PRepet (x,_,_) -> walk x
        | PSeq (elems, ac, lbl) ->
            elems |> List.iter (fun e ->
                e.binding |> Option.iter addAC
                e.checker |> Option.iter addAC
                walk e.rule
            )
        | PPerm elems -> elems |> List.iter walk
        | PShuff _ -> failwith "Unsupported"

    grammar |> List.iter (fun mod' ->
        add <| getModuleName mod'
        mod'.rules |> List.iter (fun rule ->
            addSrc rule.name
            rule.args |> List.iter addAC
            rule.metaArgs |> List.iter addSrc
            walk rule.body
        )
    )

let newName (n : string) =
    let addPrefix = 
        try
            let yardPrefix = withPrefix ""
            if n.Substring (0, min n.Length yardPrefix.Length) = yardPrefix 
            then n
            else withPrefix n
        with (*Invalid_argument*) _ -> withPrefix n
    incr curNum
    let res = ref <| sprintf "%s_%d" addPrefix !curNum
    while usedNames.Contains !res do
        incr curNum
        res := sprintf "%s_%d" addPrefix !curNum
    usedNames.Add !res |> ignore
    !res

let newSource (old : Source) = new Source(newName old.text, old)

let genNewSourceWithRange (name : string) (body : Production<_,_>) =
    let rec getBegin = function
        | PSeq (s, ac,l) ->
            match s with
            | h::_ -> getBegin h.rule
            | _ ->
                match ac with
                | Some (ac : Source) -> ac
                | None -> failwith "Empty sequence without action code"
        | PRef (n,_) -> n
        | PAlt (l, r) -> getBegin l
        | PConj (l, r) -> getBegin l
        | PLiteral l -> l
        | PMany b -> getBegin b
        | PSome b -> getBegin b
        | POpt b -> getBegin b
        | PToken t -> t
        | PMetaRef (n,_,_) -> n
        | PRepet (b,_,_) -> getBegin b
        | PPerm _  as x -> failwithf "Unrealized construction: %A" x
        | PShuff _ -> failwith "Unsupported"
        | PNeg _ -> failwith "Unsupported"
    let oldSource = getBegin body
    new Source(name, oldSource)

(** returns true if given name is metarule name for EBNF *)
let isEBNFmeta name = 
    name = Names.opt || name = Names.some || name = Names.many

//let getMetaRuleName rName = 
//    try
//      let ind = String.rindex rName '_' in
//      let rName' = rName.Substring(0, ind) 
//      in
//        if (isEBNFmeta rName') then rName'
//        else begin
//          let ind' = (rName'.IndexOf '_') + 1
//          in rName'.Substring(ind', (rName'.Length - ind'))
//        end
//    with Not_found -> rName

/// formal parameter name in metarules for EBNF clauses
let getItem = "item"

let getItemBind = withPrefix getItem

/// Does name have prefix 'item'
let isItem (name:string) = 
    let len = getItem.Length in
    if  name.Length >= len then
        name.Substring(0, len) = getItem 
    else false
    
(** names specific for Elkhound, *)

//(** there is special nonterminal for empty alternative: empty *)
//let getEmptyAltName = "empty"

(** name of parameter in function "keep" *)
let getKeepParamName = withPrefix "val"

let headBind = withPrefix "h"

let tailBind = withPrefix "t"

let oneElemListAction = sprintf " [%s] "

let addToListAction = sprintf " %s::%s " headBind tailBind

let someAction = sprintf " Some %s "

let noneAction = " None "

/// prefix for Elkhound bindings
let withElkPrefix x = "_elk_" + x

/// token type (need for using bindings with tokens)
let withTokenPrefix token_name = 
    sprintf "'%stoken_%s" (withPrefix "") token_name

/// type of semantic value
let createTypeName ruleName =  sprintf "'%stype_%s" (withPrefix "") ruleName

/// returns file name for tokens
let createTknFileName (fname: string) = System.IO.Path.GetFileNameWithoutExtension fname |> sprintf "tokens_%s.tok"

(*
/// Create new name for rule
let createRuleName name =  sprintf "'%srule_%s_%d" (withPrefix "") (fst name) (incr curNum; !curNum)

/// Create new name for argument
let createArgName =  sprintf "'%sarg_%d" (withPrefix "") (incr curNum; !curNum)
*)
/// returns token type for literal
let createLiteralToken number = sprintf "LITERAL_%d" number

/// returns token name for EOF (End Of File)
let getEofTokenName = (withPrefix "EOF").ToUpper()
    
(* end of module Namer *)


