//  Module ExpandEBNF contains:
//  - functions for rules convertion from EBNF to metarules 
//    (rules parametrized by other rules)
//
//  Copyright 2009,2010 by Ilia Chemodanov
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
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



module Yard.Core.Convertions.ExpandEbnf

open Yard.Core
open Yard.Core.IL
open Production
open Yard.Core.Namer
open TransformAux

let createBinding bindName = Some ( createSource bindName )

let createMetaRef name = 
    PMetaRef (createSource name, None, [ PRef(createSource getItem, None) ])

let createMetaRule name body = 
    createRule name [] body false [ createSource getItem ]

let getAddToListAction = createSource addToListAction

let getMetaSeq metaName = 
    PSeq(
      [ createSimpleElem (createRef getItem None)(*createItemRef*) (createBinding headBind);
        createSimpleElem (createMetaRef metaName) (createBinding tailBind)],
       (Some getAddToListAction))

let createListAction elem = Some ( oneElemListAction elem |> createSource )

let createEmptySeq action = 
    PSeq([ createDefaultElem (createRef getEmptyAltName None)(*createEmptyRef*)],action)

let createItemSeq binding action = 
    PSeq([ createSimpleElem (createRef getItem None)(*createItemRef*) binding ],action)

(** create metarule for option: "item?" *)
let createSomeAction x = someAction x |> createSource

let createNoneAction = createSource noneAction

let getOptBody = 
    let binding = createBinding getItemBind in 
    let action = Some ( createSomeAction getItemBind ) in
    let action' = Some createNoneAction in
    PAlt ( createItemSeq binding action, createEmptySeq action' )

let getMetaOpt = createMetaRule Names.opt getOptBody

(** create metarule for some: "item+" *)
let getSomeBody =
    let binding = createBinding headBind 
    let action = createListAction headBind 
    PAlt (createItemSeq binding action, getMetaSeq Names.some)

let getMetaSome = createMetaRule Names.some getSomeBody

(** create metarule for many: "item*" *)
let getManyBody = 
    PAlt (createEmptySeq (createListAction ""), getMetaSeq Names.many)

let getMetaMany = createMetaRule Names.many getManyBody

(** start of converting... *)
//let transformError () = reportError "in transforming EBNF to metarules"

let rec convertToMeta (r:(Rule.t<Source.t,Source.t>)) = 
    let insideArguments =
        r.metaArgs
        |> List.map (fun x -> PRef (x, None))
    let rec getMeta nr paramList metaName = 
        function
        | PRef (rName, _params) -> (PMetaRef (createSource metaName, _params, [PRef(rName, None)]), nr)
        | PToken t 
        | PLiteral t            -> (PMetaRef (createSource metaName, None, [PRef(t, None)]), nr)
        | other                 -> let newName = createNewName (createSource getItem) in
                                   let _params = createParams paramList in
//                                   let newRule = createSimpleRule (getText newName) _params other in
                                   let newRule = createRule (getText newName) _params other false r.metaArgs in
                                   let rules = convertToMeta newRule in 
                                   (PMetaRef (createSource metaName, list2opt _params, [PMetaRef(newName, None, insideArguments)]), nr @ rules)
    and bodyToMeta rs     (** new rules *) 
                  _params (** rule parameters and bindings *) =     
        let nameOf = function 
            | POpt  _ -> Names.opt 
            | PSome _ -> Names.some 
            | PMany _ -> Names.many 
            | _       -> invalidArg "nameOf" "Argument may be only POpt, PSome or PMany."
        in
        let elemToMeta paramList e = 
            let (b, rs') = bodyToMeta [] paramList e.rule 
            in ({ e with Production.rule = b }, rs')
        function
        | POpt  r as r' -> getMeta rs _params (nameOf r') r
        | PSome r as r' -> getMeta rs _params (nameOf r') r
        | PMany r as r' -> getMeta rs _params (nameOf r') r
        | PSeq (seq, a) -> 
          let rec seqToMeta _params' = function
             | []   -> ([], [])
             | h::t -> 
               let (e', rs') = elemToMeta _params' h
               let (l', rs'') = seqToMeta (addBinding _params' h.binding) t 
               (e'::l', rs' @ rs'')
          in let (seq', nr) = seqToMeta _params seq
                in (PSeq (seq', a), rs @ nr)
        | PAlt (l, r) -> 
          let (l', rs') = bodyToMeta rs _params l in
          let (r', rs'') = bodyToMeta rs' _params r 
          in (PAlt (l', r'), rs'')
        (* do nothing in other case *)
        | PMetaRef(name, attrs, mRules) ->
            let rec listToMeta _params' = function
                | []   -> ([], [])
                | h::t -> 
                    let (e', rs') = bodyToMeta [] _params' h
                    let (l', rs'') = listToMeta _params' t 
                    (e'::l', rs' @ rs'')
            let (mArgs', nr) = listToMeta _params mRules
            (PMetaRef(name, attrs, mArgs'), rs @ nr)
        | other -> (other, rs)
    let (b', l') = bodyToMeta [] r.args r.body
    in l' @ [{ r with Rule.body = b' }]

(** main function *)
let convertEBNFtoMeta l = 
    let l' = List.map (convertToMeta) l |> List.concat in
    if l' |> List.exists (fun r -> r.name = Names.many) then
        l'
    else
        getMetaOpt :: getMetaSome :: getMetaMany :: l'

(* end of converting EBNF to metarules *)

/// Convert rules from EBNF to metarules 
/// (rules parametrized by other rules)
type ExpandEbnf() = 
    inherit Convertion()
        override this.Name = "ExpandEbnf"
        override this.ConvertList ruleList = convertEBNFtoMeta ruleList
        override this.EliminatedProductionTypes = ["POpt"; "PSome"; "PMany"]

