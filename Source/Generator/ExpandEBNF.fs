#light "off"


(** Module ExpandEBNF contains:
 *  - functions for rules convertion from EBNF to metarules 
 *    (rules parametrized by other rules)
 *
 *  Author: Ilia
 *)
module Yard.Core.ExpandEBNF 

open IL
open Production
open Namer
open TransformAux

let createBinding bindName = Some ( createSource bindName )

let createMetaRef name = 
    PMetaRef (createSource name, None, [ createSource getItem ])

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
let getSomeBody = let binding = createBinding headBind in
    let action = createListAction headBind in
    PAlt (createItemSeq binding action, getMetaSeq Names.some)

let getMetaSome = createMetaRule Names.some getSomeBody

(** create metarule for many: "item*" *)
let getManyBody = 
    PAlt (createEmptySeq (createListAction ""), getMetaSeq Names.many)

let getMetaMany = createMetaRule Names.many getManyBody

(** start of converting... *)
//let transformError () = reportError "in transforming EBNF to metarules"

let rec convertToMeta (r:(Rule.t<Source.t,Source.t>)) = 
    let rec getMeta nr paramList metaName = 
        function
        | PRef (rName, _params) -> (PMetaRef (createSource metaName, _params, [rName]), nr)
        | PToken t 
        | PLiteral t            -> (PMetaRef (createSource metaName, None, [t]), nr)
        | other                 -> let newName = createNewName (createSource getItem) in
                                   let _params = createParams paramList in
                                   let newRule = createSimpleRule (getText newName) _params other in
                                   let rules = convertToMeta newRule in 
                                   (PMetaRef (createSource metaName, list2opt _params, [newName]), nr @ rules)
    and bodyToMeta rs     (** new rules *) 
                  _params (** rule parameters and bindings *) =     
        let nameOf = function 
            | POpt  _ -> Names.opt 
            | PSome _ -> Names.some 
            | PMany _ -> Names.many 
            | _       -> invalid_arg "nameOf"
        in
        function
        | POpt  r as r' -> getMeta rs _params (nameOf r') r
        | PSome r as r' -> getMeta rs _params (nameOf r') r
        | PMany r as r' -> getMeta rs _params (nameOf r') r
        | PSeq (seq, a) -> 
          let elemToMeta paramList e = 
            let (b, rs') = bodyToMeta [] paramList e.rule 
            in ({ e with Production.rule = b }, rs')
          in let rec seqToMeta _params' = function
             | []   -> ([], [])
             | h::t -> let (e', rs') = elemToMeta _params' h in 
               let (l', rs'') = seqToMeta (addBinding _params' h.binding) t 
               in (e'::l', rs' @ rs'')
             in let (seq', nr) = seqToMeta _params seq
                in (PSeq (seq', a), rs @ nr)
        | PAlt (l, r) -> 
          let (l', rs') = bodyToMeta rs _params l in
          let (r', rs'') = bodyToMeta rs' _params r 
          in (PAlt (l', r'), rs'')
        (* do nothing in other case *)
        | other -> (other, rs)
in
    let (b', l') = bodyToMeta [] r.args r.body
    in l' @ [{ r with Rule.body = b' }]

(** main function *)
let convertEBNFtoMeta l = 
    let l' = List.map (convertToMeta) l |> List.concat
    in getMetaOpt :: getMetaSome :: getMetaMany :: l'

(* end of converting EBNF to metarules *)

