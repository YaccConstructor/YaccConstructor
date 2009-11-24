#light "off"

(** Module ExpandMeta contains:
 *  - function, which expands metarules 
 *
 *  Author: Ilia
 *)
module Yard.Core.ExpandMeta 
open IL
open Production
//open Misc
//open Diagnostics
open Namer
open TransformAux

(** if rule has metaArgs then it's metarule *)
let isMetaRule (r:Rule.t<'a,'b>) = r.metaArgs <> []

(** find metarule with given name in hash map of collected metarules *)
let findMetaRule (tbl:Hashtbl.t<string,Rule.t<Source.t,Source.t>>) mName = 
    match tbl.TryFind mName with
    | None -> failwith "undeclared metarule "(*reportError ("undeclared metarule " ^ mName)*) ; None
    | res    -> res

(** reports error with arguments of metarule *)
let invalidArgs name = 
    failwith "invalid number of parameters in metarule "
    (*reportError ("invalid number of parameters in metarule " ^ name)*) ; []

(** create list of pairs: (<formal parameter>, <actual parameter>) *)
let createPairsList name l1 l2 =
     try List.map2(fun x  y -> getText x, getText y ) l1 l2 
     with :? System.ArgumentException -> invalidArgs  name
    


(** update actual parameters if condition is true and there are no oldParams
 *  (this function is used when need pass parameters to EBNF metarules or 
 *  their metaparameter - item)
 *)
let updateActParams cond newParams oldParams = 
    if cond
    then ( match oldParams with None -> newParams | Some _ -> oldParams )
    else oldParams

let getFormalArgs name actParams formalArgs =     
    if isEBNFmeta name 
    (** for EBNF metarules formal parameters equal actual *)
    then match actParams with 
         | None   -> formalArgs
         | Some x -> [x]         
    else formalArgs

(** create list of strings from list of Source *)
let createStrList = List.map (fun x -> getText x) 

(** key for hash table *)
let getKey metaName metaArgs = 
    String.concat "" ((getText metaName)::(createStrList metaArgs))

(** returns actual parameter (if given list contains it with given formal) 
 *  (args is list of pairs (<formal>, <actual>) )
 *)
let getActualParam formalName args = List.try_assoc formalName args

let replaceFormal args formal = 
    let act = getActualParam (getText formal) args in
    match act with
    | Some x -> createSource x
    | None   -> formal

(** replace formal metaparameters with actual *)
let replaceFormals mArgs args =
     List.map (replaceFormal args) mArgs

(** expand references to metarules 
 *  and generate new rules every such reference 
 *)
let rec expandMeta body (metaRulesTbl:Hashtbl.t<string,Rule.t<Source.t,Source.t> >)
                        (refsTbl:Hashtbl.t<string,Source.t>)
                    res = 
let rec transformBody ruleName actParams args nRules =
    function
    | PRef (r, p) -> 
      let rName = getText r in
      (** pass actual parameters for item 
       *  (which is replaced with yard_item_...) 
       *)
      let aParams = updateActParams (isItem rName) actParams p in 
      let param = getActualParam rName args in 
      (match param with
      | Some n -> (PRef (getNewSource r n, aParams), nRules)
      | None   -> (PRef (r, p), nRules)
      )
    | PAlt (l, r) -> 
      let (l', nRules') = transformBody ruleName actParams args nRules l 
      in let (r', nRules'') = transformBody ruleName actParams args nRules' r 
      in (PAlt (l', r'), nRules'')
    | PSeq (seq, a) -> 
      let rec transformSeq args = function
          | h::t -> 
            let (body', newRules) = transformBody ruleName actParams 
                                                  args [] h.rule 
            in let (seq', newRules') = transformSeq args t 
               in ({ h with rule = body' }::seq', newRules @ newRules')
          | []   -> ([], [])
      in let (l', nRules') = transformSeq args seq 
         in (PSeq (l', a), nRules @ nRules')
    | PToken t -> (PToken t, nRules)
    | PLiteral r -> (PLiteral r, nRules)

    | PMetaRef (n, p, mArgs) -> 
      (** we accept other metarule as parameter *)
      let n' = (*printPairList args ;*) replaceFormal args n in
      let mRuleName' = getText n' in
      let p' = updateActParams (isEBNFmeta mRuleName') actParams p in
      let mArgs' = replaceFormals mArgs args in
      let (b', nRules') = expandMetaRef n' p' mArgs'
      in (b', nRules @ nRules')      
    | POpt r -> 
      let (b', nRules') = transformBody ruleName actParams args nRules r
      in (POpt b', nRules')
    | PSome r -> 
      let (b', nRules') = transformBody ruleName actParams args nRules r
      in (PSome b', nRules')
    | PMany r -> 
      let (b', nRules') = transformBody ruleName actParams args nRules r
      in (PMany b', nRules')
(*    | other -> reportError "EBNF construction has already be transformed" 
      ; (other, nRules)
*)
and transformRule rName _params (metaRule: Rule.t<Source.t,Source.t>) metaArgs =
    let       
      args = createPairsList metaRule.name metaRule.metaArgs metaArgs 
    in 
    let fArgs = getFormalArgs metaRule.name _params metaRule.args
    in let args' = list2opt fArgs in
    let (b, newRules) = transformBody rName args' args [] metaRule.body 
    in ( newRules @ [ createRule rName fArgs b metaRule._public [] ] )

and genNewRule rName args' mRulesTbl metaName' metaArgs =
    let res = findMetaRule mRulesTbl metaName' in 
    match res with 
    | Some metaRule -> transformRule rName args' metaRule metaArgs
    | None -> []

and expandMetaRef metaName _params metaArgs' = 
    let key = getKey metaName metaArgs' in
    let rs = refsTbl.TryFind key in
    match rs with
    | None -> let rName = createNewName metaName in       
      let eRules = Hashtbl.add refsTbl key rName ;
                   genNewRule (getText rName) _params 
                              metaRulesTbl (getText metaName) metaArgs' 
      in let b = PRef (rName, _params) 
         in (b, res @ eRules)
    | Some rn -> (PRef (rn, _params), res)

in
    match body with
    | PMetaRef (mn, p, mArgs) -> expandMetaRef mn p mArgs
    | PSeq (seq, a) -> 
      let rec expandSeq mRulesTbl rfsTbl rs = function
      | h::t -> let (b, res') = expandMeta h.rule mRulesTbl rfsTbl rs in 
        let (b', res'') = expandSeq mRulesTbl rfsTbl res' t
        in ({ h with rule = b }::b', res'')
      | [] -> ([], rs)
      in let (eSeq, res') = expandSeq metaRulesTbl refsTbl res seq      
         in (PSeq (eSeq, a), res')
    
    | PAlt (l, r) -> 
      let (l', res') = expandMeta l metaRulesTbl refsTbl res in
      let (r', res'') = expandMeta r metaRulesTbl refsTbl res'
      in (PAlt(l', r'), res'')
    | POpt r  -> let (r', res') = expandMeta r metaRulesTbl refsTbl res 
                 in (POpt r', res')
    | PSome r -> let (r', res') = expandMeta r metaRulesTbl refsTbl res 
                 in (PSome r', res')
    | PMany r -> let (r', res') = expandMeta r metaRulesTbl refsTbl res 
                 in (PMany r', res')
    | other -> (other, res)

(** grammar processing:
 *  - collect metarules
 *  - call metarules expanding 
 *)
let rec handleMeta rules ((metaRulesTbl:Hashtbl.t<string,Rule.t<Source.t,Source.t> >),refsTbl) res = 
    match rules with 
    | [] -> res
    | h::t ->
      if (isMetaRule h) then begin
        metaRulesTbl.Add(h.name,h)
      ; handleMeta t (metaRulesTbl,refsTbl) res
      end else 
        let (b, rules) = expandMeta (h:Rule.t<Source.t,Source.t>).body metaRulesTbl refsTbl [] in 
        let r = { h with Rule.body = b } 
        in handleMeta t (metaRulesTbl,refsTbl) (res @ rules @ [r])

(** main function 
 *  - create hash tables
 *  - call process grammar to expand metarules
 *)
let expandMetaRules rules =
    (** hash table for metarules *)
    let metaRulesTbl = Hashtbl.create 200 in
    (** hash table for references to expanded metarules *)
    let refsTbl = Hashtbl.create 200 in
    handleMeta rules (metaRulesTbl,refsTbl) []
