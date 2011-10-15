//  Module ExpandMeta contains:
//  - function, which expands metarules 
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

module Yard.Core.Convertions.ExpandMeta 

open Yard.Core
open Yard.Core.IL
open Production
open Yard.Core.Namer
open TransformAux

type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>
(** if rule has metaArgs then it's metarule *)
let isMetaRule (r:Rule.t<'a,'b>) = r.metaArgs <> []

/// find metarule with given name in hash map of collected metarules
let findMetaRule (tbl:Dictionary<string,Rule.t<Source.t,Source.t>>) mName = 
    try Some (tbl.Item mName) with
    | :?System.Collections.Generic.KeyNotFoundException -> failwith "undeclared metarule "(*reportError ("undeclared metarule " ^ mName)*) ; None
    

/// reports error with arguments of metarule
let invalidArgs name = 
    failwith "invalid number of parameters in metarule "
    (*reportError ("invalid number of parameters in metarule " ^ name)*) ; []

/// create list of pairs: (<formal parameter>, <actual parameter>)
let createPairsList name l1 l2 =
     try List.map2(fun x  y -> getText x, y ) l1 l2 
     with :? System.ArgumentException -> invalidArgs  name
    


/// update actual parameters if condition is true and there are no oldParams
/// (this function is used when need pass parameters to EBNF metarules or 
/// their metaparameter - item)
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

(** create list of strings from list of productions *)
let createStrList = List.map (fun x -> getTextIL x) 

/// key for hash table
let getKey metaName metaArgs = 
    String.concat "" ((getText metaName)::(createStrList metaArgs))

(** returns actual parameter (if given list contains it with given formal) 
 *  (args is list of pairs (<formal>, <actual>) )
 *)

/// Returns actual value of formal parameter.
/// args - list of parameters name and their values, which is searched for formalName
let getActualParam formalName args = List.tryPick (fun (x,y) -> if x = formalName then Some y else None) args

/// expand references to metarules 
/// and generate new rules for every such reference
/// body:t - production which can contain metareferences
/// metaRulesTbl:Dictionary<string,Rule.t<Source.t,Source.t>> - table, which contains expanding metareferences rules 
/// refsTbl:Dictionary<string,Source.t> - map of metareference with actual params to generated rule name
/// res:Rule.t list - generated rules from body
/// returns (new body, generated rules)
let rec expandMeta body (metaRulesTbl:Dictionary<string,Rule.t<Source.t,Source.t> >) (refsTbl:Dictionary<string,Source.t>) res args = 
//    printfn "b: %A" body

    /// replaces all metarule parameters in body with given args
    let rec transformBody (ruleName:string) actParams (args:(string * t<Source.t, Source.t>) list) nRules = 
        function
        | PRef (r, p) -> 
            let rName = getText r
            (** pass actual parameters for item 
            *  (which is replaced with yard_item_...) 
            *)
            let aParams = updateActParams (isItem rName) actParams p
            let param = getActualParam rName args
            let (|Terminal|NonTerminal|) (name:string) = 
                if name.[0] = name.ToLower().[0] then NonTerminal else Terminal

          // test if param is replaced with actual param
            match param with          
            | Some n -> 
                match n with
                | PRef(actR, actP) -> PRef (actR, aParams), nRules
                | PToken(actR)     -> PToken (actR), nRules
                | x -> x, nRules
            | None   -> PRef (r, p), nRules
          
        | PAlt (l, r) -> 
            let (l', nRules') = transformBody ruleName actParams args nRules l 
            let (r', nRules'') = transformBody ruleName actParams args nRules' r
            PAlt (l', r'), nRules''
        | PSeq (seq, a) -> 
            let rec transformSeq args = function
                | h::t -> 
                    let (body', newRules) = 
                        transformBody ruleName actParams args [] h.rule 
                    let (seq', newRules') = transformSeq args t 
                    ({ h with rule = body' }::seq', newRules @ newRules')
                | []   -> ([], [])
            let (l', nRules') = transformSeq args seq 
            (PSeq (l', a), nRules @ nRules')
        | PToken t -> (PToken t, nRules)
        | PLiteral r -> (PLiteral r, nRules)

        | PMetaRef (n, p, mArgs) -> 
          (** we accept other metarule as parameter *)

            let replaceFormal args formal = 
                let act = getActualParam (getText formal) args
                match act with
                | Some x -> x
//                  match x with
//                  | PRef(s,None) -> s
//                  | x -> failwith <| "metaparam substitution "+(Source.toString formal)+"->"+(getTextIL x)+" expected to be ref"
                | None   -> PRef(formal,None)
        (** replace formal metaparameters with actual for inner meta param*)
            let rec replaceFormals mArgs args =
                let rec replaceFormalInProd = 
                    function
                    | PSeq(elements, ac) -> PSeq(elements |> List.map (fun elem -> {elem with rule = replaceFormalInProd elem.rule}), ac) 
                    | PAlt(l, r) -> PAlt(replaceFormalInProd l, replaceFormalInProd r) 
                    | PSome(x) -> PSome(replaceFormalInProd x)
                    | POpt(x) -> POpt(replaceFormalInProd x)
                    | PMany(x) -> PMany(replaceFormalInProd x)
                    | PRef(s,p) -> replaceFormal args s 
                    | x -> x

                List.map replaceFormalInProd mArgs
                        
            let n' = (*printPairList args ;*) 
                match replaceFormal args n with
                | PRef(s, None) -> s
                | x -> failwith <| "metaparam substitution "+(Source.toString n)+"->"+(getTextIL x)+" expected to be ref"
            let mRuleName' = getText n'
            let p' = updateActParams (isEBNFmeta mRuleName') actParams p
            let mArgs' = replaceFormals mArgs args
            let (b', nRules') = expandMetaRef nRules n' p' mArgs' args
            b', nRules'     
        | POpt r -> 
            let (b', nRules') = transformBody ruleName actParams args nRules r
            (POpt b', nRules')
        | PSome r -> 
            let (b', nRules') = transformBody ruleName actParams args nRules r
            (PSome b', nRules')
        | PMany r -> 
            let (b', nRules') = transformBody ruleName actParams args nRules r
            (PMany b', nRules')
        | PPerm _ | PRepet _ -> raise (new System.NotImplementedException())
    (*      | other -> reportError "EBNF construction has already be transformed" 
            ; (other, nRules)
    *)
    /// transforms rule with
    and transformRule rName _params (metaRule: Rule.t<Source.t,Source.t>) (metaArgs:t<Source.t, Source.t> list) =
        let args = createPairsList metaRule.name metaRule.metaArgs metaArgs
        // probably equal to metaRule.args(L-attributes)
        let fArgs = getFormalArgs metaRule.name _params metaRule.args
        let args' = list2opt fArgs
        let (b, newRules) = transformBody rName args' args [] metaRule.body 
        ( newRules @ [ createRule rName fArgs b metaRule._public [] ] )

    /// creates new rule with chosen name 
    and genNewRule rName args' mRulesTbl metaName' metaArgs =
        let res = findMetaRule mRulesTbl metaName'
        match res with 
        | Some metaRule -> transformRule rName args' metaRule metaArgs
        | None -> []
 
    /// returns (ref to new rule, list of generated rules)
    and expandMetaRef res (metaName:Source.t) _params (metaArgs':t<Source.t, Source.t> list) args =
//        printfn "r %A" metaName
        let key = getKey metaName metaArgs'
        // checks if we already expanded rule with given name and meta args
        if refsTbl.ContainsKey(key) then
            (PRef (refsTbl.Item key, _params), res)
        else
            let newMetaArgs, newRules =
                metaArgs' 
                |> List.fold
                    (fun (curArgs, curRules) rule ->
                      let (body, rules) = expandMeta rule metaRulesTbl refsTbl curRules args
                      (curArgs @ [body], rules))
                    ([],res)
            let rName = createNewName metaName
            let eRules = 
                refsTbl.Add(key,rName)
                genNewRule (getText rName) _params 
                        metaRulesTbl (getText metaName) newMetaArgs
            let b = PRef (rName, _params) 
            (b, newRules @ eRules)

    in
        match body with
        | PMetaRef (mn, p, mArgs) ->
            if mArgs = [] then (PRef(mn, p), res)
            else expandMetaRef res mn p mArgs args
        | PSeq (seq, a) -> 
            let rec expandSeq mRulesTbl rfsTbl rs = function
            | h::t ->                         
                let (b, res') = expandMeta h.rule mRulesTbl rfsTbl rs args
                let (b', res'') = expandSeq mRulesTbl rfsTbl res' t            
                ({ h with rule = b }::b', res'')
            | [] -> ([], rs)
            let (eSeq, res') = expandSeq metaRulesTbl refsTbl res seq      
            (PSeq (eSeq, a), res')
        
        | PAlt (l, r) -> let (l', res') = expandMeta l metaRulesTbl refsTbl res args
                         let (r', res'') = expandMeta r metaRulesTbl refsTbl res' args
                         (PAlt(l', r'), res'')
        | POpt r  -> let (r', res') = expandMeta r metaRulesTbl refsTbl res args
                     (POpt r', res')
        | PSome r -> let (r', res') = expandMeta r metaRulesTbl refsTbl res args
                     (PSome r', res')
        | PMany r -> let (r', res') = expandMeta r metaRulesTbl refsTbl res args
                     (PMany r', res')
        | other -> (other, res)

(** grammar processing:
 *  - collect metarules
 *  - expand metarules
 *  - call metarules expanding 
 *)
let rec collectMeta rules ((metaRulesTbl:Dictionary<string,Rule.t<Source.t,Source.t> >),refsTbl) = 
    match rules with 
    | [] -> ()
    | h::t -> 
        if (isMetaRule h) then 
//            printfn "+%s" h.name
            metaRulesTbl.Add(h.name,h)        
        collectMeta t (metaRulesTbl,refsTbl)
//        printfn "%s" h.name

let rec replaceMeta rules ((metaRulesTbl:Dictionary<string,Rule.t<Source.t,Source.t> >),refsTbl) res = 
    match rules with 
    | [] -> res
    | h::t -> 
        let res = 
            if (isMetaRule h) then 
                replaceMeta t (metaRulesTbl,refsTbl) res      
            else 
//                printfn "-%s" h.name
                let (b, rules) = expandMeta (h:Rule.t<Source.t,Source.t>).body metaRulesTbl refsTbl [] []
                let r = { h with Rule.body = b }
                replaceMeta t (metaRulesTbl,refsTbl) (res @ rules @ [r])
//        printfn "%s" h.name
        res

(** main function 
 *  - create hash tables
 *  - call process grammar to expand metarules
 *)
let expandMetaRules rules =
    (** hash table for metarules *)
    let metaRulesTbl = new Dictionary<string,Rule.t<Source.t,Source.t> >(200)
    (** hash table for references to expanded metarules *)
    let refsTbl = new Dictionary<string,Source.t>(200)
    collectMeta rules (metaRulesTbl,refsTbl)
    replaceMeta rules (metaRulesTbl,refsTbl) []

type ExpandMeta() = 
    inherit Convertion()
        override this.Name = "ExpandMeta"
        override this.ConvertList ruleList = expandMetaRules ruleList
        override this.EliminatedProductionTypes = [""]



//let expandMetaRules = id