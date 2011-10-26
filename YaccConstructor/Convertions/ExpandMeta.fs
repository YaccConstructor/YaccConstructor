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
     try List.map2(fun x  y -> (getText x), y ) l1 l2 
     with :? System.ArgumentException -> invalidArgs  name
    
/// Return newParam if condition is true and there are no oldParams
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

/// create list of strings from list of productions
let createStrList = List.map (fun x -> getTextIL x)//x.ToString()) 

/// key for hash table
let getKey metaName metaArgs (args : Source.t option) = 
    (*String.concat "$#"
        <| *)(String.concat "^#" ((getText metaName)::(createStrList metaArgs)))
            (*::(match args with
                | Some x -> [Source.toString x]
                | None -> []
                )*)

/// <summary>
/// <para> Returns actual value of formal parameter. </para>
/// <para> formalToAct - list of (parameter name, its value), which is searched for 'formalName' </para>
/// </summary>
let getActualParam formalName formalToAct = List.tryPick (fun (x,y) -> if x = formalName then Some y else None) formalToAct

/// Create pair (formal argument name, actual argument name)
let addBindingPair args = function
    | None -> args
    | Some x -> (*printfn "%A" x; *)(createNewName ("arg", (0,0)), x)::args

/// <summary>
/// <para> Expand references to metarules </para>
/// <para> and generate new rules for every such reference </para>
/// <para> body: t - production which can contain metareferences </para>
/// <para> metaRulesTbl: Dictionary&lt;string,Rule.t&lt;Source.t,Source.t&gt;&gt; - table, which contains expanding metareferences rules </para>
/// <para> refsTbl: Dictionary&lt;string,Source.t&gt; - map of metareference with actual params to generated rule name </para>
/// <para> res: Rule.t list - generated rules from body </para>
/// <para> formalToAct - list of (metaArg name, its actual value) </para>
/// <para> args: list of heritable attributes in format (formal name, actual name) </para>
/// <para> returns (new body, generated rules) </para>
/// </summary>
let rec expandMeta body (metaRulesTbl:Dictionary<string,Rule.t<Source.t,Source.t> >)
         (refsTbl:Dictionary<string,Source.t>) res formalToAct args = 
    //printfn "b: %A" body

    /// <summary>
    /// <para> Replaces all metarule parameters in body with given args. </para>
    /// <para> Returns (new body, generated rules). </para>
    /// <para> formParams - formal L-attributes </para>
    /// <para> formalToAct - list of (metaArg name, it's value) </para>
    /// <para> nRules - accumulator - list of generated rules </para>
    /// <para> args - list of (formal (not-meta-)argument name, actual argument name). </para>
    /// <para> Last unnamed argument is an original rule body. </para>
    /// </summary>
    let rec transformBody formParams (formalToAct:(string * t<Source.t, Source.t>) list) nRules
            (args : (Source.t * Source.t) list) = 
        function
        | PRef (r, p) -> 
            let rName = getText r
            let param = getActualParam rName formalToAct
            let (|Terminal|NonTerminal|) (name:string) = 
                if name.[0] = name.ToLower().[0] then NonTerminal else Terminal

            match param with          
            | Some n -> 
                match n with
                | PRef(actR, actP) -> PRef (actR, actP), nRules
                | PToken(actR)     -> PToken (actR), nRules
                | x -> x, nRules
            | None   -> PRef (r, p), nRules
          
        | PAlt (l, r) -> 
            let (l', nRules') = transformBody formParams formalToAct nRules args l 
            let (r', nRules'') = transformBody formParams formalToAct nRules' args r
            PAlt (l', r'), nRules''
        | PSeq (seq, a) -> 
            let rec transformSeq formalToAct args' = function
                | h::t -> 
                    let (body', newRules) = transformBody formParams formalToAct [] args' h.rule
                    let (seq', newRules') = transformSeq formalToAct (addBindingPair args' h.binding) t 
                    ({ h with rule = body' }::seq', newRules @ newRules')
                | []   -> ([], [])
            let (l', nRules') = transformSeq formalToAct args seq 
            (PSeq (l', a), nRules @ nRules')
        | PToken t -> (PToken t, nRules)
        | PLiteral r -> (PLiteral r, nRules)

        | PMetaRef (name, p, mArgs) -> 

            /// Try to replace one entry of formal parameter with its actual value
            let replaceFormal formalToAct formal = 
                match getActualParam (getText formal) formalToAct with
                | Some y -> y
                | None   -> PRef(formal,None)

            /// <summary>
            /// <para> Recursively replace formal metaparameters with actual for all inner meta-arguments. </para>
            /// <para>    ('inner' means reference to meta-rule inside arguments of another meta-rule). </para>
            /// </summary>
            let rec replaceFormals mArgs formalToAct =
                let rec replaceFormalInProd = 
                    function
                    | PSeq(elements, ac) -> PSeq(elements |> List.map (fun elem -> {elem with rule = replaceFormalInProd elem.rule}), ac) 
                    | PAlt(l, r) -> PAlt(replaceFormalInProd l, replaceFormalInProd r) 
                    | PSome(x) -> PSome(replaceFormalInProd x)
                    | POpt(x) -> POpt(replaceFormalInProd x)
                    | PMany(x) -> PMany(replaceFormalInProd x)
                    | PRef(s,p) as x -> 
                        match getActualParam (getText s) formalToAct with
                        | Some y -> y
                        | None -> x
                    | PMetaRef(s,p,a) as x -> 
                        match getActualParam (getText s) formalToAct with
                        | Some y -> y
                        | None -> x
                    | x -> x

                List.map replaceFormalInProd mArgs
                        
            let name' = (*printPairList formalToAct ;*) 
                match replaceFormal formalToAct name with
                | PRef(s, None) -> s
                | x -> failwith <| "metaparam substitution " + (Source.toString name) + "->" + (getTextIL x) + " expected to be ref"

            /// Name of rule, what appears in result of replacing formal parameter with actual
            let mRuleName' = getText name'
            let mArgs' = replaceFormals mArgs formalToAct
            //printfn "ma  %A" mArgs
            //printfn "ma' %A" mArgs'
            let (b', nRules') = expandMetaRef nRules name' formParams mArgs' formalToAct args
            b', nRules'
        | POpt r -> 
            let (b', nRules') = transformBody formParams formalToAct nRules args r
            (POpt b', nRules')
        | PSome r -> 
            let (b', nRules') = transformBody formParams formalToAct nRules args r
            (PSome b', nRules')
        | PMany r -> 
            let (b', nRules') = transformBody formParams formalToAct nRules args r
            (PMany b', nRules')
        | PPerm _ | PRepet _ -> raise (new System.NotImplementedException())
    (*      | other -> reportError "EBNF construction has already been transformed" 
            ; (other, nRules)
    *)

    /// <summary>
    /// <para> Replace rule with new, eliminating original meta-rule. </para>
    /// <para> rName - new name of created rule; </para>
    /// <para> _params - L-attributes of new rule; </para>
    /// <para> mRulesTbl - table of declared meta-rules; </para>
    /// <para> metaName - name of eliminated meta-rule; </para>
    /// <para> metaArgs - list of actual parameters of metaRule; </para>
    /// <para> args - list of pairs (formal argument name, its actual value); </para>
    /// Returns list of generated rules.
    /// </summary>
    and genNewRule rName _params mRulesTbl metaName metaArgs args =
        /// <summary>
        /// <para> Replace rule with new, eliminating original meta-rule. </para>
        /// <para> rName - new name of created rule; </para>
        /// <para> formalParams - formal L-attributes of new rule; </para>
        /// <para> metaRule - name of eliminated meta-rule; </para>
        /// <para> metaArgs - list of actual parameters of metaRule; </para>
        /// <para> args - list of pairs (formal argument name, its actual value); </para>
        /// </summary>
        let transformRule rName formalParams (metaRule: Rule.t<Source.t,Source.t>)
                (metaArgs: t<Source.t, Source.t> list) args =
            let formalToAct = createPairsList metaRule.name metaRule.metaArgs metaArgs
            // probably equal to metaRule.args(L-attributes)
            //let fArgs = getFormalArgs metaRule.name _params metaRule.args
            //let args' = list2opt fArgs
            let formalAttrs = (metaRule.args @ formalParams)
            let formalAttrs' = formalAttrs |> createParams |> list2opt
            let (b, newRules) = transformBody formalAttrs' formalToAct [] args metaRule.body
            ( newRules @ [ createRule rName formalAttrs b metaRule._public [] ] )

        match findMetaRule mRulesTbl metaName with 
        | Some metaRule -> transformRule rName _params metaRule metaArgs args
        | None -> []
 
    /// <summary>
    /// <para> Creates new rule, expanding metaReference.
    ///   Returns (ref to new rule, list of generated rules) </para>
    /// <para> metaName - name of replaced metaReference; </para>
    /// <para> res - list of generated rules; </para>
    /// <para> _params - arguments of current meta_rule; </para>
    /// <para> metaArgs - actual parameters of metarule, like 'a' and 'b' in rule &lt;&lt; a b &gt;&gt; ; </para>
    /// formalToAct - list of (metaArg name, its actual value);
    /// </summary>
    and expandMetaRef res (metaName:Source.t) _params (metaArgs':t<Source.t, Source.t> list) formalToAct args =
        //printfn "r %A" metaName
        //printfn "m %A" metaArgs'
        let key = getKey metaName metaArgs' _params
        // checks if we already expanded rule with given name and meta args
        if refsTbl.ContainsKey(key) then
            (PRef (refsTbl.Item key, _params), res)
        else
            // Recursively expand rules (specified as arguments of meta-rule).
            let formalArgsList = List.map fst args |> createParams
            let actualArgsList = args |> List.map snd
            //printfn "%s %A" (fst metaName) formalArgsList
            let newMetaArgs, newRules =
                metaArgs' 
                |> List.fold
                    (fun (curMetaArgs, curRules) rule ->
                      let (body, (rules : Rule.t<Source.t, Source.t> list)) = expandMeta rule metaRulesTbl refsTbl curRules formalToAct args
                      let newRuleName = createNewName ("rule",(0,0))
                      let newRule = createSimpleRule (fst newRuleName) (actualArgsList) body
//                      printfn "%A" newRule
                      (curMetaArgs @ [PRef(newRuleName, formalArgsList |> list2opt)], newRule::rules))
                   ([], (res : Rule.t<Source.t, Source.t> list))
            //let key = getKey metaName newMetaArgs
            let rName = createNewName metaName
            //printfn "%A" newMetaArgs
            // New rules (one for replaced rule and some for recursive expanding of rule)
            let actualArgs = (addBinding actualArgsList _params) |> createParams |> list2opt
            let eRules = 
                refsTbl.Add(getKey metaName newMetaArgs _params,rName)
                refsTbl.Add(key,rName)
                //printfn "%s %A" (fst metaName) actualArgs
                genNewRule (getText rName) formalArgsList
                        metaRulesTbl (getText metaName) newMetaArgs args
            let b = PRef (rName, actualArgs) 
            (b, newRules @ eRules)

    in
        match body with
        | PMetaRef (metaName, ruleArgs, mArgs) ->
            if mArgs = [] then (PRef(metaName, ruleArgs), res)
            else expandMetaRef res metaName ruleArgs mArgs formalToAct args
        | PSeq (seq, a) -> 
            let rec expandSeq mRulesTbl rfsTbl res args = function
            | h::t ->
                let (b, res') = expandMeta h.rule mRulesTbl rfsTbl res formalToAct args
                let (b', res'') = expandSeq mRulesTbl rfsTbl res' (addBindingPair args h.binding) t
                ({ h with rule = b }::b', res'')
            | [] -> ([], res)
            let (eSeq, res') = expandSeq metaRulesTbl refsTbl res args seq
            (PSeq (eSeq, a), res')
        
        | PAlt (l, r) -> let (l', res') = expandMeta l metaRulesTbl refsTbl res formalToAct args
                         let (r', res'') = expandMeta r metaRulesTbl refsTbl res' formalToAct args
                         (PAlt(l', r'), res'')
        | POpt r  -> let (r', res') = expandMeta r metaRulesTbl refsTbl res formalToAct args
                     (POpt r', res')
        | PSome r -> let (r', res') = expandMeta r metaRulesTbl refsTbl res formalToAct args
                     (PSome r', res')
        | PMany r -> let (r', res') = expandMeta r metaRulesTbl refsTbl res formalToAct args
                     (PMany r', res')
        | other -> (other, res)


(** grammar processing:
 *  - collect metarules
 *  - expand metarules
 *  - call metarules expanding 
 *)

(** main function 
 *  - create hash tables
 *  - call process grammar to expand metarules
 *)
let expandMetaRules rules =
    /// collect a set of declared metaRules
    let rec collectMeta rules ((metaRulesTbl:Dictionary<string,Rule.t<Source.t,Source.t> >),refsTbl) = 
        match rules with 
        | [] -> ()
        | h::t -> 
            if (isMetaRule h) then 
    //            printfn "+%s" h.name
                metaRulesTbl.Add(h.name,h)        
            collectMeta t (metaRulesTbl,refsTbl)
    //        printfn "%s" h.name

    /// Replace existing meta-rules. Suppose that all high-level meta-rules are in metaRulesTbl
    let rec replaceMeta rules ((metaRulesTbl:Dictionary<string,Rule.t<Source.t,Source.t> >),refsTbl) res = 
        /// Bind all non-meta-arguments of rule
        let getRuleBindings (rule : Rule.t<Source.t,Source.t>) =
            let rec accBindings res = function
            | (h : Source.t)::t ->
                let splitted =
                    (fst h).Split([|' '; '\t'|])
                    |> Array.toList
                    |> List.filter (fun s -> s <> "")
                let curRes =
                    List.fold
                        (fun res x -> addBindingPair res (Some (x,(0,0))))
                        [] splitted
                accBindings (curRes @ res) t
            | [] -> res
            accBindings [] rule.args
        match rules with 
        | [] -> res
        | h::t -> 
            let res = 
                if (isMetaRule h) then 
                    replaceMeta t (metaRulesTbl, refsTbl) res      
                else 
    //                printfn "-%s" h.name
                    let (b, rules) = expandMeta (h:Rule.t<Source.t,Source.t>).body
                                        metaRulesTbl refsTbl [] [] (getRuleBindings h)
                    let r = { h with Rule.body = b }
                    replaceMeta t (metaRulesTbl,refsTbl) (res @ rules @ [r])
    //        printfn "%s" h.name
            res

    (** hash table for metarules *)
    let metaRulesTbl = new Dictionary<string,Rule.t<Source.t,Source.t> >(200)
    (** hash table for references to expanded metarules *)
    let refsTbl = new Dictionary<string,Source.t>(200)
    collectMeta rules (metaRulesTbl, refsTbl)
    replaceMeta rules (metaRulesTbl, refsTbl) []

type ExpandMeta() = 
    inherit Convertion()
        override this.Name = "ExpandMeta"
        override this.ConvertList ruleList = expandMetaRules ruleList
        override this.EliminatedProductionTypes = [""]



//let expandMetaRules = id