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

module Yard.Core.Conversions.ExpandMeta 

open Yard.Core
open Yard.Core.IL
open Production
open Yard.Core.Namer
open TransformAux

open System.Collections.Generic

/// find metarule with given name in hash map of collected metarules
let findMetaRule (tbl : IDictionary<string,Dictionary<string,string * Rule.t<Source.t,Source.t>>>) module' metaName =
    try tbl.[module'].[metaName] with
    | :?System.Collections.Generic.KeyNotFoundException ->
        failwith <| sprintf "unable to find metarule %s in module %s" metaName module'

let getKey module' key = 
    let rec getKey (this : t<Source.t, Source.t>) =
        let argsToString = function
            | None -> ""
            | Some x -> "[" + x.ToString() + "]"
                    
        let metaArgsToString metaArgs =
            if ((metaArgs : list<_>).IsEmpty) then ""
            else "<<" + (metaArgs
                            |> List.map getKey
                            |> String.concat " ")
                    + ">>"
                    
        match this with
        |PAlt (x, y) -> getKey x + "|" + getKey y
        |PSeq (ruleSeq, attrs, l) ->
            let strAttrs =
                match attrs with
                | None -> ""
                | Some x -> "{" + x.text + "}"

            let elemToString (x : elem<Source.t, Source.t>) =
                let check =
                    match x.checker with
                    | None -> ""
                    | Some c -> "=>{" + c.ToString() + "}=>"
                let omit = if x.omit then "-" else ""
                let bind =
                    match x.binding with
                    | None -> ""
                    | Some var -> var.text + "="
                check + omit + bind + getKey x.rule
            "<" + String.concat " " (List.map (fun x -> "(" + elemToString x + ")") ruleSeq) + ">" + strAttrs
        |PToken src -> src.text
        |PRef (name, args) ->
            name.text + argsToString args
        |PMany x -> "(" + getKey x + ")*"
        |PMetaRef (name, args, metaArgs) ->
            name.text + metaArgsToString metaArgs + argsToString args
        |PLiteral src -> src.text
        |PRepet _ -> failwith "Repetition was not realized yet"
        |PPerm src -> "[|" + (src
                                |> List.map getKey
                                |> String.concat ";")
                            + "|]"
        |PSome x -> "(" + getKey x + ")+"
        |POpt x -> "(" + getKey x + ")?"
    module' + ":" + getKey key

(*
/// Create pair (formal argument name, actual argument name)
let addBindingPair attrs (*binding*) = function
    | None -> attrs
    | Some b -> if List.exists (fun x -> snd x = b) attrs  then attrs
                else (createNewName ("arg", new Source.Position(), new Source.Position(), ""), b)::attrs

let getFormals, getActuals = fst, snd

/// Bind all heritable attributes of rule
let getRuleBindings (rule : Rule.t<Source.t,Source.t>) init =
    let rec accBindings res = function
    | (h : Source.t)::t ->
        let splitted =
            h.text.Split([|' '; '\t'; '\r'; '\n'|])
            |> Array.toList
            |> List.filter (fun s -> s <> "")
        let curRes =
            List.fold
                (fun res x -> addBindingPair res (Some (x,(0,0,""))))
                [] splitted
        accBindings (curRes @ res) t
    | [] -> res
    accBindings init rule.args
*)
/// <summary>
/// <para> Replace rule with new one, replacing references to metarules, </para>
/// <para> and generate new rules for every such reference </para>
/// <para> body: t - production which can contain metareferences </para>
/// <para> metaRules: TO_DO Dictionary&lt;string,Rule.t&lt;Source.t,Source.t&gt;&gt; - table, which contains expanding metareferences rules </para>
/// <para> expanded: Dictionary&lt;string,Production.t&gt; - map of metareference with actual params to generated rule name </para>
/// <para> res: Rule.t list - currently generated rules </para>
/// <para> returns (new body, generated rules + old rules) </para>
/// </summary>
let expandRule body (module' : string) metaRules expanded =
    /// Replace formal parameter with its actual value or (if it is not to be replaced) 
    let tryReplaceActual (formalToAct : (string * Production.t<_,_>) list) formal prev = 
        match List.tryFind (fun x -> fst x = formal) formalToAct with
        | None -> prev
        | Some res -> snd res

    /// <summary>
    /// <para> Replace rule body, expanding references to metarules, </para>
    /// <para> and generate new rules for every such reference </para>
    /// <para> body: t - production which can contain metareferences </para>
    /// <para> metaRules: TO_DO Dictionary&lt;string,Rule.t&lt;Source.t,Source.t&gt;&gt; - table, which contains expanding metareferences rules </para>
    /// <para> expanded: Dictionary&lt;string,Production.t&gt; - map of metareference with actual params to generated rule name </para>
    /// <para> res: Rule.t list - currently generated rules </para>
    /// <para> returns (new body, generated rules + old rules) </para>
    /// </summary>
    let rec expandBody body (module' : string) metaRules (expanded : Dictionary<_, Production.t<_,_>>) res =
        //printfn "b: %A" body
        /// Returns key for table of expanded rules.
        /// It's better to use hash.
        let key = getKey module' body
        if expanded.ContainsKey key then 
            // printfn "ok %A\n: \t%A\n\n:\t%A\n=========================\n" body (expanded.Item key) res
            expanded.[key], res
        else
            let rule, res =
                /// Expand rule with the previous parameters
                let simpleExpand body = expandBody body module' metaRules expanded res

                /// Applies function to the first element of result
                let applyToRes f (a,b) = (f a, b)

                let rec canUseBinding = function
                    | PSeq(_,Some _,_) -> true
                    | PSeq(s,_,_) -> s |> List.exists (fun elem -> canUseBinding elem.rule)
                    | PRef(_,None)
                    | PToken _
                    | PLiteral _ -> false
                    | PAlt(l,r) -> canUseBinding l || canUseBinding r
                    | POpt r
                    | PSome r
                    | PMany r -> canUseBinding r
                    | PMetaRef (_,_,_) -> failwith "Metaref must already be expanded"
                    | x -> true

                let expandMetaRef (name : Source.t) attrs metaArgs =
                    let (newMetaArgs, newRes) =
                        metaArgs
                        |> List.fold
                            (fun (accMeta, accRes) _body ->
                                //printfn "%A" body
                                expandBody _body module' metaRules expanded accRes
                                |> (fun (newBody, newAccRes) ->
                                    if not <| canUseBinding newBody then
                                        (newBody::accMeta, newAccRes)
                                    else
                                        let newMetaArgName = genNewSource (newName "rule") _body
                                        let newMetaArg = PRef(newMetaArgName, None)
                                        let (newRule: Rule.t<_,_>) = Rule.defaultRule newMetaArgName newBody
                                        (newMetaArg::accMeta, newRule::newAccRes)
                                    )
                            )
                            ([], res)
                        |> applyToRes (List.rev)
                    // TODO catch exception
                    let declModule, metaRule = findMetaRule metaRules module' name.text
                    let newRuleName = new Source.t(newName ("rule_" + name.text), metaRule.name)
                    let formalArgs = metaRule.args
                    let substitution = PRef(new Source.t(newRuleName.text, name), attrs)
                    let newKey = getKey module' <| PMetaRef(name, attrs, newMetaArgs)
                    if not <| expanded.ContainsKey key then
                        expanded.Add(key, substitution)
                    if not <| expanded.ContainsKey newKey then
                        expanded.Add(newKey, substitution)
                    let newFormalToAct =
                        let expected = metaRule.metaArgs.Length
                        let actual = newMetaArgs.Length
                        if expected = actual then
                            List.zip (metaRule.metaArgs |> List.map Source.toString) newMetaArgs
                        else 
                            let pos (p:Source.Position) = string p.line + ":"  + string p.column
                            failwithf "%s - %s. Incorrect number of args. Metarule \"%s\" expected %i parameter(s) but get %i." 
                                (pos name.startPos) (pos name.endPos) metaRule.name.text expected actual
                    // TODO There can be a bug
                    //let newGlobalAttrs = getRuleBindings metaRule globalAttrs
                
                    let metaExp = expandBody (replaceMetasInBody newFormalToAct metaRule.body) declModule metaRules expanded newRes
                    let newRule = {Rule.defaultRule newRuleName (fst metaExp) with args = formalArgs}
                    (substitution, newRule::snd metaExp)

                match body with
                | PSome body -> applyToRes PSome <| simpleExpand body
                | POpt body  -> applyToRes POpt  <| simpleExpand body
                | PMany body -> applyToRes PMany <| simpleExpand body
                | PRef(name, attrs) as x -> (x, res)
                | PAlt (l, r) ->
                    let x,y = expandBody l module' metaRules expanded [], simpleExpand r
                    (PAlt (fst x, fst y), snd x @ snd y)
                | PSeq (ruleList, actionCode, l) ->
                    ruleList
                    |> List.fold (fun (curSeq, accRes) elem' ->
                        let bodyExp = expandBody elem'.rule module' metaRules expanded accRes
                        bodyExp |> applyToRes (fun h -> {elem' with rule = h}::curSeq)
                    ) ([], res)
                    |> applyToRes (fun x -> PSeq (List.rev x, actionCode, l))
                | PLiteral _ as literal -> (literal, res)
                | PToken _ as token -> (token, res)
                | PMetaRef (name, attrs, metaArgs) as x -> 
                    if metaArgs.IsEmpty then
                        (PRef(name, attrs), res)
                    else expandMetaRef name attrs metaArgs 
                | PPerm _ -> failwith "Unrealised meta-expanding of permutation"
                | PRepet _ -> failwith "Unrealised meta-expanding of repetition"
            if not <| expanded.ContainsKey key then
                expanded.Add(key, rule)
//            printfn "%A\n: \t%A\n\n:\t%A\n=========================\n" body rule res
            (rule, res)

    /// <para> Replace all rules to be replaced (meta-arguments substitution) in production. </para>
    /// <para> formalToAct - list of (metaArg name, its actual value) </para>
    /// <para> body - production to replace.  </para>
    and replaceMetasInBody formalToAct body = 
        let replace = replaceMetasInBody formalToAct
        match body with
        | PSome body -> PSome <| replace body
        | POpt body  -> POpt  <| replace body
        | PMany body -> PMany <| replace body
        | PRef(name, attrs) as prev ->
            prev |> tryReplaceActual formalToAct (Source.toString name)
        | PAlt (l, r) -> PAlt(replace l, replace r)
        | PSeq (ruleList, actionCode, l) ->
            PSeq (ruleList |> List.map (fun x ->  {x with rule = replace x.rule})
                    , actionCode, l)
        | PLiteral _ as literal -> literal
        | PToken _ as token -> token
        | PMetaRef (name, attrs, metaArgs) -> 
            if metaArgs.IsEmpty then
                PRef(name, attrs) |> tryReplaceActual formalToAct (Source.toString name)
            else PMetaRef(name, attrs, List.map replace metaArgs)
        | PPerm _ -> failwith "Unrealised meta-expanding of permutation"
        | PRepet _ -> failwith "Unrealised meta-expanding of repetition"

    expandBody body module' metaRules expanded []

(** grammar processing:
 *  - collect metarules
 *  - expand metarules
 *  - call metarules expanding 
 *)

(** main function 
 *  - create hash tables
 *  - call process grammar to expand metarules
 *)
let expandMetaRules grammar =
    /// if rule has metaArgs then it is a metarule
    let isMetaRule (r:Rule.t<Source.t,Source.t>) = r.metaArgs <> []

    /// Map: using_module -> (rule_name -> (decl_module, rule_body));
    let collectMeta grammar = 
        let rulesMap = getRulesMap grammar
        let publicRules = new Dictionary<_,_>(getPublicRules grammar)
        /// Only public meta-rules present here
        let publicMeta =
            let map = new Dictionary<string,Rule.t<Source.t, Source.t> list>()
            publicRules |> Seq.iter (fun item ->
                map.[item.Key] <- List.filter isMetaRule item.Value
            )
            map

        grammar
        |> List.map (fun m ->
            let res = new Dictionary<_,_>()
            m.openings
            |> List.iter (fun op ->
                publicMeta.[op.text]
                |> List.iter (fun rule ->
                    res.[rule.name.text] <- (op.text, rule)
                )
            )
            m.rules
            |> List.filter isMetaRule
            |> List.iter (fun rule ->
                res.[rule.name.text] <- (getModuleName m, rule)
            )
            getModuleName m, res
        )
        |> dict
    
    /// Replace existing meta-rules. Suppose that all high-level meta-rules are in metaRulesTbl
    let rec replaceMetasInBody (grammar : Grammar.t<_,_>) metaRulesTbl refsTbl = 
        grammar
        |> List.map (fun m ->
            m.rules
            |> List.collect (fun rule ->
                if isMetaRule rule then []
                else 
                    let b, rules = expandRule rule.body (getModuleName m) metaRulesTbl refsTbl
                    let r = { rule with Rule.body = b }
                    r::rules
            )
            |> (fun rules -> {m with rules = rules})
        )

    /// hash table for metarules
    let metaRulesTbl = collectMeta grammar
    /// hash table for references to expanded metarules
    let refsTbl = new Dictionary<_, Production.t<_,_> >(200)
    
    replaceMetasInBody grammar metaRulesTbl refsTbl

type ExpandMeta() = 
    inherit Conversion()
        override this.Name = "ExpandMeta"
        override this.ConvertGrammar (grammar,_) = expandMetaRules grammar

