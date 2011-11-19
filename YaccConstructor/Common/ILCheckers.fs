module Yard.Core.Checkers

open Yard.Core.IL.Production
open System.Collections.Generic
open System.Linq

let private startRulesCount (def:Yard.Core.IL.Definition.t<_,_>) =
    def.grammar.Count(fun r -> r._public)

let IsStartRuleExists (def:Yard.Core.IL.Definition.t<_,_>) =
    startRulesCount def > 0

let IsSingleStartRule (def:Yard.Core.IL.Definition.t<_,_>) =
    startRulesCount def = 1

let IsChomskyNormalForm (def:Yard.Core.IL.Definition.t<_,_>) =
    def.grammar.All 
        (fun r ->
            match r.body with
            | PSeq([{rule = PToken(_);omit =_ ;binding =_;checker = _}],_)
            | PSeq([{rule = PRef(_);omit =_ ;binding =_;checker = _}
                   ;{rule = PRef(_);omit =_ ;binding =_;checker = _}],_) -> true 
            | _ -> false)

let GetUndeclaredNonterminalsList(def:Yard.Core.IL.Definition.t<_,_>) =
    let declaredRules = def.grammar |> List.map (fun r -> r.name) 
    let undeclaredRules = new HashSet<string>()
    let addUndeclaredRule name additionRules = 
        let name = (fst name).ToString()
        if (List.tryFind ( (=) name) (declaredRules) ).IsNone
        && (Seq.tryFind ( (=) name) (additionRules) ).IsNone then
            undeclaredRules.Add( name ) |> ignore

    let rec getUndeclaredRules (additionRules, body) =
        let getUndeclaredRulesCurried body = getUndeclaredRules (additionRules, body)
        match body with
            |PRef (name,_) -> addUndeclaredRule name additionRules
            |PMetaRef (name,_,exprList) ->  addUndeclaredRule name additionRules
                                            exprList |> List.iter (fun r -> getUndeclaredRulesCurried r )
            |PSeq (exprList,_) -> exprList |> List.iter (fun r -> getUndeclaredRulesCurried r.rule )
            |PPerm (exprList) -> exprList |> List.iter (fun r -> getUndeclaredRulesCurried r )
            |PRepet (expr,_,_)
            |PMany(expr)
            |PSome (expr)
            |POpt (expr) -> getUndeclaredRulesCurried expr
            |PAlt (lExpr,rExpr) -> getUndeclaredRulesCurried lExpr
                                   getUndeclaredRulesCurried rExpr
            | _ -> ()

    def.grammar |> List.iter (fun r -> 
                                let additionRules = new HashSet<string>()
                                r.metaArgs |> List.iter (fun i -> additionRules.Add( (fst i).ToString() ) |> ignore )
                                getUndeclaredRules (additionRules, r.body)
                                )

    List.ofSeq undeclaredRules

(*
let IsUndeclaredNonterminalsExists (def:Yard.Core.IL.Definition.t<_,_>) =
    let declaredRules = def.grammar |> List.map (fun r -> r.name) 
    let searchedRules = new HashSet<string>()
    let rec getUndeclaredRules (body:Yard.Core.IL.Production.t<_,_>) =
        match body with
            |PRef (name,_)
            |PMetaRef (name,_,_) -> let name = (fst name).ToString()
                                    if isNotViewedRule name then
                                        searchedRules.Add( name ) |> ignore
            |PSeq (exprList,_) -> exprList |> List.iter (fun r -> getUndeclaredRules r.rule )
            |PPerm (exprList) -> exprList |> List.iter (fun r -> getUndeclaredRules r )
            |PRepet (expr,_,_)
            |PMany(expr)
            |PSome (expr)
            |POpt (expr) -> getUndeclaredRules expr
            |PAlt (lExpr,rExpr) -> getUndeclaredRules lExpr
                                   getUndeclaredRules rExpr
            | _ -> ()
    and isNotViewedRule name = 
        (Seq.tryFind ( (=) name) searchedRules).IsSome

    def.grammar |> List.iter (fun r ->  getUndeclaredRules r.body )
    searchedRules


let Unused (def:Yard.Core.IL.Definition.t<_,_>) : bool =    
    let unusedLst (def:Yard.Core.IL.Definition.t<_,_>) (*: List<Rule.t>*) =
        let rulesNameList = 
            List.map (fun r -> r.name) def.grammar 
            |> Set.ofSeq
        let startRulesList = List.filter (fun r -> r._public) def.grammar
        let rec getReachableRules (startRule:Yard.Core.IL.Rule.t<_,_>) = 
            let rec inner (body:Yard.Core.IL.Production.t<_,_>) =
                match body with
                |PAlt  (lAlt,rAlt) -> inner lAlt @ inner rAlt
                |PSeq      (elem<'patt,'expr>) list * 'expr option
                |PRef     of Source.t * 'expr option
                |PMany    of (t<'patt,'expr>)
                |PMetaRef of Source.t * 'expr option * t<'patt,'expr> list
                |PRepet   of (t<'patt,'expr>) * int option * int option
                |PPerm    of (t<'patt,'expr>) list
                |PSome    of (t<'patt,'expr>)
                |POpt     of (t<'patt,'expr>) 
            inner startRule.body
            |> List.map (fun name -> (List.filter (fun r -> r.name = name) def.grammar))
            |> List.map getReachableRules
        List.map getReachableRules startRulesList
        |> Set.ofSeq
        |> Set.difference rulesNameList
    unusedLst def > 0     



    //unusedLst.Length > *)