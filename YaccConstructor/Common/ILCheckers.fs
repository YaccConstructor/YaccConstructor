module Checker

open System.Linq
open Yard.Core.IL.Production

let IsStartRuleExists (def:Yard.Core.IL.Definition.t<_,_>) =
    def.grammar |> List.exists (fun r -> r._public) 

let IsChomskyNormalForm (def:Yard.Core.IL.Definition.t<_,_>) =
    def.grammar.All 
        (fun r -> 
            match r.body with 
            | PSeq([{rule = PToken(_);omit =_ ;binding =_;checker = _}],_)
            | PSeq([{rule = PRef(_);omit =_ ;binding =_;checker = _}
                   ;{rule = PRef(_);omit =_ ;binding =_;checker = _}],_) -> true 
            | _ -> false)

(*let Unused (def:Yard.Core.IL.Definition.t<_,_>) : bool =    
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