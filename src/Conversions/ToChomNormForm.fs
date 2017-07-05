module Yard.Core.Conversions.ToChomNormForm

open System.IO
open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Core.IL.Rule


let deleteLongRules (rules: Rule.t<'a,'b> list) =
    let rulesNew = ref []
    for rule in rules do
        let elements = match rule.body with PSeq(e, a, l) -> e 
        if elements.Length < 3 then 
            rulesNew := !rulesNew @ [rule]
        else 
            let names = [|for i in 0..elements.Length-3 -> Namer.newSource(rule.name)|]  
            rulesNew := !rulesNew @ [TransformAux.createRule rule.name rule.args (PSeq([elements.[0]; TransformAux.createDefaultElem (PRef(names.[0], None))], None, None)) false rule.metaArgs              ] 
            for i in 1..elements.Length-3 do
                rulesNew := !rulesNew @ [ TransformAux.createRule names.[i-1] rule.args (PSeq([elements.[i]; TransformAux.createDefaultElem (PRef(names.[i], None))], None, None)) false rule.metaArgs] 
            rulesNew := !rulesNew @ [TransformAux.createRule names.[elements.Length-3] rule.args (PSeq([elements.[elements.Length-2]; elements.[elements.Length-1]], None, None)) false rule.metaArgs            ] 
    !rulesNew


let toChomNormForm (ruleList: Rule.t<_, _> list) = 
    ruleList
    |> deleteLongRules

type ToChomNormForm() = 
    inherit Conversion()
        override this.Name = "ToChomNormForm"
        override this.ConvertGrammar (grammar,_) = mapGrammar toChomNormForm grammar



                
        
    

