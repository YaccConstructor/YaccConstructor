module Checker

let checkStartRule (def:Yard.Core.IL.Definition.t<_,_>) =
    let StartRuleList = def.grammar |> List.filter (fun r -> r._public=true) 
    StartRuleList.Length=1