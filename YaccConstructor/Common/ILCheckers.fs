module Checker

let IsStartRuleExists (def:Yard.Core.IL.Definition.t<_,_>) =
    def.grammar |> List.exists (fun r -> r._public) 