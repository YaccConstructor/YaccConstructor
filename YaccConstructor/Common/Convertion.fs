namespace Yard.Core

open Yard.Core.IL

[<AbstractClass>]
type Convertion() =
    abstract Name : string
    abstract ConvertList : Rule.t<Source.t, Source.t> list -> Rule.t<Source.t, Source.t> list
    abstract ConvertList : Rule.t<Source.t, Source.t> list * string -> Rule.t<Source.t, Source.t> list
    default this.ConvertList(ruleList, string) = this.ConvertList(ruleList)
    abstract EliminatedProductionTypes : string list