namespace Yard.Core

open Yard.Core.IL

type IConvertion = interface
    abstract Name : string
    abstract ConvertList : Rule.t<Source.t, Source.t> list -> Rule.t<Source.t, Source.t> list
    abstract EliminatedProductionTypes : string list
end