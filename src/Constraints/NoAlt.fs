
module Yard.Core.ConstraintsImpl.NoAlt

open Yard.Core
open IL
open Production

let private checker grammar =
    let rec inner = function
        | PAlt (_,_) -> true
        | _ -> false
    false

let noAlt = new Constraint("NoAlt", checker, Conversions.ExpandTopLevelAlt.ExpandTopLevelAlt())