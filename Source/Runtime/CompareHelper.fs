module Yard.Core.CompareHelper

let inline equalsOn f x (yobj:obj) =
    match yobj with
    | :? 'T as y -> (f x = f y)
    | _ -> false

let inline hashOn f x =  hash (f x)

let inline compareOn f x (yobj: obj) =
    match yobj with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "yobj" "cannot compare values of different types"