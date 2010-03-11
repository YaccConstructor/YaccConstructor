// CompareHelper.fs 
// contains helper functions for CustomEquality and CustomComparison realisation
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module Yard.Core.CompareHelper

let inline equalsOn f x (mObj:obj) =
    match mObj with
    | :? 'T as y -> (f x = f y)
    | _ -> false

let inline hashOn f x =  hash (f x)

let inline compareOn f x (mObj: obj) =
    match mObj with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "mObj" "Cannot compare values of different types"