//  CompareHelper.fs 
//  contains helper functions for CustomEquality and CustomComparison realisation
//
//  Copyright 2009,2010,2011 Semen Grigorev <rsdpisuy@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

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