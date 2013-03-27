//  Describe constraint - each rule in grammar must have action code.
//
//  Copyright 2013 Avdyukhin Dmitry<dimonbv@gmail.com>
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

module Yard.Core.ConstraintsImpl.NeedAC

open Yard.Core
open IL
open Production
open Yard.Core.ConstraintsImpl.Common

let private checker =
    existsProd (function
        | PSeq (_, None, _) -> true
        | _ -> false
    ) >> not
    
let needAC = new Constraint("NeedAC", checker, Conversions.AddDefaultAC.AddDefaultAC())