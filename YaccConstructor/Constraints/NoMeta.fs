//  Describe constraint - grammar cannot contain meta-rules.
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

module Yard.Core.ConstraintsImpl.NoMeta

open Yard.Core
open IL.Production
open Yard.Core.ConstraintsImpl.Common

let private isMeta = function
    | PMetaRef _ -> true
    | _ -> false

let private checker grammar = not <| existsProd isMeta grammar
    
let noMeta = new Constraint("NoMeta", checker, Conversions.ExpandMeta.ExpandMeta())