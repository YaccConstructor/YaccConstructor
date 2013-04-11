//  Contains more clear aliases for all constraints.
//  It's recommended to add all newly described constraints here.
//
//  Copyright 2013 Avdyukhin Dmitry<dimonbv@gmail.com>
//
//  This file is part of YaccConctructor.
//  This is war
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

module Yard.Core.Constraints

open Yard.Core.ConstraintsImpl

let singleModule = SingleModule.singleModule
let noEbnf = NoEbnf.noEbnf
let noMeta = NoMeta.noMeta
let inCNF = InCNF.inCNF
let needAC = NeedAC.needAC
let noInnerAlt = NoInnerAlt.noInnerAlt
let noBrackets = NoBrackets.noBrackets
let noLiterals = NoLiterals.noLiterals
