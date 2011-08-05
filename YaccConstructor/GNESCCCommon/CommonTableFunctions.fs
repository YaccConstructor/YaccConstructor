//  CommonTableFunctions.fs 
//  contains common Generator and Interpreter table functions.
//
//  Copyright 2011 Semen Grigorev <rsdpisuy@gmail.com>
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

module Yard.Generators.GNESCCGenerator.CommonTableFunctions

open Yard.Generators.GNESCCGenerator.CommonTypes

/// Represent (ProductionIndex,ProdictionDotIndex) as one integer 
let mkItem0 (prodIdx,dotIdx) : Item0 = (uint32 prodIdx <<< 16) ||| uint32 dotIdx
let prodIdx_of_item0 (item0:Item0) = int32 (item0 >>> 16)
let dotIdx_of_item0 (item0:Item0) = int32 (item0 &&& 0xFFFFu)

