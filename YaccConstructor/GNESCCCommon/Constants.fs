// Constants.fs contains general GNESCC constants
//
//  Copyright 2010 Semen Grigorev <rsdpisuy@gmail.com>
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

namespace  Yard.Generators.GNESCCGenerator

module Constants =
    let gnesccStartRuleName = "gnesccStart"
    let gnesccStartRuleTag  = 2
    let gnesccEndStreamTag  = 1
    let gnesccDummyLookaheadTag = 0
    let gnesccSymbolEnumeratorFrom = 3

    exception CheckerFalse

