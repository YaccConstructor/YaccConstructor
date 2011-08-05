//  Layouts.fs contains functions for different datatypes layout.
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

module Yard.Core.Layouts

open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

let LayoutTable table layoutItem =
    table
    |> Seq.map
        (Seq.map 
            (layoutItem >> wordL)
            >> Seq.toList 
            >> semiListL
            >> fun l -> [ wordL "[|"; l; wordL "|];"] |> spaceListL )
    |> Seq.toList
    |> aboveListL
    |> fun l -> [ wordL "[|"; l; wordL "|]"] 
    |> spaceListL

let LayoutArray layoutItem = 
    Seq.map
            layoutItem
            >> Seq.toList 
            >> semiListL
            >> fun l -> [ wordL "[|"; l; wordL "|];"] |> spaceListL