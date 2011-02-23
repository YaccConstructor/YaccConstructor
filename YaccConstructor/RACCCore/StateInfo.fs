// StateInfo.fs contains StateInfo type
//
//  Copyright 2009,2010 Semen Grigorev <rsdpisuy@gmail.com>
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

namespace Yard.Generators.RACCGenerator

open Yard.Core.CompareHelper

[<ReferenceEquality>]
type StateInfo<'position, 'tree, 'itemName, 'traceStep 
                when 'itemName : equality 
                and 'position : equality
                and 'tree : equality> =
    {
        position : 'position
        forest   : List<'tree>
        itemName : 'itemName    
        sTrace   : List<'traceStep>
    }
     member self.GetValue (x:StateInfo<_,_,_,_>) =
        x.position, x.itemName , (x.forest)     
     interface System.Collections.IStructuralComparable with
            member self.CompareTo (y,c) = -1
                

