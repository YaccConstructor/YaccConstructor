//  Copyright 2011 by Konstantin Ulitin
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
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


namespace Yard.Generators.TreeDump

open Yard.Core

type TreeDump() = 
    inherit Generator()
        override this.Name = "TreeDump"
        override this.Generate(t,_) = (sprintf "%A" t) :> obj
        override this.Generate(t,_,_) = (sprintf "%A" t) :> obj
        override this.Constraints = [||]
        member this.Generate t = (sprintf "%A" t) :> obj
