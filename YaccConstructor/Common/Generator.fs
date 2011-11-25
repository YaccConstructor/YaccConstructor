//  Copyright 2010-2011 by Konstantin Ulitin
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

namespace Yard.Core

open Yard.Core.IL

[<AbstractClass>]
type Generator() as this =
    abstract Name : string
    abstract Generate : Definition.t<Source.t,Source.t> -> obj
    abstract Generate : Definition.t<Source.t,Source.t> * string -> obj
    default this.Generate(grammar, string) = this.Generate(grammar)
    abstract AcceptableProductionTypes : string list
    interface Yard.Core.Manager.IComponent with
        member self.Name : string =  this.Name