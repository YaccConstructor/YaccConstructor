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


namespace Yard.Generators.YardPrinter

open Yard.Core

type YardPrinter() = 
    inherit Generator()
        override this.Name = "YardPrinter"
        override this.Generate(t,f) = Generator.generate t :> obj
        override this.Generate(t,_,_) = Generator.generate t :> obj
        override this.Constraints = [||]
