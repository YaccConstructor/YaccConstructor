//  FParsecGenerator.fs
//
//  Copyright 2010 Anastasia Nishnevich <Anastasia.Nishnevich@gmail.com>
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

namespace Yard.Generators.FParsecGenerator

open Yard.Core
open Constraints
open Mono.Addins

[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type FParsecGenerator() = 
    inherit Generator()
        override this.Name = "FParsecGenerator"
        override this.Generate t = Program.generate t :> obj
        override this.Constraints = [|noMeta; singleModule|]
