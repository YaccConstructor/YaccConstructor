//  Copyright 2009-2011 Konstantin Ulitin
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

namespace Yard.Generators.FsYaccPrinter

open Yard.Core
open Constraints
open Mono.Addins

[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type FsYaccPrinter() = 
    inherit Generator()
        override this.Name = "FsYaccPrinter"
        override this.Generate t =
            this.Generate (t, "")
        override this.Generate(t, tokenType) = 
            let outFile = t.info.fileName + ".fsy"
            let res = Generator.generate t tokenType
            System.IO.File.WriteAllText(outFile,res)
            res :> obj
        override this.Constraints = [|noMeta; noEbnf; noInnerAlt; noLiterals; noInnerAlt; noBrackets; needAC; singleModule|]
