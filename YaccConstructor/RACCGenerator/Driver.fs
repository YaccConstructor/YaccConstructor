//  Driver.fs -- main file of generator
//
//  Copyright 2009, 2010, 2011 Semen Grigorev <rsdpisuy@gmail.com>
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

open Yard.Core

type RACCGenerator() = 
    inherit Generator()
        override this.Name = "RACCGenerator"
        override this.Generate t = 
            let extension = ".fs"
            let tablesStr = ".tables"
            let actionsStr = ".actions"
            let codeGenerator = CodeGenerator(t.info.fileName + actionsStr + extension)
            let tableGenerator = TableGenerator(t.info.fileName + tablesStr + extension)
            let transformedGrammar = {t with grammar = Convertions.ExpandMeta.expandMetaRules t.grammar}
            let typeToTag = tableGenerator.Gemerate transformedGrammar
            codeGenerator.Gemerate transformedGrammar typeToTag :> obj
        override this.AcceptableProductionTypes = 
            List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) 
            |> List.map (fun unionCase -> unionCase.Name)