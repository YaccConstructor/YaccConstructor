// Driver.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.
// Driver for RACC generator. 


namespace Yard.Generators._RACCGenerator

open Yard.Core

type _RACCGenerator() = 
    interface IGenerator with        
        member this.Name = "_RACCGenerator"
        member this.Generate t = 
            let extension = ".fs"
            let tablesStr = ".tables"
            let actionsStr = ".actions"
            let codeGenerator = CodeGenerator(t.info.fileName + actionsStr + extension)
            let tableGenerator = TableGenerator(t.info.fileName + tablesStr + extension)
            tableGenerator.Gemerate t
            codeGenerator.Gemerate t :> obj
        member this.AcceptableProductionTypes = 
            List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) 
            |> List.map (fun unionCase -> unionCase.Name)
    end