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
            let tables_str = ".tables"
            let codeGenerator = CodeGenerator(t.info.fileName + extension)
            let tableGenerator = TableGenerator(t.info.fileName + tables_str + extension)
            tableGenerator.Gemerate t :> obj
            //codeGenerator.Gemerate t :> obj
        member this.AcceptableProductionTypes = 
            List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) 
            |> List.map (fun unionCase -> unionCase.Name)
    end