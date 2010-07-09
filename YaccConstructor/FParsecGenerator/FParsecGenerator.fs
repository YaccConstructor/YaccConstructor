namespace Yard.Generators.FParsecGenerator

open Yard.Core

type FParsecGenerator() = 
    interface IGenerator with
        member this.Name = "FParsecGenerator"
        member this.Generate t = Program.generate t :> obj
        member this.AcceptableProductionTypes = List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) |> List.map (fun unionCase -> unionCase.Name)
    end