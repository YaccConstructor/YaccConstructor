namespace Yard.Generators.YardPrinter

open Yard.Core

type YardPrinter() = 
    interface IGenerator with
        member this.Name = "YardPrinter"
        member this.Generate t = Generator.generate t :> obj
        member this.AcceptableProductionTypes = 
            List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>)
            |> List.map (fun unionCase -> unionCase.Name)
    end