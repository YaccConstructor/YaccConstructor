namespace Yard.Generators.RecursiveAscent

open Yard.Core

type RecursiveAscent() = 
    interface IGenerator with
        member this.Name = "RecursiveAscent"
        member this.Generate t = Generator.generate t :> obj
        member this.AcceptableProductionTypes = List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) |> List.map (fun unionCase -> unionCase.Name)
    end
