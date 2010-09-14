namespace Yard.Generators.TreeDump

open Yard.Core

type TreeDump() = 
    interface IGenerator with
        member this.Name = "TreeDump"
        member this.Generate t = t.ToString() :> obj
        member this.AcceptableProductionTypes = List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) |> List.map (fun unionCase -> unionCase.Name)
    end
