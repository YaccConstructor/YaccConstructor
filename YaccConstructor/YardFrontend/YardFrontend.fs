namespace Yard.Frontends

open Yard.Core

type YardFrontend() = 
    interface IFrontend with
        member this.Name = "YardFrontend"
        member this.ParseFile t = Main.ParseFile t
        member this.ProductionTypes = List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) |> List.map (fun unionCase -> unionCase.Name)
    end

