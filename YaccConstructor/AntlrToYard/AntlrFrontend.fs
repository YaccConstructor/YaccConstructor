namespace Yard.Frontends.AntlrFrontend

open Yard.Core

type AntlrFrontend() = 
    interface IFrontend with
        member this.Name = "AntlrFrontend"
        member this.ParseGrammar t = 
            match t with
            | (:? System.String as s) -> Main.ParseFile s
            | _ -> IL.Definition.empty
        member this.ProductionTypes = List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) |> List.map (fun unionCase -> unionCase.Name)
    end


module Run = 

Yard.Frontends.AntlrFrontend.Main.run ()