﻿namespace Yard.Frontends.YardFrontend

open Yard.Core

type YardFrontend() = 
    interface IFrontend with
        member this.Name = "YardFrontend"
        member this.ParseGrammar t = 
            match t with
            | (:? System.String as s) -> Main.ParseFile s
            | _ -> IL.Definition.empty
        member this.ProductionTypes = List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) |> List.map (fun unionCase -> unionCase.Name)
    end

