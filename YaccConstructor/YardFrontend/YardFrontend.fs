namespace Yard.Frontends.YardFrontend

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

// For testing switch to Console App and then switch back to Class Library
module Run = 
    FrontendsManager.Register(new YardFrontend()) // Not register itself automatically
    //let filename = @"..\..\..\..\Tests\Basic\test_include\test_include_main.yrd" 
    let filename = @"..\..\..\..\Tests\RACC\test_arithm_glr\test_arithm_glr.yrd" 
    printf "%A\n" <| Yard.Frontends.YardFrontend.Main.ParseFile filename