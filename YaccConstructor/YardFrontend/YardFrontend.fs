namespace Yard.Frontends

open Yard.Core

type YardFrontend() = 
    interface IFrontend with
        member this.Name = "YardFrontend"
        member this.ParseFile t = Main.ParseFile t
    end

