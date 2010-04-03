namespace Yard.Generators

open Yard.Core

type TreeDump() = 
    interface IGenerator with
        member this.Name = "TreeDump"
        member this.Generate t = t.ToString()
    end