module Yard.Generators.TreeDump
open Yard.Core.GeneratorsManager
do Register ("TreeDump", {new IGenerator with member this.Generate t = t.ToString ()})
do printf "Hello"