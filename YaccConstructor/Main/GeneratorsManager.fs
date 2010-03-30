module Main.GeneratorsManager

open Yard.Core.IL

type Generator = 
    abstract generate:Definition.t<Source.t,Source.t> -> string

let private generatorsCollection = 
    [
        ("TreeDump", {new Generator with member this.generate ilTree = Generators.TreeDump.generate ilTree})
    ] 
    |> Map.ofList

(*
let registerFrontend feName fe = 
    frontendsCollection <- frontendsCollection.Add (feName, fe)
*)

let getGenerator genName = 
    generatorsCollection.Item genName  
