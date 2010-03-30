module Main.FrontendsManager

open Yard.Core.IL

type Frontend = 
    abstract parseFile:string -> Definition.t<Source.t,Source.t>

let private frontendsCollection = 
    [
        ("Yard", {new Frontend with member this.parseFile path = Yard.Core.Main.ParseFile path})
    ]
    |> Map.ofList 

(*
let registerFrontend feName fe = 
    frontendsCollection <- frontendsCollection.Add (feName, fe)
*)

let getFrontend feName = 
    frontendsCollection.Item feName  
