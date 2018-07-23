module Yard.Tools.FsYaccApp

open Yard.Generators.RNGLR
open Yard.Frontends.FsYaccFrontend

let gen = new RNGLR()
let fe = new FsYaccFrontend()

let generate name options =
    let il = fe.ParseGrammar name
    gen.Generate(il, true, options) |> ignore

[<EntryPoint>]
let main argv =
    generate argv.[0] argv.[1]
    0
