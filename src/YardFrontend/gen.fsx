#r @"RNGLRGenerator.dll"
#r @"Common.dll"
#r @"FsYaccFrontend.dll"

open Yard.Generators.RNGLR
open Yard.Frontends.FsYaccFrontend

module YardFrontend =
    let gen = new RNGLR()
    let fe = new FsYaccFrontend()

    let generate() =
        let il = fe.ParseGrammar "parser.fsy"
        gen.Generate(il, true, "-o Parser.fs -module Yard.Frontends.YardFrontend.GrammarParser -pos SourcePosition -token Source") |> ignore

YardFrontend.generate()
