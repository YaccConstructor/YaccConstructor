#I @"../../3rdParty/YCLight"

#r @"RNGLRGenerator.dll"
#r @"Core.dll"
#r @"FsYaccFrontend.dll"

open YC.Parsing.RNGLR.Generator
open YC.Frontends.FsYaccFrontend

module YardFrontend =
    let gen = new RNGLR()
    let fe = new FsYaccFrontend()

    let generate() =
        let il = fe.ParseGrammar "src/YardFrontend/Parser.fsy"
        gen.Generate(il, true, "-o src/YardFrontend/Parser.fs -module YC.Frontends.YardFrontend.GrammarParser -pos SourcePosition -token Source") |> ignore

YardFrontend.generate()
