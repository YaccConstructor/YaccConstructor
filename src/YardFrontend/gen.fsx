#I @"../../3rdParty/YCLight"

#r @"RNGLRGenerator.dll"
#r @"Core.dll"
#r @"FsYaccFrontend.dll"
#r @"Parsing.Common.dll"
//Comment next line if you want to execute script in FSharpInteractive in IDE
#r @"../../packages/FSharp.Compiler.Tools.10.0.2/tools/FSharp.Core.dll"

open YC.Parsing.RNGLR.Generator
open YC.Frontends.FsYaccFrontend

module YardFrontend =
    let gen = new RNGLR()
    let fe = new FsYaccFrontend()

    let generate() =
        let il = fe.ParseGrammar "src/YardFrontend/Parser.fsy"
        gen.Generate(il, true, "-o src/YardFrontend/Parser.fs -module YC.Frontends.YardFrontend.GrammarParser -pos SourcePosition -token Source") |> ignore

YardFrontend.generate()
