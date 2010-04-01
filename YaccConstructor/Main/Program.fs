module Main.Program

open Yard.Core
open Yard.Generators.TreeDump

let () =
//    let commandLineArgs = System.Environment.GetCommandLineArgs()
    let grammarFilePath = @"..\..\..\..\Tests\test010.yrd"
    let feName = "Yard"
    let generatorName = "TreeDump"
    let ilTree = (FrontendsManager.getFrontend feName).parseFile grammarFilePath
    printf "%A" ((GeneratorsManager.Generator generatorName).Generate ilTree)

