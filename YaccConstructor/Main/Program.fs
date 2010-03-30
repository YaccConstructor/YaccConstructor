module Main.Program

open Yard.Core

let main =
//    let commandLineArgs = System.Environment.GetCommandLineArgs()
    let grammarFilePath = @"..\..\..\..\Tests\test010.yrd"
    let feName = "Yard"
    let generatorName = "TreeDump"
    let ilTree = (FrontendsManager.getFrontend feName).parseFile grammarFilePath
    printf "%A" ((GeneratorsManager.getGenerator generatorName).generate ilTree)
do main
