module Main.Program

open Yard.Core

let () =
//    let commandLineArgs = System.Environment.GetCommandLineArgs()
    let grammarFilePath = @"..\..\..\..\Tests\test010.yrd"
    let feName = "YardFrontend"
    let generatorName = "TreeDump"

    // Load frontends assemblies dlls - get them from file, current folder or command line
    let ass = System.Reflection.Assembly.Load("YardFrontend")
    let inst = ass.CreateInstance("Yard.Frontends.YardFrontend")
    FrontendsManager.Register(inst :?> IFrontend);

    // Load generator assemblies dlls - get them from file, current folder or command line
    let ass = System.Reflection.Assembly.Load("TreeDump")
    let inst = ass.CreateInstance("Yard.Generators.TreeDump")
    GeneratorsManager.Register(inst :?> IGenerator);

    let ilTree = (FrontendsManager.Frontend feName).ParseFile grammarFilePath
    let gen = GeneratorsManager.Generator(generatorName)
    let s = gen.Generate ilTree
    printf "%A" s

