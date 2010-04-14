module Main.Program

open Yard.Core
open Yard.Core.IL

//TODO: move it to ConvertionManager
let ApplyConvertion (ilTree:Definition.t<Source.t,Source.t>) (conv:IConvertion) = 
    {   new Definition.t<Source.t,Source.t>
        with info = ilTree.info
        and  head = ilTree.head
        and  grammar = conv.ConvertList ilTree.grammar
        and  foot = ilTree.foot
    }

let () =
//    let commandLineArgs = System.Environment.GetCommandLineArgs()
    let grammarFilePath = @"..\..\..\..\Tests\test101.yrd"
    let feName = "YardFrontend"
    let generatorName = "TreeDump"

    // Load frontends assemblies dlls - get them from file, current folder or command line
    let assembly = System.Reflection.Assembly.Load("YardFrontend")
    let inst = assembly.CreateInstance("Yard.Frontends.YardFrontend")
    FrontendsManager.Register(inst :?> IFrontend);

    // Load generator assemblies dlls - get them from file, current folder or command line
    let assembly = System.Reflection.Assembly.Load("TreeDump")
    let inst = assembly.CreateInstance("Yard.Generators.TreeDump")
    GeneratorsManager.Register(inst :?> IGenerator);

    let ilTree = (FrontendsManager.Frontend feName).ParseFile grammarFilePath
    let ilTreeExpandedEBNF = ApplyConvertion ilTree (new Yard.Core.Convertions.ExpandEBNF.ExpandEBNF())
    let gen = GeneratorsManager.Generator(generatorName)
    let s = gen.Generate ilTree
    printf "%A" s

