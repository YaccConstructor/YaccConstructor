module Main.Program

open Yard.Core
open Yard.Core.IL
open Microsoft.FSharp.Text

//TODO: move it to ConvertionManager
let ApplyConvertion (ilTree:Definition.t<Source.t,Source.t>) (conv:IConvertion) = 
    {   new Definition.t<Source.t,Source.t>
        with info = ilTree.info
        and  head = ilTree.head
        and  grammar = conv.ConvertList ilTree.grammar
        and  foot = ilTree.foot
    }

let () =   
  try 

    let feName = ref "YardFrontend"
    let generatorName = ref "YardPrinter"
    let testsPath = ref @"..\..\..\..\Tests"
    let testFile = ref "test003.yrd"

    let commandLineSpecs =
        ["-f", ArgType.String (fun s -> feName := s), "Frontend name"
//         "-c", ArgType.String (fun s -> 
         "-g", ArgType.String (fun s -> generatorName := s), "Generator name"
         "--testpath", ArgType.String (fun s -> testsPath := s), "Directory where test files are placed"
         "-t", ArgType.String (fun s -> testFile := s), "Name of test file"
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    let commandLineArgs = System.Environment.GetCommandLineArgs()
    ArgParser.Parse commandLineSpecs

    let grammarFilePath = !testsPath + "\\" + !testFile


    // Load frontends assemblies dlls - get them from file, current folder or command line
    try
        let assembly = System.Reflection.Assembly.Load(!feName)
        let inst = assembly.CreateInstance("Yard.Frontends." + !feName + "." + !feName)
        FrontendsManager.Register(inst :?> IFrontend);
    with _ -> printfn "%A is not correct frontend name" !feName
    
    // Load generator assemblies dlls - get them from file, current folder or command line
    try
        let assembly = System.Reflection.Assembly.Load(!generatorName)
        let inst = assembly.CreateInstance("Yard.Generators." + !generatorName + "." + !generatorName)
        GeneratorsManager.Register(inst :?> IGenerator);
    with _ -> printfn "%A is not correct generator name" !generatorName
    
    // Parse grammar    
    let ilTree = (FrontendsManager.Frontend !feName).ParseGrammar grammarFilePath

    // Apply convertions
    let ilTreeExpandedMeta = ApplyConvertion ilTree (new Yard.Core.Convertions.ExpandMeta.ExpandMeta())

    // Generate something
    let gen = GeneratorsManager.Generator(!generatorName)
    let s = gen.Generate (ilTree) // дерево передается без конвертации для FParsecGenerator

    //Run tests
  //  let tester = Yard.Generators.RecursiveAscent.RACCTester((*s :?> _*))
  //  let s = tester.RunTest 
    printf "%A" s
    //printf "file Name \n %A \n" <| System.IO.Path.ChangeExtension(ilTree.info.fileName,".fs")
  with 
  | :? System.IO.IOException -> eprintf "Could not read file"
  | x -> eprintf "%A" x // program should terminate correctly. Writing to error stream for Tester