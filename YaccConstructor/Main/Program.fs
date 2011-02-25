module Main.Program

open Yard.Core
open Yard.Core.IL
open Microsoft.FSharp.Text
open System.IO

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
  
    let feName = ref "AntlrFrontend"
    let generatorName = ref "YardPrinter"
    let testsPath = ref @"..\..\..\..\Tests\ANTLR"
    let testFile = ref "url.g"    

    let printItems iName items = 
        fun _ ->
            printfn "\nAvailable %s: " iName                                    
            items
            |> String.concat ",\n    " 
            |> fun x -> printf "    %s" (x + "\n") 
    let commandLineSpecs =
        ["-f", ArgType.String (fun s -> feName := s), "Frontend name"
         "-af", ArgType.Unit (printItems "frontends" FrontendsManager.AvailableFrontends),"Available frontends"
         "-g", ArgType.String (fun s -> generatorName := s), "Generator name"
         "-ag", ArgType.Unit (printItems "generators" GeneratorsManager.AvailableGenerators),"Available generators"
         "-i", ArgType.String (fun s -> 
                                   testFile := System.IO.Path.GetFileName(s);
                                   testsPath := System.IO.Path.GetDirectoryName(s)), "Input grammar"         
         "--testpath", ArgType.String (fun s -> testsPath := s), "[DEBUG] Directory where test files are placed"
         "-t", ArgType.String (fun s -> testFile := s), "[DEBUG] Name of test file"
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    let commandLineArgs = System.Environment.GetCommandLineArgs()
    ArgParser.Parse commandLineSpecs

    let grammarFilePath = System.IO.Path.Combine(!testsPath, !testFile)

    // Parse grammar    
    let ilTree = (FrontendsManager.Frontend !feName).ParseGrammar grammarFilePath        

    // Apply convertions
    let ilTreeExpandedMeta = ApplyConvertion ilTree (new Yard.Core.Convertions.ExpandMeta.ExpandMeta())

    // Generate something
    let gen = GeneratorsManager.Generator(!generatorName)
    let s = gen.Generate (ilTree) // дерево передается без конвертации для FParsecGenerator
       
    printf "%A" s
    
    ()
  with 
  | :? System.IO.IOException -> printf "Could not read file"
  | x -> printf "%A" x // program should terminate correctly. Writing to error stream for Tester