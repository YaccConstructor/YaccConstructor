module Main.Program

open Yard.Core
open Yard.Core.IL
open Microsoft.FSharp.Text
open System.IO

exception InvalidFEName of string
exception InvalidGenName of string
exception EmptyArg of string
exception FEError of string
exception GenError of string

//TODO: move it to ConvertionManager
let ApplyConvertion (ilTree:Definition.t<Source.t,Source.t>) (conv:IConvertion) = 
    {   new Definition.t<Source.t,Source.t>
        with info = ilTree.info
        and  head = ilTree.head
        and  grammar = conv.ConvertList ilTree.grammar
        and  foot = ilTree.foot
    }

let () =     
    let feName = ref None
    let generatorName = ref None
    let testsPath = ref <| Some ""
    let testFile = ref None

    let printItems iName items = 
        fun _ ->
            printfn "\nAvailable %s: " iName                                    
            items
            |> String.concat ",\n    " 
            |> fun x -> printf "    %s" (x + "\n") 

    let commandLineSpecs =
        ["-f", ArgType.String (fun s -> feName := Some s), "Frontend name"
         "-af", ArgType.Unit (printItems "frontends" FrontendsManager.AvailableFrontends),"Available frontends"
         "-g", ArgType.String (fun s -> generatorName := Some s), "Generator name"
         "-ag", ArgType.Unit (printItems "generators" GeneratorsManager.AvailableGenerators),"Available generators"
         "-i", ArgType.String (fun s -> 
                                   testFile := System.IO.Path.GetFileName(s) |> Some 
                                   testsPath := System.IO.Path.GetDirectoryName(s) |> Some), "Input grammar"         
         "--testpath", ArgType.String (fun s -> testsPath := Some s), "[DEBUG] Directory where test files are placed"
         "-t", ArgType.String (fun s -> testFile := Some s), "[DEBUG] Name of test file"
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    let commandLineArgs = System.Environment.GetCommandLineArgs()
    ArgParser.Parse commandLineSpecs
    
    let run () =
        match !testFile, !feName , !generatorName with
        | None, None, None                               -> ()
        | Some(fName), Some(feName), Some(generatorName) -> 
            let grammarFilePath = System.IO.Path.Combine((!testsPath).Value, fName)
            let fe =
                let _raise () = InvalidFEName feName |> raise
                if Seq.exists ((=) feName) FrontendsManager.AvailableFrontends
                then
                    try
                        FrontendsManager.Frontend feName
                    with
                    | _ -> _raise ()
                else _raise ()

            // Parse grammar    
            let ilTree =                
                try
                    fe.ParseGrammar grammarFilePath
                with
                | e -> FEError e.Message |> raise

            // Apply convertions
            let ilTreeExpandedMeta = ApplyConvertion ilTree (new Yard.Core.Convertions.ExpandMeta.ExpandMeta())

            let gen =
                let _raise () = InvalidGenName generatorName |> raise
                if Seq.exists ((=) generatorName) GeneratorsManager.AvailableGenerators
                then              
                    try
                        GeneratorsManager.Generator generatorName
                    with
                    | _ -> _raise ()
                else _raise ()
                               
            // Generate something
            let result =            
                try
                    gen.Generate (ilTree)
                with
                | e -> GenError e.Message |> raise

#if DEBUG               
            printf "%A" result
#endif
            ()
        | None , _, _         -> EmptyArg "file name" |>raise 
        | _, None, _          -> EmptyArg "frontend name" |> raise
        | _, _, None          -> EmptyArg "genearator name" |> raise
                
    try
        run ()
    with 
    | InvalidFEName (feName)   -> 
        "Frontend with name " + feName + " is not available. Run \"Main.exe -af\" for get all available frontends.\n" 
        |> System.Console.WriteLine
    | InvalidGenName (genName) ->
        "Generator with name " + genName + " is not available. Run \"Main.exe -ag\" for get all available generators.\n"
        |> System.Console.WriteLine
    | EmptyArg (argName)       ->
        "Argument can not be empty: " + argName + "\n"
        |> System.Console.WriteLine
    | FEError (error)          ->
        "Frontend error: " + error + "\n"
        |> System.Console.WriteLine
    | GenError (error)         ->
        "Generator error: " + error + "\n"
        |> System.Console.WriteLine
    | :? System.IO.IOException -> printf "Could not read input file\n"
    | x -> printf "%A\n" x