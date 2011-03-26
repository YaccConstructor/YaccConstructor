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

let () =
    let feName = ref None
    let generatorName = ref None
    let testsPath = ref <| Some ""
    let testFile = ref None

    feName := // Fill by default value
        if Seq.exists ((=) "YardFrontend") FrontendsManager.AvailableFrontends then
            Some("YardFrontend")
        else
            Seq.tryFind (fun _ -> true) FrontendsManager.AvailableFrontends

    generatorName :=
        if Seq.exists ((=) "RACCGenerator") GeneratorsManager.AvailableGenerators then
            Some("RACCGenerator")
        else
            Seq.tryFind (fun _ -> true) GeneratorsManager.AvailableGenerators

    let generateSomething = ref true

    let printItems iName items deft =
        fun _ ->
            generateSomething := false
            printfn "\nAvailable %s: " iName
            Seq.map (fun x -> x + (if Some(x)=deft then " (default)" else "")) items
            |> String.concat "\n    "
            |> fun x -> printf "    %s\n" x

    let commandLineSpecs =
        ["-f", ArgType.String (fun s -> feName := Some s), "Frontend name. Use -af to list available."
         "-af", ArgType.Unit (printItems "frontends" FrontendsManager.AvailableFrontends !feName), "Available frontends"
         "-g", ArgType.String (fun s -> generatorName := Some s), "Generator name. Use -ag to list available."
         "-ag", ArgType.Unit (printItems "generators" GeneratorsManager.AvailableGenerators !generatorName), "Available generators"
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
            let ilTreeExpandedMeta = ConvertionsManager.ApplyConvertion ilTree (new Yard.Core.Convertions.ExpandMeta.ExpandMeta())

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
        | _, None, _          -> EmptyArg "frontend name (-f)" |> raise
        | _, _, None          -> EmptyArg "generator name (-g)" |> raise
        | None , _, _         -> EmptyArg "file name (-t)" |> raise 
    try
        if !generateSomething = true then 
            run ()
    with 
    | InvalidFEName (feName)   -> 
        "Frontend with name " + feName + " is not available. Run \"Main.exe -af\" for get all available frontends.\n" 
        |> System.Console.WriteLine
    | InvalidGenName (genName) ->
        "Generator with name " + genName + " is not available. Run \"Main.exe -ag\" for get all available generators.\n"
        |> System.Console.WriteLine
    | EmptyArg (argName)       ->
         printfn "Argument can not be empty: %s\n\nYou need to specify frontend, generator and input grammar. Example:
Main.exe -f AntlrFrontend -g YardPrinter -i ../../../../Tests/ANTLR/C.g > C.yrd\n
List of available frontends and generators can be obtained by -af -ag keys" argName
    | FEError (error)          ->
        "Frontend error: " + error + "\n"
        |> System.Console.WriteLine
    | GenError (error)         ->
        "Generator error: " + error + "\n"
        |> System.Console.WriteLine
    | :? System.IO.IOException -> printf "Could not read input file\n"
    | x -> printf "%A\n" x


//Tests. Please do not remove
//Main.exe -g YardPrinter -t ../../../../Tests/Basic/test_include/test_include_main.yrd
//Main.exe -g YardPrinter -t ../../../../Tests/Basic/test_seq/test_seq.yrd

(*
open Yard.Core.IL.Production

let () = 
    let filename = @"..\..\..\..\Tests\TempTests\test_expand_brackets_2.yrd" 
//    let ilTree = ref (Yard.Frontends.YardFrontend.Main.ParseFile filename)\
    let ilTree = ref { new Definition.t<Source.t, Source.t> with
        info = { fileName = "" } 
        and head = None 
        and foot = None 
        and grammar = [
            { new Rule.t<Source.t, Source.t> with 
                name = "s"
                and args = []
                and metaArgs = []
                and _public = true
                and body = PAlt(
                    PSeq([
                        {new elem<Source.t, Source.t> with omit=false and binding=None and checker=None and rule=PToken("NUMBER",(0,0))};
                        {new elem<Source.t, Source.t> with omit=false and binding=None and checker=None and rule=PAlt(PToken("ALT1",(0,0)),PToken("ALT2",(0,0)))}
                        {new elem<Source.t, Source.t> with omit=false and binding=None and checker=None and rule=PToken("CHUMBER",(0,0))};
                        ], None),
                    PToken("OUTER",(0,0))
                )
            }
        ]
    }

    printfn "Before:\n%A\n" <| (GeneratorsManager.Generator "YardPrinter").Generate(!ilTree)
//    printfn "%A\n" !ilTree
//    ilTree := ConvertionsManager.ApplyConvertion !ilTree (new Yard.Core.Convertions.ExpandEBNF.ExpandEBNF())
//    ilTree := ConvertionsManager.ApplyConvertion !ilTree (new Yard.Core.Convertions.ExpandMeta.ExpandMeta())
    ilTree := ConvertionsManager.ApplyConvertion !ilTree (new Yard.Core.Convertions.ExpandBrackets.ExpandBrackets())
    printfn "After\n%A\n" <| (GeneratorsManager.Generator "YardPrinter").Generate(!ilTree)
//    printfn "%A" !ilTree
    ()
*)