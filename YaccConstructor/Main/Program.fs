//  Copyright 2010,2011 Konstantin Ulitin, Semen Grigorev
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module YaccConstructor.Program

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
    let convertions = new ResizeArray<string>()

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
         "-c", ArgType.String (fun s -> convertions.Add(s)), "Convertion applied in order. Use -ac to list available."
         "-ac", ArgType.Unit (printItems "convertions" ConvertionsManager.AvailableConvertions None), "Available convertions"
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
                    ref (fe.ParseGrammar grammarFilePath)
                with
                | e -> FEError e.Message |> raise

            // Apply convertions
            Seq.iter (fun conv -> ilTree := (ConvertionsManager.ApplyConvertion !ilTree conv)) convertions

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
                    gen.Generate (!ilTree)
                with
                | e -> GenError e.Message |> raise

//#if DEBUG               
            printf "%s" (result :?> string)
//#endif
            ()
        | _, None, _          -> EmptyArg "frontend name (-f)" |> raise
        | _, _, None          -> EmptyArg "generator name (-g)" |> raise
        | None , _, _         -> EmptyArg "file name (-i)" |> raise 
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
YaccConstructor.exe -f YardFrontend -c BuildAST -g YardPrinter -i ../../../../Tests/Convertions/buildast_1.yrd \n
List of available frontends, generators and convertions can be obtained by -af -ag -ac keys" argName
    | FEError (error)          ->
        "Frontend error: " + error + "\n"
        |> System.Console.WriteLine
    | GenError (error)         ->
        "Generator error: " + error + "\n"
        |> System.Console.WriteLine
    | :? System.IO.IOException -> printf "Could not read input file\n"
    | x -> printf "%A\n" x


//Tests. Please do not remove
//YaccConstructor.exe -f AntlrFrontend -g FsYaccPrinter -c ExpandEBNF -c ExpandMeta -c ExpandBrackets -i ../../../../Tests/ANTLR/C.g
//YaccConstructor.exe -g YardPrinter -i ../../../../Tests/Basic/test_include/test_include_main.yrd
//YaccConstructor.exe -g YardPrinter -i ../../../../Tests/Basic/test_seq/test_seq.yrd
//YaccConstructor.exe -g YardPrinter -c ExpandEBNF -c ExpandMeta -c ExpandBrackets -i ../../../../Tests/RACC/claret/braces_1/test_simple_braces.yrd
//YaccConstructor.exe -g FsYaccPrinter -c ExpandEBNF -c ExpandMeta -c ExpandBrackets -i ../../../../Tests/RACC/claret/braces_1/test_simple_braces.yrd
//YaccConstructor.exe -g FsYaccPrinter -c ExpandEBNF -c ExpandMeta -c ExpandBrackets -c AddEOF -c ReplaceLiterals -i ../../../../Tests/RACC/claret/braces_1/test_simple_braces.yrd
//YaccConstructor.exe -g TreeDump -c AddEOF -c ExpandEBNF -c ExpandMeta -c ExpandBrackets -i ../../../../Tests/RACC/claret/braces_1/test_simple_braces.yrd
//YaccConstructor.exe -g YardPrinter -c ReplaceLiterals -i ../../../../Tests/TempTests/test1.yrd
//YaccConstructor.exe -g FsYaccPrinter -c ExpandEBNF -c ExpandMeta -c ExpandBrackets -c AddEOF -c ReplaceLiterals -i ..\..\..\FsYaccFrontend\fsyacc.yrd
//YaccConstructor.exe -f FsYaccFrontend -g YardPrinter -i ../../../../Tests/FsYacc/antlr.fsy
//YaccConstructor.exe -c BuildAST -g YardPrinter -i ../../../../Tests/Basic/test_summator_1/test_summator_1.yrd
//YaccConstructor.exe -f FsYaccFrontend -g YardPrinter -i ../../../../Tests/FsYacc/cparser.mly
//YaccConstructor.exe -c "ReplaceLiterals KW_%s" -g YardPrinter -i ../../../../Tests/TempTests/test1.yrd
//YaccConstructor.exe -c ReplaceLiterals -g YardPrinter -i ../../../../Tests/TempTests/test1.yrd
//YaccConstructor.exe -c BuildAST -g YardPrinter -i ../../../../Tests/Convertions/buildast_1.yrd
//YaccConstructor.exe -g YardPrinter -c "ReplaceLiterals KW_%s" -c BuildAST -i ../../../../Tests/Convertions/buildast_1.yrd
//YaccConstructor.exe -g YardPrinter -c ExpandEbnfStrict -i ../../../../Tests/Convertions/expandebnfstrict_1.yrd
//YaccConstructor.exe -g YardPrinter -c "BuildAST typed"-i ../../../../Tests/Convertions/buildast_1.yrd
//YaccConstructor.exe -g YardPrinter -c MergeAlter -i ../../../../Tests/Convertions/mergealter_1.yrd

