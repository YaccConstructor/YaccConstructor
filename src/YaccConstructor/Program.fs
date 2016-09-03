//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

module YaccConstructor.Program

open Mono.Addins
open Yard.Core
open Yard.Core.IL
open Yard.Core.Helpers
open Yard.Core.Checkers
open Microsoft.FSharp.Text
open System.IO
open System.Reflection

exception InvalidFEName of string
exception InvalidGenName of string
exception EmptyArg of string
exception FEError of string
exception GenError of string
exception CheckerError of string

[<assembly:AddinRoot ("YaccConstructor", "1.0")>]
do()

let eol = System.Environment.NewLine

let log (e:System.Exception) msg =
    printfn "ERROR!"
    "\nStack trace:\n " + e.StackTrace
    |> printfn "%s"
    "\nInternal message:\n  " + e.Message
    |> printfn "%s"
    "\nMessage:\n  " + msg
    |> printfn "%s"

open Argu

type CLIArguments =
    | [<Unique; AltCommandLine("-f")>] Frontend of feName:string
    | [<AltCommandLine("-af")>] AvailableFrontends
    | [<Unique; AltCommandLine("-g")>] Generator of generatorName:string 
    | [<AltCommandLine("-ag")>] AvailableGenerators
    | [<AltCommandLine("-c")>] Conversion of conversionName:string
    | [<AltCommandLine("-ac")>] AvailableConversions
    | [<AltCommandLine("-d")>] DefConstant of userD:string
    | [<AltCommandLine("-u")>] UndefConstant of userR:string
    | [<Unique; AltCommandLine("-i")>] Input of path:string
with
    interface IArgParserTemplate with   
        member s.Usage =
            match s with
            | Frontend _ -> "Frontend name. Use -af to list available."
            | AvailableFrontends _ -> "Available frontends"
            | Generator _ -> "Generator name. Use -ag to list available."
            | AvailableGenerators _ -> "Available generators"
            | Conversion _ -> "Conversion applied in order. Use -ac to list available."
            | AvailableConversions _ -> "Available conversions"
            | DefConstant _ -> "User defined constants for YardFrontend lexer."
            | UndefConstant _ -> "Remove previously defined constants for YardFrontend lexer."
            | Input _ -> "Input grammar"

let () =
    let feName = ref None
    let generatorName = ref None
    let generatorParams = ref None
    let testsPath = ref <| Some ""
    let testFile = ref None
    let conversions = new ResizeArray<string>()
    
    AddinManager.Initialize()    
    //let x = AddinManager.Registry.RegistryPath
    //printfn "%A" x
    //System.IO.Directory.Delete(x, true)
    AddinManager.Registry.Update(null)



    let addinFrontends = AddinManager.GetExtensionObjects (typeof<Frontend>) |> Seq.cast<Frontend> |> Seq.toArray
    let addinConversions = AddinManager.GetExtensionObjects (typeof<Conversion>) |> Seq.cast<Conversion> |> Seq.toArray
    let addinGenerators = AddinManager.GetExtensionObjects (typeof<Generator>) |> Seq.cast<Generator> |> Seq.toArray
    let addinFrontendNames = Seq.map (fun (elem : Frontend) -> elem.Name) addinFrontends |> Seq.toArray
    let addinConversionNames = Seq.map (fun (elem : Conversion) -> elem.Name) addinConversions |> Seq.toArray
    let addinGeneratorNames = Seq.map (fun (elem : Generator) -> elem.Name) addinGenerators |> Seq.toArray

    let userDefs = ref []
    let userDefsStr = ref ""

    feName := // Fill by default value
        if Array.exists (fun (elem : Frontend) -> elem.Name = "YardFrontend") addinFrontends
        then Some "YardFrontend"
        elif not <| Array.isEmpty addinFrontends
        then 
            let tmpName = addinFrontends.[0].Name
            Some tmpName
        else None
            
    generatorName :=
        if Array.exists (fun (elem : Generator) -> elem.Name = "RNGLRGenerator") addinGenerators
        then Some "RNGLRGenerator"
        elif not <| Array.isEmpty addinGenerators
        then 
            let tmpName = addinGenerators.[0].Name
            Some tmpName
        else None

    let generateSomething = ref true

    let printItems iName items deft =
            generateSomething := false
            printfn "\nAvailable %s: " iName
            Seq.map (fun x -> x + (if Some x = deft then " (default)" else "")) items
            |> String.concat "\n    "
            |> fun x -> printf "    %s\n" x

    let argv = System.Environment.GetCommandLineArgs()
    let parser = ArgumentParser.Create<CLIArguments>(errorHandler = ProcessExiter())
    let args = parser.Parse argv.[1..]
    for res in args.GetAllResults() do
        match res with 
        | Frontend fe -> feName := Some fe
        | AvailableFrontends -> printItems "frontends" addinFrontendNames !feName
        | Generator g -> match Array.toList (g.Split ' ') with
                         | name::[] -> 
                            generatorName := Some name
                            generatorParams := None
                         | name::parameters -> 
                            generatorName := Some name
                            generatorParams := Some (String.concat " " parameters)
                         | _ -> failwith "You need to specify generator name"
        | AvailableGenerators -> printItems "generators" addinGeneratorNames !generatorName
        | Conversion c -> conversions.Add c
        | AvailableConversions -> printItems "frontends" addinFrontendNames !feName
        | DefConstant d -> userDefs := !userDefs@[d]
        | UndefConstant u -> userDefs := List.filter((<>) u) !userDefs
        | Input i -> 
            testFile := System.IO.Path.GetFileName i |> Some
            testsPath := System.IO.Path.GetDirectoryName i |> Some


    let run () =
        match !testFile, !feName, !generatorName with
        | Some fName, Some feName, Some generatorName ->
            let grammarFilePath = System.IO.Path.Combine(testsPath.Value.Value, fName)
            let fe =
                let _raise () = InvalidFEName feName |> raise
                if Array.exists (fun (elem : Frontend) -> elem.Name = feName) addinFrontends
                then
                    try
                        match Array.tryFind (fun (elem : Frontend) -> elem.Name = feName) addinFrontends with
                        | Some fe -> fe
                        | None -> failwith "Frontend is not found."
                    with
                    | _ -> _raise ()
                else _raise ()

            // Parse grammar
            let ilTree =
                //try
                    let defStr = String.concat ";" !userDefs
                    if System.String.IsNullOrEmpty defStr
                    then grammarFilePath
                    else grammarFilePath + "%" + defStr
                    |> fe.ParseGrammar
                    |> ref
                //with
                //| e -> FEError (e.Message + " " + e.StackTrace) |> raise
            Namer.initNamer ilTree.Value.grammar

            let repeatedInnerRules, repeatedExportRules, undeclaredRules = GetUndeclaredNonterminalsList !ilTree

            let processErrors (errorList : (_ * 'a list) list) msg map delimiter =
                if errorList.Length > 0 then
                    eprintfn  "%s" msg
                    errorList |> List.iter (fun (m,rules) ->
                        rules
                        |> List.map map
                        |> String.concat delimiter
                        |> eprintfn "Module %s:%s%s" (getModuleName m) eol
                    )
                    
            processErrors undeclaredRules
                "Input grammar contains some undeclared nonterminals:"
                (fun rule -> sprintf "%s (%s:%d:%d)" rule.text rule.file rule.startPos.line rule.startPos.column)
                "; "
            processErrors repeatedInnerRules
                "There are more then one rule in one module for some nonterminals:"
                id
                ", "
            processErrors repeatedExportRules
                "There are rules, exported from different modules:"
                (fun (rule, ms) -> sprintf "%s (%s)" rule (String.concat "," ms))
                "; "
            processErrors (GetIncorrectMetaArgsCount !ilTree)
                "Some meta-rules have incorrect arguments number:"
                (fun (rule, got, expected) -> sprintf "%s(%d,%d): %d (expected %d)" rule.text rule.startPos.line rule.startPos.column got expected)
                eol
//            printfn "%A" <| ilTree
            let lostSources = ref false
            // Let possible to know, after what conversion we lost reference to original code
            let checkSources name il = 
                if not !lostSources then
                    match sourcesWithoutFileNames il with
                    | [] -> ()
                    | x ->
                        lostSources := true
                        x
                        |> List.map(fun s -> s.text) |> String.concat "\n"
                        |> printfn "Lost sources after frontend or conversion %s:\n %s" name
            checkSources fe.Name !ilTree
            // Apply Conversions

            let apply_Conversion (convNameWithParams:string) (ilTree:Definition.t<Source.t,Source.t>) = 
                let parameters = convNameWithParams.Split(' ')
                    //printfn "Conversion: %s" convNameWithParams
                if parameters.Length = 0 then failwith "Missing Conversion name"
                else
                    {ilTree
                     with grammar =
                            match Seq.tryFind (fun (elem : Conversion) -> elem.Name = parameters.[0]) addinConversions with 
                            | Some conv -> conv.ConvertGrammar (ilTree.grammar, parameters.[1..parameters.Length - 1])
                            | None -> failwith <| "Conversion not found: " + parameters.[0]
                            }

            for conv in conversions do
                ilTree := apply_Conversion conv !ilTree
                checkSources conv !ilTree
  //          printfn "========================================================"
           // printfn "%A" <| !ilTree
            let gen =
                let _raise () = InvalidGenName generatorName |> raise
                if Array.exists (fun (elem : Generator) -> elem.Name = generatorName) addinGenerators
                then              
                    try
                        match Array.tryFind (fun (elem : Generator) -> elem.Name = generatorName) addinGenerators with
                        | Some gen -> gen
                        | None -> failwith "TreeDump is not found."
                    with
                    | _ -> _raise ()
                else _raise ()
                               
            // Generate something
            
            let result =  
                //if not (IsSingleStartRule !ilTree) then
                //   raise <| CheckerError "Input grammar should contains only one start rule."
                //try
                //    let gen = new Yard.Generators.RNGLR.RNGLR()
                    for constr in gen.Constraints do
                        let grammar = ilTree.Value.grammar
                        if not <| constr.Check grammar then
                            eprintfn "Constraint %s: applying %s..." constr.Name constr.Conversion.Name
                            ilTree := {!ilTree with grammar = constr.Fix grammar}

                    match !generatorParams with
                    | None -> 
                        printfn "%A" <| !ilTree
                        gen.Generate !ilTree
                    | Some genParams -> gen.Generate(!ilTree, genParams)
                //with
//                | Yard.Generators.GNESCCGenerator.StartRuleNotFound 
//                    -> GenError "Start rule cannot be found in input grammar. Please, specify start rule."
//                       |> raise
                //| e -> GenError e.Message |> raise

            printf "%A" result
            System.IO.File.WriteAllText("out", string result)
            ()
        | _, None, _          -> EmptyArg "frontend name (-f)" |> raise
        | _, _, None          -> EmptyArg "generator name (-g)" |> raise
        | None , _, _         -> EmptyArg "file name (-i)" |> raise 
    try
        if !generateSomething = true then 
            run ()
    with
    | InvalidFEName feName as e  -> 
        "Frontend with name " + feName + " is not available. Run \"Main.exe -af\" for get all available frontends.\n" 
        |> log e
    | InvalidGenName genName as e->
        "Generator with name " + genName + " is not available. Run \"Main.exe -ag\" for get all available generators.\n"
        |> log e
    | EmptyArg argName as e ->
        sprintf "Argument can not be empty: %s\n\nYou need to specify frontend, generator and input grammar. Example:
YaccConstructor.exe -f YardFrontend -c BuildAST -g YardPrinter -i ../../../Tests/Conversions/buildast_1.yrd \n
List of available frontends, generators and conversions can be obtained by -af -ag -ac keys" argName
        |> log e
    | FEError error as e ->
        "Frontend error: " + error + "\n"
        |> log e
    | GenError error as e  ->
        "Generator error: " + error + "\n"
        |> log e
    | CheckerError error as e  ->
        error + "\n"
        |> log e
    | :? System.IO.IOException as e -> 
        "Could not read input file.\n"
        |> log e
    | x -> "Correct this or above construction. Pay attention to the punctuation.\n"
        |> log x


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
//YaccConstructor.exe -c BuildAST -g YardPrinter -i ../../../../Tests/Conversions/buildast_1.yrd
//YaccConstructor.exe -g YardPrinter -c "ReplaceLiterals KW_%s" -c BuildAST -i ../../../../Tests/Conversions/buildast_1.yrd
//YaccConstructor.exe -g YardPrinter -c ExpandEbnfStrict -i ../../../../Tests/Conversions/expandebnfstrict_1.yrd
//YaccConstructor.exe -g YardPrinter -c "BuildAST typed"-i ../../../../Tests/Conversions/buildast_1.yrd
//YaccConstructor.exe -g YardPrinter -c MergeAlter -i ../../../../Tests/Conversions/mergealter_1.yrd
//YaccConstructor.exe -g FsYaccPrinter -c ExpandMeta -c ExpandEbnfStrict -c ExpandBrackets -c AddEOF -i ../../../../Tests/Conversions/expandbrackets_1.yrd

