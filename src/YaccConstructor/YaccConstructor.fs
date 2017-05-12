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


open Yard.Core
open Yard.Core.IL
open Yard.Core.Helpers
open Yard.Core.Checkers
open Microsoft.FSharp.Text
open System.IO
open System.Reflection
open YaccConstructor.Common
open YaccConstructor.API
open Argu
(*
let () =
    let feName = ref None
    let generatorName = ref None
    let generatorParams = ref None
    let testsPath = ref <| Some ""
    let testFile : string option ref= ref None
    let conversions = new ResizeArray<string>()
    
    let userDefs = ref []
    let userDefsStr = ref ""

    feName := // Fill by default value
        if Array.exists (fun (elem : Frontend) -> elem.Name = "YardFrontend") (Addin.GetFrontends())
        then Some "YardFrontend"
        elif not <| Array.isEmpty (Addin.GetFrontends())
        then 
            let tmpName = (Addin.GetFrontends()).[0].Name
            Some tmpName
        else None
            
    generatorName :=
        if Array.exists (fun (elem : Generator) -> elem.Name = "RNGLRGenerator") (Addin.GetGenerators())
        then Some "RNGLRGenerator"
        elif not <| Array.isEmpty (Addin.GetGenerators())
        then 
            let tmpName = (Addin.GetGenerators()).[0].Name
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
        | AvailableFrontends -> printItems "frontends" (Addin.GetFrontendNames()) !feName
        | Generator g -> match Array.toList (g.Split ' ') with
                         | name::[] -> 
                            generatorName := Some name
                            generatorParams := None
                         | name::parameters -> 
                            generatorName := Some name
                            generatorParams := Some (String.concat " " parameters)
                         | _ -> failwith "You need to specify generator name"
        | AvailableGenerators -> printItems "generators" (Addin.GetGeneratorNames()) !generatorName
        | Conversion c -> conversions.Add c
        | AvailableConversions -> printItems "frontends" (Addin.GetFrontendNames()) !feName
        | DefConstant d -> userDefs := !userDefs@[d]
        | UndefConstant u -> userDefs := List.filter((<>) u) !userDefs
        | Input i -> 
            testFile := System.IO.Path.GetFileName i |> Some
            testsPath := System.IO.Path.GetDirectoryName i |> Some
    
    match !testFile, !feName, !generatorName with
        | _, None, _  -> EmptyArg "frontend name (-f)" |> raise
        | _, _, None  -> EmptyArg "generator name (-g)" |> raise
        | None , _, _ -> EmptyArg "file name (-i)" |> raise
        | _           -> ()

    if !generateSomething = true
    then
        try
            generateToFile (System.IO.Path.Combine(testsPath.Value.Value, testFile.Value.Value))
                           (feName.Value.Value)
                           (generatorName.Value.Value)
                           (generatorParams.Value)
                           conversions
                           userDefs.Value
        with 
            | _ -> ()
            *)
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

