module YaccConstructor.API

open Mono.Addins
open Yard.Core
open Yard.Core.IL
open Yard.Core.Helpers
open Yard.Core.Checkers
open Microsoft.FSharp.Text
open System.IO
open System.Reflection
open YaccConstructor.Common

exception InvalidFEName of string
exception InvalidGenName of string
exception EmptyArg of string
exception FEError of string
exception GenError of string
exception CheckerError of string

let log (e:System.Exception) msg =
    printfn "ERROR!"
    "\nStack trace:\n " + e.StackTrace
    |> printfn "%s"
    "\nInternal message:\n  " + e.Message
    |> printfn "%s"
    "\nMessage:\n  " + msg
    |> printfn "%s"

let logf e m = 
    log e m
    failwith ""

let eol = System.Environment.NewLine


let run grammarFile frontendName generatorName generatorParams conversions userDefs generateToFile =
    let frontend =
        let fen = (Addin.GetFrontends())
        let _raise () = InvalidFEName frontendName |> raise
        if Array.exists (fun (elem : Frontend) -> elem.Name = frontendName) (Addin.GetFrontends())
        then
            try
                match Array.tryFind (fun (elem : Frontend) -> elem.Name = frontendName) (Addin.GetFrontends()) with
                | Some fe -> fe
                | None -> failwith "Frontend is not found."
            with
            | _ -> _raise ()
        else _raise ()

    // Parse grammar
    let ilTree =
        //try
            let defStr = String.concat ";" userDefs
            if System.String.IsNullOrEmpty defStr
            then grammarFile
            else grammarFile + "%" + defStr
            |> frontend.ParseGrammar
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
    checkSources frontend.Name !ilTree
    // Apply Conversions

    let apply_Conversion (convNameWithParams:string) (ilTree:Definition.t<Source.t,Source.t>) = 
        let parameters = convNameWithParams.Split(' ')
            //printfn "Conversion: %s" convNameWithParams
        if parameters.Length = 0 then failwith "Missing Conversion name"
        else
            {ilTree
                with grammar =
                    match Seq.tryFind (fun (elem : Conversion) -> elem.Name = parameters.[0]) (Addin.GetConversions()) with 
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
        if Array.exists (fun (elem : Generator) -> elem.Name = generatorName) (Addin.GetGenerators())
        then              
            try
                match Array.tryFind (fun (elem : Generator) -> elem.Name = generatorName) (Addin.GetGenerators()) with
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

            match generatorParams with
            | None -> 
                //printfn "%A" <| !ilTree
                gen.Generate(!ilTree, generateToFile)
            | Some genParams -> gen.Generate(!ilTree, generateToFile, genParams)
        //with
//                | Yard.Generators.GNESCCGenerator.StartRuleNotFound 
//                    -> GenError "Start rule cannot be found in input grammar. Please, specify start rule."
//                       |> raise
        //| e -> GenError e.Message |> raise

    result

let gen grammarFile frontendName generatorName generatorParams conversions userDefs generateToFile = 
    run grammarFile frontendName generatorName generatorParams conversions userDefs generateToFile

let generateToFile grammarFile frontendName generatorName generatorParams conversions userDefs = 
    try
        gen grammarFile frontendName generatorName generatorParams conversions userDefs true |> ignore
    with
    | InvalidFEName frontendName as e  -> 
        "Frontend with name " + frontendName + " is not available. Run \"Main.exe -af\" for get all available frontends.\n" 
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
    

let generate grammarFile frontendName generatorName generatorParams conversions userDefs = 
//    try
    gen grammarFile frontendName generatorName generatorParams conversions userDefs false
//    with
//    | InvalidFEName frontendName as e  -> 
//        "Frontend with name " + frontendName + " is not available. Run \"Main.exe -af\" for get all available frontends.\n" 
//        |> logf e
//    | InvalidGenName genName as e->
//        "Generator with name " + genName + " is not available. Run \"Main.exe -ag\" for get all available generators.\n"
//        |> logf e
//    | EmptyArg argName as e ->
//        sprintf "Argument can not be empty: %s\n\nYou need to specify frontend, generator and input grammar. Example:
//YaccConstructor.exe -f YardFrontend -c BuildAST -g YardPrinter -i ../../../Tests/Conversions/buildast_1.yrd \n
//List of available frontends, generators and conversions can be obtained by -af -ag -ac keys" argName
//        |> logf e
//    | FEError error as e ->
//        "Frontend error: " + error + "\n"
//        |> logf e
//    | GenError error as e  ->
//        "Generator error: " + error + "\n"
//        |> logf e
//    | CheckerError error as e  ->
//        error + "\n"
//        |> logf e
//    | :? System.IO.IOException as e -> 
//        "Could not read input file.\n"
//        |> logf e
//    | x -> "Correct this or above construction. Pay attention to the punctuation.\n"
//        |> logf x
    