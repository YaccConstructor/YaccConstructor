﻿module YaccConstructor.API


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

let getLogMsg (e:System.Exception) msg = 
    "ERROR!\n"
        + "\nStack trace:\n " + e.StackTrace
        + "\nSource:\n " + e.Source
        + "\nInternal message:\n  " + e.Message
        + "\nMessage:\n  " + msg

let log e m =
    failwith <| getLogMsg e m

let eol = System.Environment.NewLine

let getFrontend frontendName =
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

let getIlTreeFromFile grammarFile (frontend : Frontend) userDefs = 
    try
        let defStr = String.concat ";" userDefs
        if System.String.IsNullOrEmpty defStr
        then grammarFile
        else grammarFile + "%" + defStr
        |> frontend.ParseGrammar
        |> ref
    with
    | e -> FEError (e.Message + "\nFailed parsing grammar file: " + e.StackTrace) |> raise

let getIlTreeFromStr grammarStr (frontend : Frontend) = 
    try
        ref (frontend.ParseGrammarFromStr grammarStr)
    with
    | e -> FEError (e.Message + "\nFailed parsing grammar string: " + e.StackTrace) |> raise

let processErrors (errorList : (_ * 'a list) list) msg map delimiter =
    if errorList.Length > 0 then
        eprintfn  "%s" msg
        errorList |> List.iter (fun (m, rules) ->
            rules
            |> List.map map
            |> String.concat delimiter
            |> eprintfn "Module %s:%s%s" (getModuleName m) eol
        )

let dealWithErrors ilTree = 
    let repeatedInnerRules, repeatedExportRules, undeclaredRules = GetUndeclaredNonterminalsList !ilTree
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

let applyConversion (convNameWithParams : string) (ilTree : Definition.t<Source.t, Source.t>) = 
    let parameters = convNameWithParams.Split(' ')
    if parameters.Length = 0 then failwith "Missing Conversion name"
    else
        {
            ilTree with grammar = match Seq.tryFind (fun (elem : Conversion) -> elem.Name = parameters.[0]) (Addin.GetConversions()) with 
                                  | Some conv -> conv.ConvertGrammar (ilTree.grammar, parameters.[1..parameters.Length - 1])
                                  | None -> failwith <| "Conversion not found: " + parameters.[0]
        }

let prepareGenerator generatorName =
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

let checkIlTree ilTree (gen : Generator) =  
    if not (IsSingleStartRule !ilTree) then
        raise <| CheckerError "Input grammar should contains only one start rule."
    try
        for constr in gen.Constraints do
            let grammar = ilTree.Value.grammar
            if not <| constr.Check grammar then
                eprintfn "Constraint %s: applying %s..." constr.Name constr.Conversion.Name
                ilTree := {!ilTree with grammar = constr.Fix grammar}
    with
        | e -> GenError e.Message |> raise

let genToFile ((generator : Generator), genParams, ilTree) =
    try
        match genParams with
        | None -> generator.Generate(!ilTree, true)
        | Some x -> generator.Generate(!ilTree, true, x)
    with
        | e -> GenError e.Message |> raise

let genToObj ((generator : Generator), genParams, ilTree) =
    try
        match genParams with
        | None -> generator.Generate(!ilTree, false)
        | Some x -> generator.Generate(!ilTree, false, x) 
    with
        | e -> GenError e.Message |> raise

let applyConversions (ilTree : Definition.t<Source.t, Source.t> ref) (frontend : Frontend) conversions =
    Namer.initNamer ilTree.Value.grammar
    dealWithErrors ilTree
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
    for conv in conversions do
        ilTree := applyConversion conv !ilTree
        checkSources conv !ilTree

let prepareGrammarFromString frontendName grammarStr = 
    let frontend = getFrontend frontendName
    let ilTree = getIlTreeFromStr grammarStr frontend
    (frontend, ilTree)

let prepareGrammarFromFile frontendName grammarFile userDefs = 
    let frontend = getFrontend frontendName
    let ilTree = getIlTreeFromFile grammarFile frontend userDefs
    (frontend, ilTree)

let prepareResultForGeneration (frontend, ilTree) generatorName generatorParams conversions = 
    applyConversions ilTree frontend conversions
    let generator = prepareGenerator generatorName
    checkIlTree ilTree generator
    (generator, generatorParams, ilTree)

//should be simplified with pipeline
let GenerateFromStrToFile grammarStr frontendName generatorName generatorParams conversions = 
    let preparedTuple = prepareGrammarFromString frontendName grammarStr
    let preparedResult = prepareResultForGeneration preparedTuple generatorName generatorParams conversions
    genToFile preparedResult |> ignore

let GenerateFromStrToObj grammarStr frontendName generatorName generatorParams conversions = 
    let preparedTuple = prepareGrammarFromString frontendName grammarStr
    let preparedResult = prepareResultForGeneration preparedTuple generatorName generatorParams conversions
    genToObj preparedResult


let gen grammarFile frontendName generatorName generatorParams conversions userDefs generateToFile = 
    try
        let preparedTuple = prepareGrammarFromFile frontendName grammarFile userDefs
        let preparedResult = prepareResultForGeneration preparedTuple generatorName generatorParams conversions
        if generateToFile then
            genToFile preparedResult
        else
            genToObj preparedResult
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

let generateToFile grammarFile frontendName generatorName generatorParams conversions userDefs = 
    gen grammarFile frontendName generatorName generatorParams conversions userDefs true |> ignore
    
let generate grammarFile frontendName generatorName generatorParams conversions userDefs = 
    gen grammarFile frontendName generatorName generatorParams conversions userDefs false