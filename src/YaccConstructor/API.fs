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

//parse grammar from file
let getIlTreeFromFile grammarFile userDefs (frontend : Frontend) =
    try
        let defStr = String.concat ";" userDefs
        if System.String.IsNullOrEmpty defStr
        then grammarFile
        else grammarFile + "%" + defStr
        |> frontend.ParseGrammar |> ref
    with
    | e -> FEError (e.Message + " " + e.StackTrace) |> raise

//parse grammar from string
let getIlTreeFromStr grammar (frontend : Frontend) = 
    grammar |> frontend.ParseGrammarString |> ref

// Let possible to know, after what conversion we lost reference to original code
let checkSources name il isSourcesLost= 
    if not !isSourcesLost then
        match sourcesWithoutFileNames il with
        | [] -> ()
        | x ->
            isSourcesLost := true
            x
            |> List.map(fun s -> s.text) |> String.concat "\n"
            |> printfn "Lost sources after frontend or conversion %s:\n %s" name

let run ilTree generatorName conversions frontendName =
    let repeatedInnerRules, repeatedExportRules, undeclaredRules = GetUndeclaredNonterminalsList !ilTree
    Namer.initNamer ilTree.Value.grammar
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
        (fun rule -> sprintf "%s (%s: %d: %d)" rule.text rule.file rule.startPos.line rule.startPos.column)
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
        "Some meta-rules have incorrect arguments number: "
        (fun (rule, got, expected) -> sprintf "%s(%d,%d): %d (expected %d)" rule.text rule.startPos.line rule.startPos.column got expected)
        eol

    let lostSources = false |> ref

    checkSources frontendName !ilTree lostSources

    // Apply Conversions
    let apply_Conversion (convNameWithParams:string) (ilTree:Definition.t<Source.t,Source.t>) = 
        let parameters = convNameWithParams.Split(' ')
            //printfn "Conversion: %s" convNameWithParams
        if parameters.Length = 0 then failwith "Missing Conversion name"
        else
            {ilTree with grammar = match Seq.tryFind (fun (elem : Conversion) -> elem.Name = parameters.[0]) (Addin.GetConversions()) with 
                                   | Some conv -> conv.ConvertGrammar (ilTree.grammar, parameters.[1..parameters.Length - 1])
                                   | None -> failwith <| "Conversion not found: " + parameters.[0]
            }

    for conv in conversions do
        ilTree := apply_Conversion conv !ilTree
        checkSources conv !ilTree lostSources

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
    gen

let generateResult ilTree (generator : Generator) generatorParams genToFile =
    match generatorParams with
    | None -> generator.Generate(!ilTree, genToFile)
    | Some genParams -> generator.Generate(!ilTree, genToFile, genParams)

let applyConstraints (generator : Generator) ilTree = 
    if not (IsSingleStartRule !ilTree) then
        raise <| CheckerError "Input grammar should contains only one start rule."
    try
        //let gen = new Yard.Generators.RNGLR.RNGLR()
        for constr in generator.Constraints do
            let grammar = ilTree.Value.grammar
            if not <| constr.Check grammar then
                eprintfn "Constraint %s: applying %s..." constr.Name constr.Conversion.Name
                ilTree := {!ilTree with grammar = constr.Fix grammar}
        ilTree
    with
        | e -> GenError e.Message |> raise

let resultToFile generator ilTree generatorParams = 
    generateResult (applyConstraints generator ilTree) generator generatorParams true

let resultToObj ilTree generator generatorParams =
    generateResult (applyConstraints generator ilTree) generator generatorParams false

let gen grammarFile frontendName generatorName generatorParams conversions userDefs = 
    try
        let frontend = getFrontend frontendName
        let ilTree = getIlTreeFromFile grammarFile userDefs frontend
        let generator = run ilTree generatorName conversions frontendName
        resultToFile generator ilTree generatorParams
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

(*
let genStr grammar frontendName generatorName generatorParams conversions userDefs generateToFile =
    try
        run grammar true frontendName generatorName generatorParams conversions userDefs generateToFile
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
    | x -> "Correct this or above construction. Pay attention to the punctuation.\n"
        |> log x
        *)
let generateToFile grammarFile frontendName generatorName generatorParams conversions userDefs = 
        gen grammarFile frontendName generatorName generatorParams conversions userDefs 
        |> ignore
        (*
let generateToFileFromString grammarFile frontendName generatorName generatorParams 
        conversions userDefs =
            genStr grammarFile frontendName generatorName generatorParams conversions userDefs true 
            |> ignore
    *)
let generate grammarFile frontendName generatorName generatorParams conversions userDefs = 
    gen grammarFile frontendName generatorName generatorParams conversions userDefs

(*let generateFromString grammar frontendName generatorName generatorParams conversions userDefs = 
    genStr grammar frontendName generatorName generatorParams conversions userDefs false
    *)