module YaccConstructor.Arguments

open Mono.Addins
open Argu
open Yard.Core
open Yard.Core.IL
open Yard.Core.Helpers
open Yard.Core.Checkers
open Microsoft.FSharp.Text

let addinFrontends = AddinManager.GetExtensionObjects (typeof<Frontend>) |> Seq.cast<Frontend> |> Seq.toArray
let addinConversions = AddinManager.GetExtensionObjects (typeof<Conversion>) |> Seq.cast<Conversion> |> Seq.toArray
let addinGenerators = AddinManager.GetExtensionObjects (typeof<Generator>) |> Seq.cast<Generator> |> Seq.toArray
let addinFrontendNames = addinFrontends |> Seq.fold (fun buf (elem : Frontend) -> buf + elem.Name + "\n") ""
let addinConversionNames = addinConversions |> Seq.fold (fun buf (elem : Conversion) -> buf + elem.Name + "\n") ""
let addinGeneratorNames = addinGenerators |> Seq.fold (fun buf (elem : Generator) -> buf + elem.Name + "\n") ""

type CLIArguments =
    | [<Mandatory>][<AltCommandLine("-f")>] Frontend of string //"Frontend name. Use -af to list available."
    //| [<Mandatory>][<AltCommandLine("-af")>]"-af", ArgType.Unit (printItems "frontends" addinFrontendNames !feName), "Available frontends"
    | [<Mandatory>][<AltCommandLine("-g")>] Generator of string
     (*(fun s -> 
        match Array.toList (s.Split ' ') with
        | name::[] -> generatorName := Some name; generatorParams := None
        | name::parameters -> generatorName := Some name; generatorParams := Some (String.concat " " parameters)
        | _ -> failwith "You need to specify generator name"
    ), "Generator name. Use -ag to list available."*)
    //| [<Mandatory>][<AltCommandLine("-ag")>], ArgType.Unit (printItems "generators" addinGeneratorNames !generatorName), "Available generators"
    | [<Mandatory>][<AltCommandLine("-c")>] Conversion of string //(fun s -> conversions.Add s), "Conversion applied in order. Use -ac to list available."
    //| [<Mandatory>][<AltCommandLine("-ac")>]"-ac", ArgType.Unit (printItems "conversions" addinConversionNames None), "Available conversions"
    | [<Mandatory>][<AltCommandLine("-D")>] UserDefs of string //(fun s -> userDefs := !userDefs @ [s]), "User defined constants for YardFrontend lexer."
    | [<Mandatory>][<AltCommandLine("-U")>] NewUserDefs of string //(fun s -> userDefs := List.filter ((<>) s) !userDefs), 
        //"Remove previously defined constants for YardFrontend lexer."
    | [<Mandatory>][<AltCommandLine("-i")>] Input of string (* (fun s ->
                            testFile := System.IO.Path.GetFileName s |> Some
                            testsPath := System.IO.Path.GetDirectoryName s |> Some), "Input grammar"      *)
with
    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Frontend _ ->    "Frontend name. Avaliable frontends:" + addinFrontendNames
            | Generator _ ->   "Generator name. Avaliable generators:" + addinGeneratorNames
            | Conversion _ ->  "Conversion name. Avaliable conversions:" + addinConversionNames
            | UserDefs _ ->    "User defined constants for YardFrontend lexer."
            | NewUserDefs _ -> "Remove previously defined constants for YardFrontend lexer."
            | Input _ ->       "Input grammar."