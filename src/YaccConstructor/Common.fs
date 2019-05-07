module YC.Common

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
