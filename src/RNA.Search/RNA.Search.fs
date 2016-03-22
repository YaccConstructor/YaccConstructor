module YC.Bio.RNA.Search

open Argu

type CLIArguments =
    | [<Mandatory>][<AltCommandLine("-i")>] Input of string
    
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "specify a graph for processing."            

let searchTRNA graphs =
    ()

let search file =
    ()

[<EntryPoint>]
let main argv = 
    let parser = ArgumentParser.Create<CLIArguments>()
    let args = parser.Parse argv
    let inputGraphPath = 
        args.GetResult <@Input@>
        |> System.IO.Path.GetFileNameWithoutExtension
    0
