open Microsoft.FSharp.Text

let generate inFile =
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let be = new Yard.Generators.RNGLR.RNGLR()
    fe.ParseGrammar inFile |> be.Generate |> ignore

let () =
    let userDefs = ref []
    let userDefsStr = ref ""
    let inFile = ref None
    let commandLineSpecs =
        [
         "-D", ArgType.String (fun s -> userDefs := !userDefs @ [s]), "User defined constants for YardFrontend lexer."
         "-U", ArgType.String (fun s -> userDefs := List.filter ((<>) s) !userDefs), 
                "Remove previously defined constants for YardFrontend lexer."
         "-i", ArgType.String (fun s -> inFile := s|> Some), "Input grammar"
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    ArgParser.Parse commandLineSpecs
    generate inFile

