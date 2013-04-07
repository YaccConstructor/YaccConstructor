module Yard.FsYard

open Microsoft.FSharp.Text
open Yard.Core.Conversions
open Yard.Core.IL

let generate inFile replLit rnglrArgs =
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let be = new Yard.Generators.RNGLR.RNGLR()
    let applyConversion (conv:Yard.Core.Conversion) parameters (ilTree:Definition.t<Source.t,Source.t>)  = 
      {new Definition.t<Source.t,Source.t>
        with info = ilTree.info
        and  head = ilTree.head
        and  grammar = conv.ConvertGrammar (ilTree.grammar, parameters)
        and  foot = ilTree.foot
        and options = ilTree.options
      }
    let conversions =
        [
            new ExpandMeta.ExpandMeta() :> Yard.Core.Conversion ,[||]
            new ExpandEbnfStrict.ExpandEbnf() :> Yard.Core.Conversion ,[||] 
            new ReplaceLiterals.ReplaceLiterals() :> Yard.Core.Conversion ,[|replLit|]
            new ExpandInnerAlt.ExpandInnerAlt() :> Yard.Core.Conversion ,[||]
            new ExpandBrackets.ExpandBrackets() :> Yard.Core.Conversion ,[||]
            new LeaveLast.LeaveLast() :> Yard.Core.Conversion ,[||]
        ]
        |> List.map (fun (x,y) -> fun (il:Definition.t<_,_>) -> applyConversion x y il)
    fe.ParseGrammar inFile 
    |> (fun il -> List.fold (fun il conv -> conv il) il conversions)
    |>  fun il -> be.Generate(il,rnglrArgs) |> ignore

let cmdRun () =
    let userDefs = ref []
    let userDefsStr = ref ""
    let inFile = ref ""
    let replLit = ref ""
    let rnglrArgs = ref ""
    let commandLineSpecs =
        [
         "-D", ArgType.String (fun s -> userDefs := !userDefs @ [s]), "User defined constants for YardFrontend lexer."
         "-U", ArgType.String (fun s -> userDefs := List.filter ((<>) s) !userDefs), 
                "Remove previously defined constants for YardFrontend lexer."
         "-i", ArgType.String (fun s -> inFile := s), "Input grammar"
         "-module", ArgType.String (fun s -> rnglrArgs := !rnglrArgs + " -module " + s), "Target module name."
         "-token", ArgType.String (fun s -> rnglrArgs := !rnglrArgs + " -token " + s), "Token type."
         "-pos", ArgType.String (fun s -> rnglrArgs := !rnglrArgs + " -pos " + s), "Token position type."
         "-o", ArgType.String (fun s -> rnglrArgs := !rnglrArgs + " -o " + s), "Output file name."
         "-table", ArgType.String (fun s -> rnglrArgs := !rnglrArgs + " -table " + s), " Table type."
         "-fullpath", ArgType.String (fun s -> rnglrArgs := !rnglrArgs + " -fullpath " + s), "Use full path."
         "-translate" , ArgType.String (fun s -> rnglrArgs := !rnglrArgs + " -translate " + s), "Generate action code."
         "-light", ArgType.String (fun s -> rnglrArgs := !rnglrArgs + " -light " + s), "Light on/off."
         "-infEpsPath", ArgType.String (fun s -> rnglrArgs := !rnglrArgs + " -infEpsPath " + s), "Path for infinite epsilons stats."
         "-lang", ArgType.String (fun s -> rnglrArgs := !rnglrArgs + " -lang " + s), "Targrt language."
         "-replaceLiterals", ArgType.String (fun s -> replLit := s), "Replace literals regexp."
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    ArgParser.Parse commandLineSpecs
    generate !inFile !replLit !rnglrArgs

