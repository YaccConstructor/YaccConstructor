module Yard.FsYard

open Microsoft.FSharp.Text
open Yard.Core.Conversions
open Yard.Core.IL

let generate inFile replLit rnglrArgs =
    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
    let be = new Yard.Generators.RNGLR.RNGLR()
    let applyConversion (conv:Yard.Core.Conversion) parameters (ilTree:Definition.t<Source.t,Source.t>)  = 
      { ilTree with  grammar = conv.ConvertGrammar (ilTree.grammar, parameters) }
    let conversions =
        [
            new ExpandMeta.ExpandMeta() :> Yard.Core.Conversion ,[||]
            new ExpandEbnfStrict.ExpandEbnf() :> Yard.Core.Conversion ,[||] 
            new ReplaceLiterals.ReplaceLiterals() :> Yard.Core.Conversion ,[|replLit|]
            new ExpandInnerAlt.ExpandInnerAlt() :> Yard.Core.Conversion ,[||]
            new ExpandBrackets.ExpandBrackets() :> Yard.Core.Conversion ,[||]
            new LeaveLast.LeaveLast() :> Yard.Core.Conversion ,[||]
            new Linearize.Linearize() :> Yard.Core.Conversion ,[||]
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
    let addRnglrArg opt =
        ArgType.String (fun value -> rnglrArgs := !rnglrArgs + opt + value)
    let commandLineSpecs =
        [
         "-D", ArgType.String (fun s -> userDefs := !userDefs @ [s]), "User defined constants for YardFrontend lexer."
         "-U", ArgType.String (fun s -> userDefs := List.filter ((<>) s) !userDefs), 
                "Remove previously defined constants for YardFrontend lexer."
         "-i", ArgType.String (fun s -> inFile := s), "Input grammar"
         "-module",     addRnglrArg " -module "    , "Target module name."
         "-token",      addRnglrArg " -token "     , "Token type."
         "-pos",        addRnglrArg " -pos "       , "Token position type."
         "-o",          addRnglrArg " -o "         , "Output file name."
         "-table",      addRnglrArg " -table "     , " Table type."
         "-fullpath",   addRnglrArg " -fullpath "  , "Use full path."
         "-translate",  addRnglrArg " -translate " , "Generate action code."
         "-light",      addRnglrArg " -light "     , "Light on/off."
         "-infEpsPath", addRnglrArg " -infEpsPath ", "Path for infinite epsilons stats."
         "-lang",       addRnglrArg " -lang "      , "Target language."
         "-replaceLiterals", ArgType.String (fun s -> replLit := s), "Replace literals regexp."
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    ArgParser.Parse commandLineSpecs
    generate !inFile !replLit !rnglrArgs

