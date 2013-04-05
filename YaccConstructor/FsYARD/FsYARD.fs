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
        |> List.map (fun (x,y) -> fun (il:Definition.t<_,_>) -> System.IO.File.WriteAllText("D:/projects/yc/recursive-ascent/YaccConstructor/FsYARD/сссс"+x.Name,il.grammar.ToString()+y.ToString());applyConversion x y il)
    fe.ParseGrammar inFile 
    |> (fun il -> List.fold (fun il conv -> conv il) il conversions)
    |>  fun il -> System.IO.File.WriteAllText("D:/projects/yc/recursive-ascent/YaccConstructor/FsYARD/сссс",il.grammar.ToString() + "\n" + rnglrArgs); be.Generate(il,rnglrArgs) |> ignore

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
    generate inFile "" ""

