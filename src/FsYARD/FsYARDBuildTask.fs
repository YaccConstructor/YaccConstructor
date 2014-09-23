//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

namespace YC.FsYARD

open Microsoft.Build.Framework
open System.IO
open Microsoft.FSharp.Text
open Yard.Core.Conversions
open Yard.Core.IL

[<Class>] 
type FsYard() as this =
    let mutable engine = Unchecked.defaultof<IBuildEngine>
    let mutable host = Unchecked.defaultof<ITaskHost>

    let mutable items = Array.empty<ITaskItem>

    let mutable moduleName = ""
    let mutable tokenType = ""
    let mutable fullPath = false
    let mutable positionType = "Microsoft.FSharp.Text.Lexing.Position"
    let mutable needTranslate = true
    let mutable light = "true"
    let mutable printInfiniteEpsilonPath = ""
    let mutable output = ""
    //let mutable replLiterals = ""
    let mutable projectBasePath = ""

    let generate inFile rnglrArgs =
        let fe = new Yard.Frontends.YardFrontend.YardFrontend()
        let be = new Yard.Generators.RNGLR.RNGLR()
        let applyConversion (conv:Yard.Core.Conversion) parameters (ilTree:Definition.t<Source.t,Source.t>)  = 
          { ilTree with  grammar = conv.ConvertGrammar (ilTree.grammar, parameters) }
        let conversions =
            [
                new Linearize.Linearize() :> Yard.Core.Conversion ,[||]
                new ExpandMeta.ExpandMeta() :> Yard.Core.Conversion ,[||]
                new ExpandEbnfStrict.ExpandEbnf() :> Yard.Core.Conversion ,[||] 
                //new ReplaceLiterals.ReplaceLiterals() :> Yard.Core.Conversion ,[|replLit|]
                new ExpandInnerAlt.ExpandInnerAlt() :> Yard.Core.Conversion ,[||]
                new ExpandBrackets.ExpandBrackets() :> Yard.Core.Conversion ,[||]
                new LeaveLast.LeaveLast() :> Yard.Core.Conversion ,[||]
            
            ]
            |> List.map (fun (x,y) -> fun (il:Definition.t<_,_>) -> applyConversion x y il)
        fe.ParseGrammar inFile 
        |> (fun il -> List.fold (fun il conv -> conv il) il conversions)
        |> (fun il -> be.Generate(il,rnglrArgs))
        |> ignore

    let cmdRun _ =
        let userDefs = ref []
        let userDefsStr = ref ""
        let inFile = ref ""
        //let replLit = ref ""
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
             "-light",      addRnglrArg " -light "     , "Light syntax true/false."
             "-infEpsPath", addRnglrArg " -infEpsPath ", "Path for infinite epsilons stats."
             "-lang",       addRnglrArg " -lang "      , "Target language."
             //"-replaceLiterals", ArgType.String (fun s -> replLit := s), "Replace literals regexp."
             ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
        ArgParser.Parse commandLineSpecs
        try 
            generate !inFile !rnglrArgs
            0 
        with
        | e -> 
            printfn "FATAL!!!\n %A" e
            1
        

    [<Required>]
    member this.InputFiles
        with get () = items
        and set v = items <- v

    [<Output>]
    member this.OutFile
        with get() = if output.Trim() <> "" then output else (items.[0].ToString())+".fs"
        and set v = output <- v

    member this.Light
        with get() = light
        and set v = light <- v

    member this.ModuleName
        with get() = moduleName
        and set v = moduleName <- v

    member this.NeedTranslate
        with get() = needTranslate
        and set v = needTranslate <- v

    member this.TokenType
        with get() = tokenType
        and set v = tokenType <- v

    member this.EpsilonPath
        with get() = printInfiniteEpsilonPath
        and set v = printInfiniteEpsilonPath <- v

    (*member this.ReplLiterals
        with get() = replLiterals
        and set v = replLiterals <- v*)

    member this.FullPath
        with get() = fullPath
        and set v = fullPath <- v

     [<Required>]
     member this.ProjectBasePath
        with get() = projectBasePath
        and set v = projectBasePath <- v

     //[<EntryPoint>]
    member this.CmdRun x = cmdRun x

    interface ITask with
        override this.Execute() =
            use sw = new StreamWriter(Path.Combine(projectBasePath,"FsYard.log"))
            use mem = new MemoryStream()
            use esw = new StreamWriter(mem)
            System.Console.SetOut(sw)
            System.Console.SetError(sw)
            let rnglrArgs = 
                sprintf "-translate  %A " needTranslate
                + if tokenType.Trim() <> "" then  sprintf "-token %s " tokenType else ""
                + if moduleName.Trim() <> "" then sprintf "-module %s " moduleName else ""
                + if printInfiniteEpsilonPath.Trim() <> "" then sprintf "-infEpsPath %s " printInfiniteEpsilonPath else ""
                + if light.Trim() <> "" then sprintf "-light %s " light else ""
                + if output.Trim() <> "" then sprintf "-o %s " output else ""
                + sprintf "-fullpath %A" fullPath            
            let eventArgs = { new CustomBuildEventArgs(message= "FsYard " + rnglrArgs + " -i " + (items.[0].ToString()) ,helpKeyword="",senderName="") with member x.Equals(y) = false }
            engine.LogCustomEvent(eventArgs)
            generate (items.[0].ToString()) rnglrArgs
            mem.Flush()
            use sr = new StreamReader(mem)
            let errors = sr.ReadToEnd().Split('\n')
            //errors
            //|> fun s ->
              //  let eventArgs = { new BuildWarningEventArgs(message= s ,helpKeyword="",senderName="") with member x.Equals(y) = false }
                //engine.LogWarningEvent(eventArgs)
            mem.Close()
            esw.Close()
            sw.Close()
            true
        //member statis cmdRun
        override this.HostObject
            with get () = host
            and set v = host <- v

        override this.BuildEngine
            with get () = engine
            and set v =  engine <- v

    module X =
        [<EntryPoint>]    
        let f x =
            FsYard().CmdRun x