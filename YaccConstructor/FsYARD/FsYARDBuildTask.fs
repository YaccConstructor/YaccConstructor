module Yard.Build

open Microsoft.Build.Framework
open System.IO

[<Class>] 
type FsYard() =
    let mutable engine = Unchecked.defaultof<IBuildEngine>
    let mutable host = Unchecked.defaultof<ITaskHost>

    let mutable items = Array.empty<ITaskItem>

    let mutable moduleName = ""
    let mutable tokenType = ""
    let mutable fullPath = false
    let mutable positionType = "Microsoft.FSharp.Text.Lexing.Position"
    let mutable needTranslate = true
    let mutable light = "on"
    let mutable printInfiniteEpsilonPath = ""
    let mutable output = ""
    let mutable replLiterals = ""
    let mutable projectBasePath = ""

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

    member this.ReplLiterals
        with get() = replLiterals
        and set v = replLiterals <- v

    member this.FullPath
        with get() = fullPath
        and set v = fullPath <- v

     [<Required>]
     member this.ProjectBasePath
        with get() = projectBasePath
        and set v = projectBasePath <- v

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
            let eventArgs = { new CustomBuildEventArgs(message= "FsYard " + rnglrArgs + " -c ReplaceLiterals " + replLiterals + " -i " + (items.[0].ToString()) ,helpKeyword="",senderName="") with member x.Equals(y) = false }
            engine.LogCustomEvent(eventArgs)
            Yard.FsYard.generate (items.[0].ToString()) replLiterals rnglrArgs
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
            
        override this.HostObject
            with get () = host
            and set v = host <- v

        override this.BuildEngine
            with get () = engine
            and set v =  engine <- v

do FsYard.cmdRun()