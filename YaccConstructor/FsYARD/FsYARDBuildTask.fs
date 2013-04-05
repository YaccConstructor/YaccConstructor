module Yard.Build

open Microsoft.Build.Framework

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

    [<Required>]
    member this.InputFiles
        with get () = items
        and set v = items <- v

    member this.OutFile
        with get() = output
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

    interface ITask with
        override this.Execute() =          
            let rnglrArgs = 
                if needTranslate then sprintf "-translate  %A " needTranslate else ""
                + if tokenType.Trim() <> "" then  sprintf "-token %s " tokenType else ""
                + if moduleName.Trim() <> "" then sprintf "-module %s " moduleName else ""
                + if printInfiniteEpsilonPath.Trim() <> "" then sprintf "-infEpsPath %s " printInfiniteEpsilonPath else ""
                + if light.Trim() <> "" then sprintf "-light %s " light else ""
                + if output.Trim() <> "" then sprintf "-o %s " output else ""
                + if fullPath then sprintf "-fullpath %A"  fullPath else ""
            Yard.FsYard.generate (items.[0].ToString()) replLiterals rnglrArgs
            //let args = new BuildMessageEventArgs(message, "", "SetEnvironmentVariable", MessageImportance.Normal)
            //engine.LogMessageEvent(args)
            //System.IO.File.WriteAllText("D:/projects/YC/recursive-ascent/YaccConstructor/FsYARD/tttt", message)
            true
            
        override this.HostObject
            with get () = host
            and set v = host <- v

        override this.BuildEngine
            with get () = engine
            and set v =  engine <- v