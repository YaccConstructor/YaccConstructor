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
    let mutable light = true
    let mutable printInfiniteEpsilonPath = ""
    let mutable output = ""

    [<Required>]
    member this.InputFiles
        with get () = items
        and set v = items <- v

    member this.OutFile
        with get() = output
        and set v = output <- v

    interface ITask with
        override this.Execute() =          
            let message = sprintf "Items: %A" items
            let args = new BuildMessageEventArgs(message, "", "SetEnvironmentVariable", MessageImportance.Normal)
            engine.LogMessageEvent(args)
            System.IO.File.WriteAllText("D:/projects/YC/recursive-ascent/YaccConstructor/FsYARD/tttt", message)
            true
            
        override this.HostObject
            with get () = host
            and set v = host <- v

        override this.BuildEngine
            with get () = engine
            and set v =  engine <- v