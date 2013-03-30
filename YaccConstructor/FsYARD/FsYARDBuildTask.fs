module Yard.Build

open Microsoft.Build.Framework

[<Class>] 
type SetEnv() =
    let mutable engine = Unchecked.defaultof<IBuildEngine>
    let mutable host = Unchecked.defaultof<ITaskHost>
    let mutable items =  Array.empty<ITaskItem>

    [<Required>]
    member this.InputFiles
        with get () = items
        and set v = items <- v

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