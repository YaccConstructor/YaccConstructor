//  Copyright 2012 Semen Griforev
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace Yard.Build

open System
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
//open Internal.Utilities

type YC() = 
    inherit ToolTask()

    let mutable inputFile  : string = null    
    let mutable fe   : string = null
    let mutable be   : string = null
    let mutable conversions   : string = null    
    let mutable otherFlags   = ""

    let mutable toolPath : string = null

    override this.ToolName = "YaccConstructor.exe"
    (*override this.Execute() = 
        this.Log.LogMessage("aaa!!!!!!!!", [||])
        printfn "!!!!!!!!!!!!!!!!!!!!!!!!!"
        true*)

    [<Required>]
    member this.InputFile
        with get ()  = inputFile
        and  set x = inputFile <- x
    
    (*[<Output>]
    member this.OutputFile
        with get ()  = outputFile
        and  set (x) = outputFile <- x
    *)
    
    member this.Frontend
        with get ()  = fe
        and  set x = fe <- x
    
    
    member this.Generator
        with get ()  = be
        and  set x = be <- x

    member this.Conversions
        with get ()  = conversions
        and  set x = conversions <- x

    member this.OtherFlags
        with get() = otherFlags
        and set s = otherFlags <- s

    // For targeting other versions of fslex.exe, such as "\LKG\" or "\Prototype\"
    member this.ToolPath
        with get ()  = toolPath
        and  set s = toolPath <- s
        
    override this.ToolExe = "YaccConstructor.exe"

    override this.GenerateFullPathToTool() = 
        System.IO.Path.Combine(toolPath, this.ToolExe)

    

    member this.ExecTool(pathToTool, responseFileCommands, commandLineCommands) =
        base.ExecuteTool(pathToTool, responseFileCommands, commandLineCommands)
        
    override this.GenerateCommandLineCommands() =        
        let builder = new CommandLineBuilder()
                
        builder.AppendSwitchIfNotNull("-f ", fe)
        builder.AppendSwitchIfNotNull("-g ", "\"" + be + "\"")
        let conversionsList = if conversions <> null then conversions.Replace(";"," -c ") else null
        builder.AppendSwitchIfNotNull(" ", conversionsList)

        // OtherFlags - must be second-to-last
        builder.AppendSwitchUnquotedIfNotNull("", otherFlags)

        builder.AppendSwitchIfNotNull("-i ", inputFile)
        
        let args = builder.ToString()

        this.Log.LogMessage("AAAAA!!! " + args, [||])

        // when doing simple unit tests using API, no BuildEnginer/Logger is attached
        if this.BuildEngine <> null then
            let eventArgs = { new CustomBuildEventArgs(message=args,helpKeyword="",senderName="") with member x.Equals(y) = false }
            this.BuildEngine.LogCustomEvent(eventArgs)
        args