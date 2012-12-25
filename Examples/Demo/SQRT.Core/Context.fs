module SQRT.Core.Context

open System.Collections.Generic

[<Class>]
type DataContext() =
    let astPerFile = new Dictionary<string,_>()
    let mutable fullAST = 1
    member x.FullAST with get() = fullAST
                     and  set v = fullAST <- v
    member x.AstPerFile with get() = astPerFile

